/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptWidgetSet.p
 > Purpose:         Loading widget sets
 > Author:          Tom Khabaza, Aug 14 1990 (see revisions)
 > Documentation:   xpt_widgetset
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING      ;;; redundant for POPC

section $-typespec_utils =>
            XptWidgetSet, XptNewWidgetSet, XptLoadClass, XptLoadClassProc,
            XptGetWidgetDependencies;

global constant procedure (
    XptWidgetSet
    XptNewWidgetSet
    XptLoadClass
    XptLoadClassProc
    XptGetWidgetDependencies
);

#_IF isproperty(XptWidgetSet)

    warning(0, 'LIB *XptWidgetSet ALREADY A LOADED');
    endsection;
    #_TERMIN_IF true;

#_ENDIF

include xdefs;
include xpt_constants;
uses exload;
uses widgetclass_key;
uses xpt_generaltypes;

/**************************************************************************
 * GLOBAL WIDGETSET PROPERTY
 **************************************************************************/

define lconstant demandload_widgetset(name, wsprop);
    lvars name libname key val wsprop;
    dlocal exload_isbatching = true; ;;; turn on exload batching

    name sys_>< 'WidgetSet.p' -> libname;
    if syslibcompile(libname, popuseslist) then
        ;;; If we found a library file, check whether it's made
        ;;; any difference.  (Can't just use direct access through
        ;;; XptWidgetSet since this might cause infinite regress.)
        for key,val in_property wsprop do
            returnif(key == name)(val);
        endfor;
    endif;

    ;;; If we get this far, we failed to load a widget-set into
    ;;; XptWidgetSet, so make a new one with defaults.
    XptNewWidgetSet(name);
enddefine;

define global XptWidgetSet =
    newanyproperty([], 10, 1, 7, false, false, "perm", false,
            demandload_widgetset);
enddefine;

/**************************************************************************
 * MAKING AND ADDING NEW WIDGETSET PROPERTIES
 **************************************************************************/

define lconstant demandload_class(wcname, ws);
    lvars key, val, wsname, wcname, libname, ws, wsprefix, wcidname;

    define lconstant libcompile(filename);
        lvars filename;
        if isword(filename) then
            useslib(filename)
        elseunless trycompile(filename) then
            syslibcompile(filename, popuseslist) ->;
        endif;
    enddefine;

    dlocal exload_isbatching = true; ;;; turn on exload batching
    ws("WidgetSetName") -> wsname;
    ;;; CHECK CLASS NAME
    if issubstring('WidgetSet', wcname) then
        mishap(wcname,wsname, 2, 'Unknown special key');
    elseif (ws("WidgetSetMembers") ->> val) /== [] then
        unless fast_lmember(wcname, val) then
            mishap(wcname, wsname, 2, 'Unknown class name');
        endunless;
    endif;
    ws("WidgetSetPrefix") -> wsprefix;
    ;;; TRY LOAD LIBRARIES
    if (ws("WidgetSetFileMapping") ->> val) /== identfn then
        val(wsprefix, wcname) -> libname;
        if libname then
            ;;; load any libraries
            if libname.islist then applist(libname, libcompile)
            else libcompile(libname);
            endif;
            ;;; if we loaded libraries, see if they loaded the class.
            ;;; Need to use a for loop so we don't trigger demand loading
            for key,val in_property ws do
                returnif(key == wcname)(val);
            endfor;
        endif;
    endif;

    ;;; check for new widgetclass identifiers
    if wsname == "Toolkit" then 'Xt' -> wsprefix endif;
    wsprefix sys_>< wcname -> wcidname;
    uppertolower(wcidname(1)) -> wcidname(1);
    consword(wcidname) -> wcidname;
    returnif(isdefined(wcidname)) (valof(wcidname) ->> ws(wcname));

    ;;; didn't find a library which loaded the class - load it ourselves:
    XptLoadClass(wsname, wcname);
    for key,val in_property ws do
        returnif(key == wcname)(val);
    endfor;
    mishap(wcname,1,'CANNOT FIND CLASS');
enddefine;

define global XptNewWidgetSet(wsname) -> widgetset;
    lvars wsname widgetset procs specs;

    define lconstant default_name_mapping(wcname);
        lvars wcname;
        uppertolower(wcname(1)), explode(allbutfirst(1,wcname)),
        explode('Class'), consword(datalength(wcname)+5);
    enddefine;
    define lconstant default_file_mapping(wsprefix, wcname);
        lvars wsprefix, wcname;
        wsprefix sys_>< wcname sys_>< 'Class.p';
    enddefine;
    newanyproperty(
        [   [WidgetSetName ^wsname]
            [WidgetSetPrefix ^wsname]
            [WidgetSetLibList ^XLINK_EXLIBS]
            [WidgetSetNameMapping ^default_name_mapping]
            [WidgetSetFileMapping ^default_file_mapping]
            [WidgetSetMembers []]],
        20, 1, 15, false, false, "perm", false, demandload_class)
            ->> widgetset -> XptWidgetSet(wsname);
enddefine;

/**************************************************************************
 * GENERATING SYMBOLS FOR EXTERNAL LOAD
 **************************************************************************/

;;; MAKING SYMBOL TABLE FOR WIDGET CLASSES

define lconstant make_class_data(wsproperty, wcname) -> name -> id -> acc_p;
    lvars wsproperty, wcname, name, acc_p, wc;
    lconstant id = false;

    ;;; build a new widgetclass object with null external pointer
    class_cons(widgetclass_key)(
        conspair(XDT_WIDGETCLASS, wcname),
        null_external_ptr,
        false,
        wsproperty("WidgetSetName")
    ) ->> wc -> wsproperty(wcname);

    ;;; construct a (no result) procedure to initialize widgetclass object
    XptPopImportWidgetClassPtr(%wc%) -> acc_p;

    ;;; build symbol data
    wsproperty("WidgetSetNameMapping")(wcname) -> name;
enddefine;

;;; MAKING SYMBOL TABLE FOR CLASS PROCEDURES

define lconstant make_proc_data(wsproperty, procspec) -> (acc_p, id, name);
    lvars   wsproperty, procspec, n, name, id, ret_spec, conv_p, nargs, acc_p,
            call_p;
    lconstant call_spec = writeable initv(2);

    unless procspec.isvector and (datalength(procspec)->>n) > 0 and n<= 4 then
        mishap(procspec,1,'vector of length 1-4 expected');
    elseunless (procspec(1) ->> name).isword then
        mishap(name,1,'WORD NEEDED');
    endunless;

    returnunless(exload_isundef(name)); ;;; don't reload
    ;;; declare identifier as global vars procedure, top level section.
    dlocal current_section = pop_section;
    pop11_define_declare(name, sysGLOBAL, sysCONSTANT, "procedure");
    identof(name) -> id;

    ;;; Deal with optional bits of spec
    if n > 1 then subscrv(2,procspec) else 0     endif -> nargs;
    if n > 2 then subscrv(3,procspec) else false endif -> ret_spec;
    if n > 3 then subscrv(4,procspec) else false endif -> conv_p;

    nargs -> call_spec(1);
    if conv_p then conv_p(%ret_spec,true%)
    else ret_spec endif -> call_spec(2);

    lconstant call_p_prop = newanyproperty([], 4, false, false, syshash,
                                            nonop =, "tmpval", false, false);

    unless call_p_prop(call_spec) ->> call_p then
        cons_access(true, call_spec, true, 1) ->> call_p
                    -> call_p_prop(copy(call_spec))
    endunless;

    XptPopImportProcedure(%name, call_p, nargs%) -> acc_p;
enddefine;

;;; no longer exported - people should use XptLoadClass/XptLoadClassProc
define lconstant make_symbol_data(wsname, spec, type);
    lvars wsname spec type ws, name, id, acc_p;
    ;;; Get widget-set
    XptWidgetSet(wsname) -> ws;

    if type == "class" then
        ;;; Class spec must be name only
        unless spec.isword then
            mishap('Word expected for class specification', [^spec]);
        endunless;
        ;;; make a new widget class
        make_class_data(ws, spec) -> name -> id -> acc_p;
    elseif type == "proc" then
        make_proc_data(ws, spec) -> name -> id -> acc_p;
    else
        mishap('"class" or "proc" expected', [^type]);
    endif;
    ;;; return vector suitable for exload_addbatch:
    {%name, 'C', undef, undef, {% undef, undef, undef, id, acc_p %} %};
enddefine;

/**************************************************************************
 * LOADING CLASSES AND CLASS PROCEDURES
 **************************************************************************/

define lconstant loadsymbols(name, syms);
    lvars name syms;
    exload_addbatch(gensym(name),XptWidgetSet(name)("WidgetSetLibList"),syms);
    unless exload_isbatching then
        exload_do_batch_load();
    endunless;
enddefine;

define global XptLoadClass(wsname, wcname);
    lvars wsname wcname w symlist;

    unless wcname.islist then conspair(wcname,[]) -> wcname endunless;

    [%  for w in wcname do
            make_symbol_data(wsname, w, "class");
    endfor %] -> symlist;
    loadsymbols(wsname, symlist);
enddefine;


define global XptLoadClassProc(wsname, procspec);
    lvars wsname p procspec symlist;

    unless procspec.islist then conspair(procspec,[])-> procspec; endunless;

    [%  for p in procspec do
        make_symbol_data(wsname, p, "proc");
    endfor %] -> symlist;

    loadsymbols(wsname, symlist);
enddefine;

/*************************************************************************
 * GENERAL WIDGET DEPENDENCY TREES
 *************************************************************************/

;;; The procedures defined in this file are abstracted out from the library
;;; files LIB *AthenaWidgetSet *OpenLookWidgetSet, and *MotifWidgetSet where
;;; they were repeated in each file.  Each of the above mentioned files now
;;; simple load this file.

;;; _XptGetWidgetDependencies_ has been generalised to cope with Athena
;;; Objects and Motif Gadgets in a single procedure.

;;; These procedures were originally written by Jon Meyer.

lvars procspecs = [], libraries = [], dependency_tree, library_prefix,
        include_files = [];

define lconstant require(name);
    lvars name dependencies procs info;
    lconstant
            pair = writeable conspair(undef,[])
        ;
    ;;; given a word, it looks in the current dependency_tree for any
    ;;; dependencies keyed on that word. If dependencies are found, it
    ;;; loads them. Depencencies are specified as a list of related
    ;;; dependencies or library files and a list of procedures.
    ;;; related dependencies are recursives "required". library files
    ;;; and procedures are added to a list which is loaded at a later stage
    returnunless(dependency_tree(name)->> info);
    false -> dependency_tree(name);
    dest(info) -> procs -> dependencies;
    if dependencies then
        unless dependencies.islist then
            dependencies -> front(pair); pair -> dependencies;
        endunless;
        for name in dependencies do
            if name.isword then
                require(name);
            elseif sys_fname_extn(name) = '.ph' then
                name :: include_files -> include_files
            elseif iscaller(demandload_class) then
                ;;; make it use useslib
                consword(library_prefix sys_>< name) :: libraries -> libraries
            else
                library_prefix dir_>< name :: libraries -> libraries
            endif;
        endfor;
    endif;
    if procs.hd then
        if isinheap(procs) then
            procs nc_<> procspecs -> procspecs
        else
            procs <> procspecs -> procspecs
        endif;
    endif;
enddefine;

define global XptGetWidgetDependencies(wsprefix, wcname, tree, wsname);
    lvars wsprefix, wcname, tree, wsname, index;
    tree -> dependency_tree;
    wsprefix -> library_prefix;
    ;;; ensure library_prefix is a proper directory name
    unless isendstring('/', library_prefix) then
        library_prefix sys_>< '/' -> library_prefix;
    endunless;
    ;;; Strip the "Widget" from wcname. Convert it into a word
    if (issubstring('Widget', wcname) ->> index) or
        (issubstring('Gadget', wcname) ->> index) or
        (issubstring('Object', wcname) ->> index) then
        consword(substring(1,index -1, wcname)) -> wcname;
    endif;
    require(wcname);

    ;;; loadinclude any include files
    applist((include_files, [] -> include_files),
        procedure();
            dlocal proglist_state = proglist_new_state(() :: []);
            nonmac loadinclude();
            pop11_compile(proglist);
        endprocedure);

    ;;; load any related procedures
    XptLoadClassProc(wsname, (procspecs, [] -> procspecs));
    ;;; return any related libraries
    return(libraries, [] -> libraries);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Integral Solutions Ltd (Julian Clinton), Nov 29 1993
        Uses xpt_generaltypes for various X definitions which were
        previously defined as a side-effect of loading the Poplog UI.
--- John Gibson, Apr  5 1993
        o  Uses new lib widgetclass_key
        o  Made p*roc_access_p be the autoloadable procedure
            XptPopImportProcedure
        o  Made .ph files in dependencies be loadinclude'd
--- John Gibson, Mar 12 1993
        Made use of nc_<> in require dependent on isinheap(procs)
--- Ian Rogers, Mar 11 1993
        made the placeholder pair in require writeable
--- Adrian Howard, Feb 10 1993
        Changed mishap to a warning
--- Adrian Howard, Feb  9 1993
        Now mishaps if you attempt to load the library twice
--- John Gibson, Sep 23 1992
        Made make_proc_data cache its extern call procedures in a tmpval
        prop
--- Simon Nichols, Oct 30 1991
        Changed -XptGetWidgetDependencies- to ensure that -library_prefix-
        is a proper directory name.
--- Integral Solutions Ltd, Oct 22 1991 (Julian Clinton)
    Changed >< to sys_><.
--- Adrian Howard, Sep  2 1991
        o -demandload- now uses -popuseslist-
        o -demandload_class- altered so "WidgetSetFileMapping" can return
          pathnames as well as library names
--- Ian Rogers, Aug 27 1991
        Made -demandload_widgetset- search -popuseslist- rather than
        the default -popliblist-
--- Robert John Duncan, Jul 19 1991
        Changed class_print of widgetclass_key to use -cucharout- on
        literal strings (to stop string quotes being printed).
        Fixed two misspelt procedure parameters.
--- Andreas Schoter, Jul 18 1991
        Added _XptGetWidgetDependencies_ and associated procedure _require_
        these are procedures common to LIB *AthenaWidgetSet, *MotifWidgetSet
        and *OpenLookWidgetSet that had previously been defined seperately in
        each library.
--- Jonathan Meyer, Jul  6 1991
        Changed to use null_external_ptr
--- Jonathan Meyer, Jul  5 1991
        Changed widgetclass to use XptDataProps style for holding name of
        widget.
--- Jonathan Meyer, Jun 27 1991
        Set widgetclass class apply to XptDescriptors class apply
--- Jonathan Meyer, Jun 27 1991
        Make p*rocess_args_and_call accept exfunc_closures
--- Jonathan Meyer, Jun 26 1991
        Made p*rocess_args_and_call accept floats
--- John Gibson, May 23 1991
        -exload_isbatching- now an active variable
--- John Gibson, May 17 1991
        Added "writeable" attribute to widgetclass definition
--- Jonathan Meyer, Apr 24 1991
        Fixed call to  mishap in demandload_class.
--- Jonathan Meyer, Mar  6 1991
        Added full vectors to list of things accepted by
        p*rocess_args_and_call, and made its pdprops -false-
--- Jonathan Meyer, Feb 16 1991
        Revised WidgetClass printing procedure.
        Added p*rocess_args_and_call, which is used to call a widgetset
        convenience procedure. It does basic string and boolean coercion,
        and also checks for illegal datatypes (class_field_spec contains
        full objects, or is false).
--- Ian Rogers, Dec  7 1990
        Changed XptWidgetSet to use the define = syntax so that it shows
        up in the doing list of mishaps.
--- Jonathan Meyer, Oct 23 1990
        Improved checking on demandload_class. Made demandloading procedures
        set exload batching on for the scope of the procedure.
--- Roger Evans, Oct 23 1990 moved to autoloadable lib
--- Jonathan Meyer, Oct 22 1990
        Incorporated all XptWidgetSet stuff in one file. Greatly Simplified.
        Made it work with exload batching.
--- Jonathan Meyer, Sep  3 1990
        Modified newanyproperty to construct a "perm" property
--- Jonathan Meyer, Sep  1 1990
        Added WidgetSetFileMapping
 */
