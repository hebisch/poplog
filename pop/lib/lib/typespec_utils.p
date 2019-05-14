/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/lib/typespec_utils.p
 > Purpose:         Utilities for field typespec syntax
 > Author:          John Gibson, Jul  2 1990 (see revisions)
 > Documentation:   REF *DEFSTRUCT, REF *KEYS
 > Related Files:   LIB *DEFCLASS, LIB *P_TYPESPEC, LIB *EXACC
 */
compile_mode:pop11 +strict;

#_INCLUDE '$usepop/pop/lib/include/vm_flags.ph'

section $-typespec_utils;

lconstant
    subspec_starters = [: ! |  (  % "{", "^" % ],
    ;

constant
    POINTER = ">->",
    procedure compound_fldmode = newproperty([], 32, true, "tmparg"),
    spec_starters = [. #] <> subspec_starters,
    non_names = [; : . , # | ! <- -> = ( ) < > endexload %"[","]","{","}","^"%],
    ;

define need_nextitem(needlist) -> item;
    lvars needlist, item = itemread();
    unless fast_lmember(item, needlist) then
        mishap(item, 1, 'UNEXPECTED ITEM, EXPECTING ' sys_>< hd(needlist))
    endunless
enddefine;

define checkr_name(item, allow_num) -> item;
    lvars item, allow_num;
    returnif(allow_num and isnumber(item));
    unless isword(item) and not(fast_lmember(item, non_names)) then
        mishap(item, 1, 'VALID NAME (I.E. WORD) EXPECTED')
    endunless
enddefine;

define prefix_field(pre, f);
    lvars pre, f;
    unless f then
        false
    elseif isref(f) then
        if fast_cont(f) ->> f then consref(pre <> f) else false endif
    else
        pre <> f
    endunless
enddefine;

    /*  Deal with unwanted itemisation of :, |, ^ and #
    */
define split_chars();
    lvars c, item = nextitem();
    if isword(item) and datalength(item) fi_> 1
    and fast_lmember(fast_subscrw(1,item)->>c, [`:` `!` `|` `#` `^`]) then
        consword(c,1) -> c;
        readitem() -> ;
        [%  c;
            if (allbutfirst(1,item) ->> item) == "-"
            and isnumber(nextreaditem()) then
                -readitem()
            else
                item
            endif
        %] nc_<> proglist -> proglist
    endif
enddefine;

define type_idname = nonop <>(% "':typespec'" %) enddefine;

define deref_struct1(spec, fldmode) -> (spec, fldmode);
    lvars spec, fldmode, fm;
    while islist(fldmode) and listlength(fldmode) == 1
    and (not(hd(fldmode) ->> fm) or fm = #_< consref(false) >_#)
    do
        ;;; struct with 1 unnamed field
        hd(tl(spec)) -> spec;
        if ispair(spec) then
            compound_fldmode(spec)
        elseif fm then  ;;; i.e. consref(false)
            consref(true)
        else
            true
        endif -> fldmode
    endwhile
enddefine;

    /*  Read a single typespec
    */
define read_typespec(pop, try_only) -> (spec, fldmode, defname);
    lvars   pop, spec, fldmode, try_only, defname = false;

    lconstant procedure read_spec;

    define lconstant read_struct() -> (specs, fldmodes);
        lvars item, (specs, fldmodes) = ([], []), f, spec, fldmode;
        repeat
            ;;; external struct must start at pointer
            unless pop then POINTER :: specs -> specs endunless;
            repeat
                split_chars();
                quitif((itemread() ->> item) == "}" or item == "|");
                if item == POINTER then
                    ;;; align at pointer
                    if specs == [] or hd(specs) /== POINTER then
                        item :: specs -> specs
                    endif;
                    split_chars(), itemread() -> item
                endif;
                if fast_lmember(item, spec_starters) then
                    ;;; no field name
                    item :: proglist -> proglist;
                    false -> item
                else
                    checkr_name(item, false) ->
                endif;
                read_spec(false, true) -> (spec, fldmode);
                if pop and islist(spec) then
                    ;;; only for pop -- 'explode' fldmodes of substruct
                    if spec /== [] and hd(spec) == POINTER then
                        tl(spec) -> spec
                    endif;
                    rev(spec) nc_<> specs -> specs;
                    if item then item <> "_" -> item endif;
                    fast_for f in fldmode do
                        if item then prefix_field(item, f) else false endif
                                                    :: fldmodes -> fldmodes
                    endfor
                else
                    spec :: specs -> specs;
                    if ispair(spec) or isvector(spec) then
                        consref(item)
                    elseif isref(fldmode) then
                        item -> fast_cont(fldmode), fldmode
                    else
                        item
                    endif :: fldmodes -> fldmodes
                endif;
                need_nextitem(#_< [, | %"}"%] >_#) -> item;
                quitif(item /== ",")
            endrepeat;
            quitif(item == "}");
            ;;; else item is | for a union
            item :: specs -> specs
        endrepeat;
        (rev(specs), rev(fldmodes)) -> (specs, fldmodes);
        fldmodes -> compound_fldmode(specs)
    enddefine;

    define lconstant read_func();
        lvars item = false, arg, nargs = 0, fltsingle = 0, fflag, spec;
        lconstant dotdotdot = "'...'";
        ;;; read arg spec(s)
        while item /== ")" do
            if isinteger(itemread() ->> item) then
                ;;; counts as item args
                item
            else
                if item == "." and nextreaditem() == "." then
                    readitem() -> ;
                    if nextreaditem() == "." then
                        ;;; ... = variadic
                        readitem() -> , dotdotdot -> item
                    else
                        "." :: proglist -> proglist
                    endif
                endif;
                if isword(item) and not(fast_lmember(item, non_names)) then
                    if item == "N" and nargs == 0 then
                        ;;; old-style variadic (only recognised on 1st arg)
                        dotdotdot
                    else
                        item
                    endif
                else
                    item :: proglist -> proglist;
                    false
                endif
            endif -> arg;
            if nextitem() == "<" then
                ;;; flag to control passing of (d)decimal. "SF" = pass
                ;;; as single, "DF" = pass as double (which is the default,
                ;;; so is never necessary)
                readitem() -> ;
                need_nextitem([SF DF]);     ;;; fflag value
                need_nextitem([>]) -> ;
                arg or "undef" -> arg
            else
                false
            endif -> fflag;
            need_nextitem(if arg == dotdotdot then [)] else [, )] endif)
                                        -> item;
            quitif(item == ")" and not(arg) and nargs == 0);    ;;; no args

            if fflag == "SF" then 1 else 0 endif -> fflag;
            if arg == dotdotdot then
                fltsingle || ((-fflag)<<nargs) -> fltsingle;
                false -> nargs
            else
                unless isinteger(arg) then 1 -> arg endunless;
                repeat arg times
                    fltsingle || (fflag<<nargs) -> fltsingle;
                    nargs + 1 -> nargs
                endrepeat
            endif
        endwhile;

        ;;; result spec
        read_typespec(false, true) -> (spec, , defname); ;;; false if none
        consvector(conspair(nargs,fltsingle), spec, 2), false
    enddefine;

    define lconstant read_spec(inner_spec, fldmode);
        lvars   spec, flag, p, inner_spec, fldmode, id, fm, f, s, nonwrit;

        split_chars();
        if not(inner_spec) and (pop11_try_nextitem([. #]) ->> flag) then
            ;;; simulate empty structure
            [%"{","}",flag%] nc_<> proglist -> proglist
        endif;

        if not(inner_spec and frozval(2,inner_spec))
        and (pop11_try_nextitem(subspec_starters) ->> flag) then
            ;;; spec follows
            if flag == "{" then
                ;;; struct
                read_struct() -> (spec, fm);
                unless pop then
                    deref_struct1(spec, fm) -> (s, f);
                    if ispair(s) or isvector(s) then
                        (s, f) -> (spec, fm)
                    endif
                endunless
            elseif flag == "(" then
                ;;; function
                read_func() -> (spec, fm)
            else
                ;;; typename
                checkr_name(itemread(), true) -> spec;
                true -> fm;
                flag == "!" or flag == "|" -> nonwrit;
                if isword(spec) then
                    spec -> defname;
                    if sys_current_ident(type_idname(spec)) ->> id then
                        ;;; defined as a typename
                        fast_destpair(fast_idval(id)) -> spec -> fm;
                        if nonwrit and ispair(spec) then
                            if islist(spec) then
                                copylist(spec) -> spec;
                                [% fast_for f in fm do
                                    if isref(f) then f else consref(f) endif
                                endfor %]
                            else
                                copy(spec) -> spec;
                                consref(true)
                            endif ->> fm -> compound_fldmode(spec)
                        endif
                    endif
                ;;; else should be integer
                endif;
                if nonwrit and not(ispair(spec)) then
                    false -> fm
                elseif flag == "^" then
                    ;;; pointer value
                    consref(spec) -> spec
                endif
            endif;
            if pop11_try_nextitem("[") do
                ;;; array
                if pop11_try_nextitem("]") then
                    false -> p          ;;; unsized
                else
                    itemread() -> p;    ;;; size
                    need_nextitem(#_< [%"]"%] >_#) ->
                endif;
                if ispair(spec) then false -> fm endif;
                conspair(spec, p) -> spec;
                if not(fm) or isref(fm) then
                    consref(true) -> compound_fldmode(spec)
                endif
            endif;
            if inner_spec then
                unless ispair(spec) or isvector(spec) then
                    fm -> fldmode
                endunless;
                ;;; implicit type access 'procedure'
                identfn(%":", spec%) -> p;
                ':' >< spec -> pdprops(p)
            else
                fm -> fldmode
            endif

        elseif inner_spec then
            ;;; procedure name following . or #
            procedure;
                lvars item;
                if fast_lmember(nextitem(), non_names) then
                    ;;; dummy to stop consing new external pointer rec
                    "identfn"
                elseif isprocedure(itemread() ->> item) then
                    return(item)
                else
                    checkr_name(sys_read_path(item, false, false), false)
                endif -> item;
                dlocal pop_autoload = true;
                sys_current_val(item)
            endprocedure() -> p;

            unless frozval(2,inner_spec) or p == identfn then
                updater(p) -> fldmode
            ;;; else for conversion procedure, leave fldmode alone
            elseif islist(fldmode) then
                false -> fldmode
            endunless

        elseif try_only then
            return(false, false)

        elseif pop then
            ;;; pop record/vector only
            return("full", true)

        else
            mishap(readitem(), 1, 'EXPECTING TYPESPEC (e.g. :, !, { etc)')
        endif;

        if inner_spec then
            p -> pdpart(inner_spec);
            inner_spec -> spec
        endif;

        split_chars();
        if pop11_try_nextitem([. #]) ->> flag then
            ;;; access/conversion spec/procedure
            read_spec( identfn(% spec, flag/=="." %), fldmode )
                                                -> (spec, fldmode);
            false -> defname
        endif;

        (spec, if fldmode then fldmode else consref(true) endif)
    enddefine;  /* read_spec */

    read_spec(false, true) -> (spec, fldmode)
enddefine;  /* read_typespec */

define get_declarator(item);
    lvars item;
    if item == "constant" then
        sysCONSTANT, sysGLOBAL
    elseif item == "vars" then
        sysVARS, sysGLOBAL
    elseif item == "lconstant" then
        sysLCONSTANT, false
    elseif item == "lvars" then
        sysLVARS, false
    elseif item == "dlvars" then
        sysDLVARS, false
    else
        false, sysGLOBAL
    endif
enddefine;

define exacc_attributes() -> (checking, mode);
    lvars (checking, mode) = (true, 1), alist;
    if islist(nextitem()) then
        [% "[", dl(itemread()), "]" %] nc_<> proglist -> proglist
    endif;
    returnunless(nextitem() == "[");
    listread() -> alist;
    if fast_lmember("fast", alist) and pop_vm_flags &&=_0 VM_NO_FAST then
        ;;; fast
        false -> checking
    endif;
    if fast_lmember("@", alist) then
        ;;; address mode
        mode || 2:10e8 -> mode
    endif;
    if fast_lmember("nc", alist) then
        ;;; non-constructive, i.e. fixed exptr result
        mode || 2:01e8 -> mode
    endif;
enddefine;

endsection;     /* $-typespec_utils */

constant $-typespec_utils = true;   ;;; for -uses-



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 16 1996
        Made exacc_attributes ignore "fast" if VM_NO_FAST set in
        pop_vm_flags.
--- John Gibson, Apr 27 1993
        Moved get_exload_initial to exload.p
--- John Gibson, Mar 30 1993
        Added get_exload_initial
--- John Gibson, Dec 15 1992
        Stopped read_spec calling deref_struct1 on pop typespec
        (cf isl-fr.4486)
--- John Gibson, Oct 27 1992
        Changed to use sys_current_val
--- John Gibson, Sep 15 1992
        Changed [sf] flag to <SF> (square brackets would stop it being used
        after a typespec)
--- John Gibson, Sep  7 1992
        Fixed exacc_attributes to test for already-compiled list on
        proglist.
--- John Gibson, Aug 21 1992
        Changed to allow new extended format for external func specs
        e.g. foobaz(x, y<SF>, z, ...), in particular to allow <SF> after arg
        to indicate (d)decimal passed as single float rather than double.
--- John Gibson, Jul  9 1992
        Fixed bug in -read_typespec- that prevented external structs being
        'exploded' inside pop records
--- John Gibson, Nov 26 1990
        Added | in structures to allow unions; added ! to flag `non-writeable'
        type (but | also retains this role for compatibility).
--- John Gibson, Oct 19 1990
        Added -exacc_attributes-
--- John Gibson, Oct 16 1990
        Now allows a typespec to begin with . or #
--- John Gibson, Sep 19 1990
        Bug fixes to -read_spec-
--- John Gibson, Sep 14 1990
        Added support for ^<typename> for getting pointer value
--- John Gibson, Sep  5 1990
        Correction to -read_spec- for dealing with bracket structures
--- John Gibson, Aug 23 1990
        Fix to -deref_struct1-
--- John Gibson, Aug 14 1990
        Fixed incorrect return from -read_spec- for default pop "full"
        spec.
--- John Gibson, Jul  2 1990
        Split off from defclass.p as separate library file
 */
