/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/src/subsystem_compile.p
 > Purpose:
 > Author:          John Gibson, Dec 29 1992 (see revisions)
 > Documentation:   REF *SUBSYSTEM
 */

;;; ----------- SUBSYSTEM SEARCHLISTS AND COMPILATION --------------------

#_INCLUDE 'declare.ph'
#_INCLUDE '../lib/include/pop11_flags.ph'
#_INCLUDE '../lib/include/subsystem.ph'

constant
        procedure (sys_fname_extn, sys_fname_nam, subscr_subsystem,
        caller_valof, device_full_name, allbutlast,
        Sys$-Subsystem_find, Sys$-Is_dos_device_name
        )
    ;

vars
        pop_syntax_only, pop_pop11_flags, pop_vm_flags,
        pop_default_type
    ;

weak constant
        procedure (syslibcompile, vedsearchfiletypes)
    ;

weak vars
        procedure (prautoloadwarn),
        vedinvedprocess, vedlibname
    ;



;;; ---------------------------------------------------------------------

section $-Sys =>    subsystem_compile_warn, subsystem_compile,
                    subsystem_libcompile, subsystem_searchlist,
                    pop_compiler_subsystem, pop_setpop_compiler;

/*  When this is dlocal to a procedure it indicates a compiler-in-subsystem
    barrier. Procedures having it dlocal (currently pop_setpop_compiler,
    subsystem_compile and Vedcompile (ie lmr)) set it to the value of
    subsystem and then call the compiler directly.
    It must always be false at top level, and is also dlocally set false
    (directly) inside vedprocess.
*/

protected vars
    pop_compiler_subsystem = false;

vars procedure
    subsystem_compile_warn = erase;

    /*  Normal value of pop_setpop_compiler, called by setpop_reset
        after a setpop. (Note that vedsetpop dlocally redefines it
        with a different ss_err procedure.)
    */

define Subsystem_setpop_compiler(ss_err);
    lvars ss_err, ssname = subsystem, compiler, reset;
    returnunless(ssname);
    if ssname == "prolog" then "top" ->> ssname -> subsystem endif;
    subscr_subsystem(SS_COMPILER, ssname, ss_err) -> compiler;
    ssname -> pop_compiler_subsystem;
    unless isundef(subscr_subsystem(SS_RESET, ssname) ->> reset) then
        reset()
    endunless;
    chain(charin, compiler)
enddefine;

protected
define vars pop_setpop_compiler();
    dlocal pop_compiler_subsystem, weakref poplineprefix = false;

    define lconstant ss_err(errms);
        lvars errms;
        if subsystem /== "pop11" then
            "pop11" -> subsystem;
            sys_raise_exception(errms, 1, {'%%S (using pop11 instead)' 16:12},
                                        `W`);
            chainfrom(pop_setpop_compiler, pop_setpop_compiler)
        else
            sysexit -> interrupt;
            mishap(0, errms)
        endif
    enddefine;

    Subsystem_setpop_compiler(ss_err)
enddefine;


/* Subsystem sensitive search lists */

define lconstant Find_extn_ss(extn);
    lvars extn, ssname;
    if VED_LOADED
    and (VED_WEAK vedsearchfiletypes(extn, "subsystem") -> ssname) then
        ssname, SS_NAME
    else
        extn, SS_FILE_EXTN
    endif;
    chain((), Subsystem_find)
enddefine;


define lconstant Get_ss(mode);
    lvars mode;
    /* mode should be a valid value for veddocsubsystem */

    if isstring(mode) then
        Find_extn_ss(if mode = nullstring then
                        pop_default_type
                     else
                        mode
                     endif)
    else
        if mode == "EXTERNAL" then
            if VED_LOADED and VED_WEAK vedinvedprocess then
                caller_valof("subsystem", iscaller(weakref vedprocess) + 1)
            else
                subsystem
            endif
        elseif mode == "CURRENT" then
            subsystem
        else
            mode
        endif or "pop11";
        Subsystem_find((), SS_NAME)
    endif
enddefine;


define subsystem_searchlist(docname, mode);
    lvars docname, mode, list, ss, return_extn = false, extn;
    if isboolean(mode) then
        ;;; optional boolean to say return subsystem extension as well
        ((), docname, mode) -> (docname, mode, return_extn)
    endif;

    if (Get_ss(mode) ->> ss)
    and length(ss) fi_>= SS_SEARCH_LISTS
    and (list_assoc_val(docname, ss(SS_SEARCH_LISTS)) ->> list) then
        unless islist(list) then [^list] -> list endunless;
        ss(SS_FILE_EXTN) -> extn
    else
        [] -> list;
        false -> extn
    endif;
    list, if return_extn then extn endif
enddefine;


define updaterof subsystem_searchlist(list, docname, mode);
    lvars docname, mode, list, search_lists, ss, ss_len;
    unless (Get_ss(mode) ->> ss) do
        mishap(mode, 1, 'CANNOT IDENTIFY SUBSYSTEM')
    endunless;
    ss(SS_SEARCH_LISTS) -> search_lists;    /* MISHAP if ss not long enough */
    lblock;
        lvars SL = search_lists;
        until SL == [] do
            if fast_front(SL) == docname then
                list -> fast_front(fast_back(SL));
                return
            else
                fast_back(fast_back(SL)) -> SL
            endif
        enduntil
    endlblock;
    cons_assoc(docname, list, search_lists) -> ss(SS_SEARCH_LISTS);
enddefine;



/* Subsystem-sensitive "compile" procedure */

define subsystem_compile(input, ssname);
    lvars extn, input, fname, ss, ssname, p;
    dlocal subsystem, pop_compiler_subsystem, weakref poplineprefix = false;

    if isdevice(input) then
        device_full_name(input)
    elseif isprocedure(input) then
        recursive_front(pdprops(input))
    else
        input
    endif -> fname;

    unless ssname then
        if (sys_fname_extn(fname or nullstring) ->> extn) = nullstring then
            pop_default_type -> extn
        endif;

        unless (Find_extn_ss(extn) ->> ss)
        or  isendstring('h', extn)  ;;; assume it's a header file
            and (Find_extn_ss(allbutlast(1, extn)) ->> ss)
        then
            unless Subsystem_find(subsystem, SS_NAME) ->> ss then
                mishap(extn, 1, 'NO SUBSYSTEM KNOWN FOR FILE TYPE')
            endunless;
            sys_raise_exception(extn, ss(SS_NAME), 2,
                {'%NO SUBSYSTEM KNOWN FOR FILE TYPE \'%S\' (using %P)' 16:12},
                    `W`)
        endunless;

        ss(SS_NAME) -> ssname
    endunless;

    subscr_subsystem(SS_COMPILER, ssname) -> p;
    ssname ->> subsystem -> pop_compiler_subsystem;
    if isstring(fname) or isword(fname) then
        subsystem_compile_warn(fname)
    endif;
    p(input)
enddefine;


define subsystem_libcompile(name, lib_list) -> dir;
    lvars   n, name, lib_list, dir, usename, fname, doing_slc;

    dlocal  weakref poplastchar,
            weakref pop_syntax_only = false,
            weakref popnewline = false,
            weakref current_section,
            weakref pop_vm_flags,
            weakref pop_pop11_flags,
            weakref poplineprefix = false,
            weakref pop_pas_mode = false,
            ;

    ;;; must do these after setting pop_pas_mode false, since it
    ;;; may change them
    if testdef pop_pop11_flags then
        weakref pop_pop11_flags fi_&& POP11_SLC_PROPAGATE
                            -> weakref pop_pop11_flags
    endif;
    0 -> weakref pop_vm_flags;

    define lconstant search(lib_list) -> dir;
        lvars dir, lib_list, dev, ssname;
        dlocal subsystem_compile_warn;
        for dir in lib_list do
            ;;; a top-level list in a searchlist is considered an annotated
            ;;; entry
            if islist(dir) then hd(dir) -> dir endif;   ;;; annotated
            ;;; deref any idents/words
            repeat
                if isident(dir) then
                    idval(dir) -> dir
                elseif isword(dir) then
                    valof(dir) -> dir
                else
                    quitloop
                endif
            endrepeat;
            ;;; must now be a (search)list, procedure, or string
            if islist(dir) then
                returnif(search(dir) ->> dir)
            elseif isprocedure(dir) then
                returnif(fast_apply(if doing_slc then name else fname endif,
                                                    dir) ->> dir)
            elseif isstring(dir) then
                if (sysopen(dir dir_>< fname, 0, false, `A`) ->> dev) then
                    if doing_slc then
                        weakref[syslibcompile] prautoloadwarn(name);
                        erase -> subsystem_compile_warn;
                        "pop11"
                    else
                        false
                    endif -> ssname;
                    subsystem_compile(dev, ssname);
                    return
                endif
            else
                mishap(dir, 1, 'INVALID ITEM IN LIBRARY SEARCHLIST')
            endif
        endfor;
        false -> dir
    enddefine;

#_IF DEF WIN32
    returnif(Is_dos_device_name(sys_fname_nam(name)))(false -> dir);
#_ENDIF

    ;;; translate a word identifier pathname $-foo$-baz to 'S-fooS-baz'
    name -> usename;
    if issubstring('$-', name) ->> n then
        copy(if isword(name) then fast_word_string(name) else name endif)
                    -> usename;
        repeat
            `S` -> fast_subscrs(n, usename);
            quitunless(issubstring('$-', n fi_+ 2, usename) ->> n)
        endrepeat
    endif;

    false -> dir;
    unless caller(1) == weakref syslibcompile ->> doing_slc then
        ;;; try subsystem lib list
        usename -> fname;
        if sys_fname_extn(fname) = nullstring then
            fname sys_>< pop_default_type -> fname
        endif;
        search(subsystem_searchlist("vedlibname", sys_fname_extn(fname)))
                                -> dir
    endunless;

    unless dir then
        ;;; try general lib list
        usename -> fname;
        if sys_fname_extn(fname) = nullstring then
            fname sys_>< if pop_default_type = '.ph' then '.ph' else '.p' endif
                                -> fname
        endif;
        search(lib_list) -> dir
    endunless;

    if isstring(dir) and not(doing_slc) and VED_LOADED then
        fname -> VED_WEAK vedlibname
    endif
enddefine;


endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Oct 20 1999
        Added Win32 check for DOS device names in subsystem_libcompile to
        stop it hanging up when autoloading words like "AUX", "CON", etc.
--- John Gibson, Sep 10 1996
        Fixed missing initial percents in sys_raise_exception messages.
--- John Gibson, Mar  5 1996
        Made poplineprefix locally false in subsystem invocation procedures.
--- John Gibson, Jul 29 1995
        Made Subsystem_setpop_compiler just return if subsystem is false
--- John Williams, Nov  3 1994
        %S instead of %P in a couple of places, in case pop_pr_quotes is true
--- John Gibson, Sep 21 1993
        Changed subsystem_libcompile (again!) to allow idents/words in a
        searchlist to contain strings/procedures
--- John Gibson, Jun 17 1993
        Moved in pop_setpop_compiler from poplog_main.p
--- John Gibson, Jun 12 1993
        weakref'ed various things
--- John Gibson, Jun  2 1993
        Made subsystem_libcompile translate a word identifier pathname
        $-foo$-baz to 'S-fooS-baz'
--- John Gibson, May 17 1993
        Changed subsystem_searchlist so that it can take an optional bool
        arg saying return subsystem extension (or false if no ss found)
--- John Williams, Jan 18 1993
        Added updater for subsystem_searchlist (cf. isl-er.206)
 */
