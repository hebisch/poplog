/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/pml/src/compile.p
 > Purpose:         PML: Compiler
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml =>
    ml_compile
    ml_filetype
    ml_sigfiletype
    ml
    pml
;

constant procedure (    ;;; forward
    ml_load,
        ;;; compiles a named file
);

vars
    ml_uselist = [],
        ;;; list of files waiting to be compiled
    ml_filetype = '.ml',
        ;;; default file extension for program files
    ml_sigfiletype = '.sig',
        ;;; default file extension for signature files
    ml_phase = false,
        ;;; current phase of compilation: <false> means not in ML
    ml_quiet = true,
        ;;; flag used to suppress all top-level printing
        ;;; (set <false> in "startup.ml")
    ml_quiet_load = false,
        ;;; flag used to suppress printing during loads
    ml_timer = false,
        ;;; flag for display of timing statistics
    ml_warnings = true,
        ;;; flag for display of warning messages
    ml_autoloading = true,
        ;;; flag to enable/disable autoloading
    ml_compiled_from_ved = false,
        ;;; flag indicating call from LMR, etc.
;

lvars
    curr_in,
        ;;; current character input stream
    curr_items,
        ;;; current item input stream
    open_files = [],
        ;;; list of files being compiled
    open_dirs = [],
        ;;; list of directories in which files are being compiled
    local_refs = [],
        ;;; flags local to the compiler
    comp_cputime = 0, comp_gctime = 0,
        ;;; accumulated compilation time
    exec_cputime = 0, exec_gctime = 0,
        ;;; accumulated execution time
;


;;; ml_localise:
;;;     localise a reference throughout a call to the compiler

define ml_localise(r);
    lvars r;
    {% r, ml_cont(r) %} :: local_refs -> local_refs;
enddefine;

define lconstant restore_local_refs();
    lvars v, r;
    For v in local_refs do
        Subscrv(1, v) -> r;
        ml_cont(r), Subscrv(2, v) -> ml_cont(r) -> Subscrv(2, v);
    endfor;
enddefine;


;;; record_input:
;;;     adds the name of the given input stream to the -open_files- list
;;;     and its directory path to -open_dirs-

define lconstant record_input(name);
    lvars name;
    if member(name, open_files) then
        ml_error(
            'compilation cycle (already compiling this file)\n\t%S\n',
            [^name], false, false);
    endif;
    conspair(name, open_files) -> open_files;
    sys_fname_path(name) -> name;
    if open_dirs == [] or name /= Front(open_dirs) then
        conspair(name, open_dirs) -> open_dirs;
    endif;
enddefine;


;;; sourcefile:
;;;     searches for a named file, trying different file extensions and
;;;     directories as appropriate; returns a pathname for the file if
;;;     it can be found.

define lconstant exists(path);
    lvars path;
    sys_file_stat(path, {}) and not(sysisdirectory(path));
enddefine;

define lconstant search(name);
    lvars dir, name, path;
    ;;; try name relative to each open directory
    For dir in open_dirs do
        returnif(exists(dir dir_>< name ->> path))(path);
    endfor;
    ;;; try name alone
    returnif(exists(name))(name);
    ;;; try name relative to directories in the library list
    For dir in ml_libdirs do
        returnif(exists(dir dir_>< name ->> path))(path);
    endfor;
    false;
enddefine;

define sourcefile(name);
    lvars name, path;
    if search(name) ->> path then
        path;
    elseif sys_fname_extn(name) /= nullstring then
        false;
    else
        search(name <> ml_filetype)
        or ml_filetype /= '.ml' and search(name <> '.ml')
        or search(name <> ml_sigfiletype)
        or ml_sigfiletype /= '.sig' and search(name <> '.sig')
    endif;
enddefine;


;;; ml_top_level:
;;;     "read-eval-print" loop

define ml_top_level();
    lvars   dec, env, ccpu, cgc, xcpu, xgc;
    dlocal  ml_phase = "top_level", ml_uselist = [], ml_errors_printed = 0,
            ml_errors_raised = 0;
    until do_commands(curr_items) == termin do
        systime() -> ccpu; popgctime -> cgc;
        ;;; 1st pass: parse
        "parse" -> ml_phase;
        parse(curr_items) -> (dec, env);
        unless ml_errors_raised == 0 then interrupt() endunless;
        ;;; 2nd pass: typecheck
        "typecheck" -> ml_phase;
        typecheck(dec);
        unless ml_errors_raised == 0 then interrupt() endunless;
        ;;; 3rd pass: generate code
        "gencode" -> ml_phase;
        gencode(dec);
        unless ml_errors_raised == 0 then interrupt() endunless;
        ;;; Record the time taken to do the compilation.
        (systime() - ccpu) / 100.0 -> ccpu;
        (popgctime - cgc) / 100.0 -> cgc;
        ;;; Update accumulated compilation time.
        comp_cputime + ccpu -> comp_cputime;
        comp_gctime + cgc -> comp_gctime;
        ;;; Execute the code planted.
        "execute" -> ml_phase;
        systime() -> xcpu;
        popgctime -> xgc;
        mlEXECUTE();
        unless ml_errors_raised == 0 then
            interrupt();
        elseunless ml_errors_printed == 0 then
            ;;; open a line after last error/warning
            printf('\n');
            0 -> ml_errors_printed;
        endunless;
        ;;; Record the time taken to do the execution.
        (systime() - xcpu) / 100.0 -> xcpu;
        (popgctime - xgc) / 100.0 -> xgc;
        ;;; Update accumulated execution time.
        exec_cputime + xcpu -> exec_cputime;
        exec_gctime + xgc -> exec_gctime;
        ;;; Add bindings into the global environment
        globalise_bindings(env);
        ;;; ... and print them
        unless ml_quiet then print_bindings(dec) endunless;
        if ml_timer then
            ;;; print time taken to do the compilation and execution
            printf(cgc, ccpu,
                ';;; Compilation: CPU time = %p secs, GC time = %p secs\n');
            printf(xgc, xcpu,
                ';;; Execution:   CPU time = %p secs, GC time = %p secs\n');
        endif;
        "top_level" -> ml_phase;
        revapp(ml_uselist, ml_load);
        [] -> ml_uselist;
    enduntil;
enddefine;


;;; ml_compile:
;;;     compiles a named file, device etc.
;;;     Since this is a subsystem compiler it must set cucharin dlocally
;;;     in its environment.

define ml_compile(input);
    lvars input;
    dlocal
        cucharin, popfilename, poplinenum,
        curr_in, curr_items, ml_linenum,
        open_dirs, open_files,
        ml_compiled_from_ved,
        ml_errors_printed = 0,
        ml_errors_raised = 0,
        popdprecision = true,
        popradians = true,
        pop_pr_ratios = false,
        popprompt = '- ',
        pop_default_type = ml_filetype,
        local_refs = [],
        0 %, restore_local_refs() %,
    ;
    GUARD_IO;
    setup_std_io();
    instream(input) -> curr_in;
    new_itemiser(curr_in) -> curr_items;
    instream_proc(curr_in) -> cucharin;
    unless curr_in == std_in or instream_name(curr_in) = nullstring then
        instream_name(curr_in) -> popfilename;
        record_input(popfilename);
    endunless;
#_IF VED_LOADED
    if is_ved_lmr_stream(cucharin) then
        true -> ml_compiled_from_ved;
    else
#_ELSE
        if true then
#_ENDIF
        1 -> poplinenum;
    endif;
    sysCOMPILE(ml_top_level);
enddefine;


;;; ml_load:
;;;     compiles the named file, printing timing statistics if required

define ml_load(name);
    lvars   name, dev, start_ccpu, start_cgc, start_xcpu, start_xgc, timing;
    dlocal  ml_timer, ml_quiet;
    unless readable(name) ->> dev then
        ml_error('cannot open file (%M)\n\t%S\n', [^name], popfilename, false);
    endunless;
    ml_timer -> timing; false -> ml_timer;
    if ml_quiet_load then true -> ml_quiet endif;
    name -> vedvedname;
    comp_cputime -> start_ccpu; comp_gctime -> start_cgc;
    exec_cputime -> start_xcpu; exec_gctime -> start_xgc;
    subsystem_compile(dev, "ml");
    if timing then
        ;;; print compilation and execution time for this file
        printf(name, ';;; File: %p\n');
        printf(comp_gctime - start_cgc, comp_cputime - start_ccpu,
            ';;; Compilation: CPU time = %p secs, GC time = %p secs\n');
        printf(exec_gctime - start_xgc, exec_cputime - start_xcpu,
            ';;; Execution:   CPU time = %p secs, GC time = %p secs\n');
    endif;
enddefine;

;;; ml_autoload:
;;;     autoloads the file corresponding to the word -name-. An optional
;;;     flag -sig- determines which file extension to use.

define ml_autoload(name);
    lvars   name, sig = false;
    dlocal  ml_quiet = true, ml_timer = false, ml_compile_debug = false,
            vedvedname;
    if isboolean(name) then name -> sig -> name endif;
    returnunless(ml_autoloading)(false);
    if sig then
        sourcefile(name sys_>< ml_sigfiletype)
        or ml_sigfiletype /= '.sig' and sourcefile(name sys_>< '.sig')
    else
        sourcefile(name sys_>< ml_filetype)
        or ml_filetype /= '.ml' and sourcefile(name sys_>< '.ml')
    endif -> name;
    returnunless(name)(false);
    prautoloadwarn(name);
    ml_load(name);
    true;
enddefine;

;;; load_cmd:
;;;     allow "load" as a PML command

define lconstant load_cmd(name, args);
    lvars   name, args;
    dlocal  pop_pr_exception = ml_pr_exception;
    if args /== [] then
        sourcefile(hd(args)) or hd(args);
    elseif vedvedname /= nullstring then
        vedvedname;
    else
        mishap('no filename given to load', []);
    endif;
    chain((), ml_load);
enddefine;
;;;
load_cmd -> command_table("load");

;;; ml, pml:
;;;     switch from Pop-11 to ML.

global constant macro (
    ml  = ["ml" -> sys_compiler_subsystem(`c`);],
    pml = nonmac ml,
);

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Changes for new exception printing
--- Robert John Duncan, Dec 20 1994
        Disabled debugging during autoloading
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Nov  8 1994
        Added ml_errors_continue
--- Robert John Duncan, Oct 24 1994
        Changes to error reporting. Added new variables ml_compiled_from_ved
        and ml_errors_printed
--- John Gibson, May  1 1993
        Fixed ml_load to use subsystem_compile
--- John Gibson, Jan 15 1993
        Made ml_compile a constant (and added comment about cucharin).
--- Robert John Duncan, Dec  9 1992
        Removed protect/unprotect for pop{filename,linenum}
--- Robert John Duncan, Apr  6 1992
        Renamed -ml_*searchpath- to -ml_libdirs-
--- Robert John Duncan, Nov  1 1991
        Renamed warning and error procedures.
--- Robert John Duncan, Mar 18 1991
        Added facility for localising references within the compiler.
--- Robert John Duncan, Mar  1 1991
        Added -curr_in/curr_items-. Included definition of -load_cmd-
        from "commands.p".
--- Robert John Duncan, Feb 11 1991
        New style error messages
--- Robert John Duncan, Feb  4 1991
        New environment interface.
--- Robert John Duncan, Oct 31 1990
        Removed assignment to -subsystem- from -ml_compile-: now handled by
        lib subsystem
--- Rob Duncan, Jan  9 1990
        Removed reference to -v*ed_try_??-
--- Rob Duncan, Oct 26 1989
        Reorganised
 */
