/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/debugger/user.p
 > Purpose:         Symbolic debugger: user interface
 > Author:          Simon Nichols and Robert John Duncan, Jun 18 1991 (see revisions)
 > Documentation:   HELP * DEBUGGER
 > Related Files:   debugger/core.p
 */

compile_mode:pop11 +strict;

section $-debugger =>
    stopin
    ved_stopin
    debugger
    enddebugger
    debugger_interrupt
    debugger_pause_interval
    debugger_default_command
    debugger_pr
;

lvars
    db_level = 0,
        ;;; current debugger level (number of recursive invocations)
;

vars
    debugger_X_interface    = false,
        ;;; whether the debugger X interface is currently in use
    debugger_pause_interval = 1_/2,
        ;;; how long to pause when animating -- default is 1/2 second
;


/*
 * File name utilities
 */

;;; file_full_name:
;;;     given a file name, return its full path name.

define lconstant file_full_name(filename) -> filename;
    lvars filename, dev;
    if readable(filename) ->> dev then
        device_full_name(dev) -> filename;
        sysclose(dev);
    endif;
enddefine;

;;; file_relative_name:
;;;     given a file name, return its relatve path name.

define lconstant file_relative_name(filename) -> filename;
    lvars filename;
    file_full_name(filename) -> filename;
    if issubstring(current_directory, filename) then
        allbutfirst(length(current_directory), filename) -> filename;
    endif;
enddefine;


/*
 * Printing utilities
 */

define vars debugger_pr =
    syspr(%%);
enddefine;

define vars db_pr() with_nargs 1;
    dlocal debugger_debugging = false, pr = debugger_pr;
    pr();
enddefine;

define vars db_printf() with_nargs 1;
    dlocal debugger_debugging = false, pr = debugger_pr;
    printf();
enddefine;

define vars pr_bracketed_number(n, width);
    lvars n, width;
    dlocal debugger_debugging = false;
    pr('[');
    pr_field(n, width, `\s`, false);
    pr(']');
enddefine;


/*
 * Other utilities
 */

;;; debugging_context:
;;;     check whether there's an active procedure which can be debugged.

define lconstant debugging_context();
    if curr_exec_pdbr then
        true;
    else
        db_pr('No active procedures compiled in debugging mode\n');
        false;
    endif;
enddefine;

;;; db_display_sourcefile:
;;;     display the sourcefile file, at line line.

define vars db_display_sourcefile(file, line);
    lvars   line, file;
    dlocal  vedinputfocus, wvedalwaysraise = true, pop_pr_quotes = false;
    if iscaller(vedprocess) and pop_charin_device /== popdevin then
        unless file = vedpathname then
            vededitor(vedveddefaults, file);
        endunless;
        vedjumpto(line, 1);
        vedcheck();
    else
        returnunless(readable(file));
        npr(sys_read_lines(file, line, line) -> );
    endif;
enddefine;

;;; db_pause:
;;;     pause at the current line (used when animating).

define vars db_pause();
    if iscaller(vedprocess) then
        vedrefreshrange(vedline, vedline, false);
    endif;
    syssleep(debugger_pause_interval * 100);
enddefine;

;;; db_error:
;;;     redefinable procedures which signals an error.

define vars db_error =
    mishap(%%);
enddefine;


/*
 * Default command (an active variable)
 */

lvars
    default_command = [step],
;

constant procedure db_validate_command; ;;; forward

define active debugger_default_command;
    default_command or [none];
enddefine;
;;;
define updaterof active debugger_default_command(cmd);
    lvars cmd;
    if not(cmd) or cmd == [] or cmd == "none" or cmd = [none] then
        false;
    elseif islist(cmd) then
        db_validate_command(hd(cmd)) -> ;
        cmd;
    else
        [% db_validate_command(cmd) and cmd %];
    endif -> default_command;
enddefine;


/*
 * Reading commands
 */

vars
    db_command_line = [],
;

define vars db_read_command_line();
    dlocal pop_readline_prompt = 'debug(' sys_>< db_level sys_>< '): ';
    lconstant db_readline = caller_valof("readline", false);
    lvars line = db_readline();
    if line == termin then
        [quit];
    elseif line == [] then
        debugger_default_command;
    elseif hd(line) == "enddebugger" then
        [quit];
    else
        line;
    endif;
enddefine;

define vars db_readitem();
    if null(db_command_line) then
        termin;
    else
        dest(db_command_line) -> db_command_line;
    endif;
enddefine;

define vars db_nextreaditem();
    if null(db_command_line) then
        termin;
    else
        hd(db_command_line);
    endif;
enddefine;

define vars db_try_nextreaditem(item);
    lvars item;
    db_nextreaditem() == item and db_readitem();
enddefine;

define vars db_read_positive_integer(message) -> n;
    lvars message, n = db_readitem();
    unless isinteger(n) and n > 0 then
        db_error(n, 1, message <> ' (a positive integer) expected');
    endunless;
enddefine;

define vars db_read_filename() -> file;
    lvars file;
    if isstring(db_nextreaditem()) then
        file_full_name(db_readitem()) -> file;
        unless db_try_nextreaditem(":") then
            db_error(db_readitem(), 1, 'Expected : after file name');
        endunless;
    else
        curr_exec_pdbr and pdbr_file(curr_exec_pdbr) -> file;
    endif;
enddefine;

define vars db_read_path() -> name;
    lvars name = db_readitem();
    dlocal proglist = db_command_line;
    sys_read_path(name, false, false) -> name;
    proglist -> db_command_line;
    unless isword(name) then
        db_error(name, 1, 'Identifier name (a word) expected');
    endunless;
enddefine;

define vars db_read_paths();
    [%  repeat
            db_read_path();
            db_try_nextreaditem(",") -> ;   ;;; allow optional commas
            quitif(db_nextreaditem() == termin);
        endrepeat;
    %];
enddefine;

define vars db_read_proc_spec() -> (file, names);
    lvars file = db_read_filename(), names;
    lvars name = db_read_path();
    [%  while db_try_nextreaditem(".") do
            lvars name1 = db_readitem();
            unless isword(name1) then
                db_error(name1, 1,
                    'Procedure name component (a word) expected');
            endunless;
            name1;
        endwhile;
    %] -> names;
    if names == [] then name else name :: names endif -> names;
enddefine;

define vars db_read_file_and_line() -> (file, line);
    lvars file = db_read_filename(), line;
    unless file then
        db_error(0, 'No current file (please specify file name)');
    endunless;
    db_read_positive_integer('Line number') -> line;
enddefine;

define vars db_read_breakpoint_number();
    if db_try_nextreaditem("all") then
        false;
    else
        db_read_positive_integer('Breakpoint number');
    endif;
enddefine;

define vars db_read_frame_number();
    if db_nextreaditem() == termin then
        curr_frame_num;
    else
        db_read_positive_integer('Frame number');
    endif;
enddefine;

define vars db_read_userstack_depth();
    if db_nextreaditem() == termin then
        false;
    else
        db_read_positive_integer('Userstack depth');
    endif;
enddefine;

define vars db_read_ignore_args();
    (db_try_nextreaditem("only"), db_read_proc_spec());
enddefine;

define vars db_read_default_command();
    return(db_command_line);
enddefine;


/*
 * Compiling Pop-11 code in the current debugging context
 */

define vars db_compile_line();
    db_compile(db_command_line);
    sys_clear_input(pop_charin_device);
enddefine;

define vars db_compile_program();
    db_compile(cucharin);
    sys_clear_input(pop_charin_device);
enddefine;


/*
 * Compute and collect information required by debugger commands
 */

;;; db_get_backtrace_info:
;;;     returns a list of 4 element vectors, each representing a frame:
;;;       (whether current, frame number, owner name, executing line number)
;;;     and the frame number of the highest numbered frame.

define db_get_backtrace_info(all) -> (n, frames);
    lvars all, n = 1, frames;

    define lconstant hidden(name);
        lvars name;
        lconstant prefixes =
            ['sys' 'ved' 'wved' 'xved' 'pop_' 'pop11_' 'subsys' 'debugger'];
        if isword(name) then
            lvars prefix;
            fast_for prefix in prefixes do
                returnif(isstartstring(prefix, name))(true)
            endfor;
        elseif isstring(name) then
            lvars l = datalength(name);
            returnif(l fi_> 2 and name(1) == `(` and name(l) == `)`)(true);
        endif;
        false;
    enddefine;

    lvars base_frame = get_base_frame();
    lvars callers = allbutfirst(base_frame, syscallers());
    lvars p;
    [%  fast_for p in callers do
            lvars name = recursive_front(pdprops(p));
            if name and (all or not(hidden(name))) or n == curr_frame_num then
                {%  n == curr_frame_num,
                    n,
                    name,
                    compiled_in_debugging_mode(p) and
                        caller_valof(WID curr_exec_line, n + base_frame)
                %}
            endif;
            n + 1 -> n;
        endfor;
    %] -> frames;
enddefine;

;;; db_get_frame_owner:
;;;     returns a procedure, the owner of the given stack frame.

define db_get_frame_owner(frame_num) -> pdr;
    lvars frame_num, pdr;
    unless caller(CALLER_NUM + (frame_num - curr_frame_num)) ->> pdr then
        db_error(frame_num, 1, 'No such frame');
    endunless;
enddefine;

;;; db_get_frame_info:
;;;     returns a 4 element vector representing the current stack frame:
;;;       (frame number, owner name, lexical bindings, permanent bindings)
;;;     where the bindings are lists of pairs: (name, value)

define db_get_frame_info();

    define lconstant get_vars(vs);
        lvars vs;
        [%  until vs == [] do
                lvars name, id;
                fast_destpair(fast_destpair(vs)) -> (name, id, vs);
                if ispair(name) then fast_front(name) -> name endif;
                unless isactive(name) then
                    ;;; omit active variables for now
                    conspair(name, idval(id));
                endunless;
            enduntil;
        %];
    enddefine;

    lvars p = db_get_frame_owner(curr_frame_num);
    {%  curr_frame_num,
        recursive_front(pdprops(p)),
        if compiled_in_debugging_mode(p) then
            get_vars(lexical_bindings()), get_vars(permanent_bindings(p));
        else
            false, false;
        endif;
    %};
enddefine;

;;; db_set_frame:
;;;     set the current frame number.

define db_set_frame(frame_num);
    lvars frame_num;
    db_get_frame_owner(frame_num) -> ;  ;;; mishaps if no such frame
    frame_num -> curr_frame_num;
enddefine;

;;; db_get_source_info:
;;;     returns a 3 element vector: (file name, procedure name, line number)

define db_get_source_info();
    lvars n = CALLER_NUM, pdbr = caller_valof(WID curr_exec_pdbr, n);
    {%  file_relative_name(pdbr_file(pdbr)),
        pdbr_name(pdbr) or caller(n),
        caller_valof(WID curr_exec_line, n)
    %};
enddefine;

;;; db_get_breakpoint_info:
;;;     returns a list of 4 element vectors:
;;;       (breakpoint identifier, file name, procedure name, line number)

define db_get_breakpoint_info();
    syssort(
        [%  app_all_pdbrs(
                procedure(pdbr);
                    lvars pdbr;
                    lvars file = file_relative_name(pdbr_file(pdbr));
                    lvars name = pdbr_name(pdbr);
                    lvars bps = pdbr_breakpoints(pdbr);
                    until bps == [] do
                        lvars line, id;
                        fast_destpair(fast_destpair(bps)) -> (line, id, bps);
                        {% id, file, name, line %};
                    enduntil;
                endprocedure);
        %],
        false,
        procedure(bp1, bp2);
            lvars bp1, bp2;
            fast_subscrv(1, bp1) fi_< fast_subscrv(1, bp2);
        endprocedure);
enddefine;

;;; db_get_userstack_info:
;;;     returns a vector of items on the user stack.

define db_get_userstack_info(n);
    lvars n, stacklen = stacklength();
    unless n and n <= stacklen then stacklen -> n endunless;
    lvars i, stackvec = initv(n);
    fast_for i to n do
        subscr_stack(i) -> fast_subscrv(i, stackvec);
    endfor;
    stackvec;
enddefine;

;;; db_get_ignoring_info:
;;;     returns a list of pairs: (file name, procedure name)

define db_get_ignoring_info();
    [%  app_all_pdbrs(
            procedure(pdbr);
                lvars pdbr;
                if pdbr_ignore(pdbr) then
                    conspair(pdbr_file(pdbr), pdbr_name(pdbr));
                endif;
            endprocedure);
    %];
enddefine;

;;; db_get_valof:
;;;     Return the value of the named variable in the current environment.

define db_get_valof(name);
    lvars name, id = db_identof(name);
    if id then
        idval(id);
    else
        db_error(name, 1, 'Undeclared identifier');
    endif;
enddefine;


/*
 * The procedures below implement commands which do not display anything,
 * and therefore work for any style of interface.
 */

define db_delete(bp);
    lvars bp;
    app_all_pdbrs(
        procedure(pdbr);
            lvars pdbr;
            if del_breakpoint(bp, pdbr) and bp then
                exitfrom(db_delete);
            endif;
        endprocedure);
enddefine;

define db_ignore(outermost_only, file, path, status);
    lvars outermost_only, file, path, status;
    lvars pdbr;
    unless get_pdbr(path, file) ->> pdbr then
        if islist(path) then last(path) -> path endif;
        db_error(path, 1, 'No such procedure');
    endunless;
    if outermost_only then
        status -> pdbr_ignore(pdbr);
    else
        app_pdbr(pdbr,
            procedure(pdbr);
                lvars pdbr;
                status -> pdbr_ignore(pdbr);
            endprocedure);
    endif;
enddefine;

define db_trace(file, path, enable);
    lvars file, path, enable, pdbr, id, p, props;
    unless get_pdbr(path, file) ->> pdbr then
        if islist(path) then last(path) -> path endif;
        db_error(path, 1, 'Cannot (un)trace non-procedure');
    endunless;
    unless pdbr_ident(pdbr) ->> id then
        if islist(path) then last(path) -> path endif;
        db_error(path, 1, 'Cannot (un)trace local lexical procedure');
    endunless;
    unless isassignable(id) then
        if islist(path) then last(path) -> path endif;
        db_error(path, 1, 'Cannot (un)trace lconstant procedure');
    endunless;
    0 -> popmaxtraceindent;
    idval(id) -> p;
    if enable then
        unless istraced(p) then
            recursive_front(pdprops(p)) -> props;
            systrace(% p, props, "systrace_proc", false %) ->> p -> idval(id);
            props ->> pdprops(p) -> pdprops(updater(p));
        endunless;
    elseif istraced(idval(id)) ->> p then
        p -> idval(id);
    endif;
enddefine;


/*
 * The procedures below implement commands which display information.
 * Consequently, they need to be redefinable.
 */

define vars db_show_backtrace(all);
    lvars all;
    lvars (n, frames) = db_get_backtrace_info(all);
    lvars width = length(n sys_>< nullstring);
    lvars frame;
    fast_for frame in frames do
        lvars (current, number, name, line) = explode(frame);
        db_printf(if current then '> ' else '  ' endif);
        pr_bracketed_number(number, width);
        db_printf(' %p', [^name]);
        if line then db_printf('  (line %p)', [^line]) endif;
        db_printf('\n');
    endfor;
enddefine;

define vars db_show_frame();

    define lconstant print_bindings(bs, kind);
        lvars bs, kind;
        unless bs == [] then
            db_printf('%p local variables:\n', [^kind]);
            lvars b;
            fast_for b in bs do
                db_printf(' %p = %p\n', [% fast_front(b), fast_back(b) %]);
            endfor;
        endunless;
    enddefine;

    lvars (n, p, lbs, pbs) = explode(db_get_frame_info());
    db_printf('Stackframe %p\n', [^n]);
    if p then
        db_printf('Owner: procedure %p\n', [^p]);
    else
        db_printf('Owner: anonymous procedure\n');
    endif;
    if lbs then
        print_bindings(lbs, 'Lexical');
        print_bindings(pbs, 'Dynamic');
    else
        db_printf('(Not compiled in debugging mode, so can\'t show variable bindings)\n');
    endif;
enddefine;

define vars db_show_source();
    lvars (file, , line) = explode(db_get_source_info());
    db_display_sourcefile(file, line);
enddefine;

define vars db_show_where(show_context);
    lvars show_context;
    dlocal curr_frame_num = 1;
    lvars (file, proc, line) = explode(db_get_source_info());
    if show_context then db_display_sourcefile(file, line) endif;
    db_printf('\'%p\' : %p, line %p\n', [^file ^proc ^line]);
enddefine;

define vars db_show_breakpoints();
    lvars bp, bps = db_get_breakpoint_info();
    lvars width = length(length(bps) sys_>< nullstring);
    fast_for bp in bps do
        pr_bracketed_number(bp(1), width);
        db_printf(' \'%p\' : %p, line %p\n', [% bp(2), bp(3), bp(4) %]);
    endfor;
enddefine;

define vars db_show_userstack(n);
    lvars n;
    lvars items = db_get_userstack_info(n);
    length(items) -> n;
    db_printf('User stack has %p items\n', [^n]);
    lvars i, width = length(n sys_>< nullstring);
    for i to n do
        pr_bracketed_number(i, width);
        db_printf(items(i), ' %p\n');
    endfor;
enddefine;

define vars db_show_ignoring();
    lvars item;
    for item in db_get_ignoring_info() do
        db_printf('\'%p\' : %p\n', [% fast_front(item), fast_back(item) %]);
    endfor;
enddefine;

define vars db_show_default();
    lvars item;
    for item in debugger_default_command do
        db_printf(item, '%p ');
    endfor;
    db_printf('\n');
enddefine;

define vars db_show_values(names);
    lvars names, name;
    for name in names do
        db_printf(db_get_valof(name), name, '%p = %p\n');
    endfor;
enddefine;

constant db_command_table;  ;;; forward

define vars db_show_commands();
    db_printf('Command Summary (command names may be abbreviated):\n');
    lvars entry;
    for entry in db_command_table do
        lvars name = entry(1), args = entry(3);
        nextif(args == undef);  ;;; a "hidden" command
        if args then
            unless islist(args) then [^args] -> args endunless;
            lvars arg;
            for arg in args do
                db_printf('\t%p %p\n', [^name ^arg]);
            endfor;
        else
            db_printf('\t%p\n', [^name]);
        endif;
    endfor;
enddefine;


/*
 * The debugger command procedures. They do not need to be redefinable since
 * they read command arguments using redefinable procedures (db_read_X).
 */

lvars
    continue_program = false,
;

constant procedure sys_debugger;  ;;; forward

define db_cmd_:();
    db_compile_line();
enddefine;

define db_cmd_?();
    db_show_commands();
enddefine;

define db_cmd_abort();
    setpop();
enddefine;

define db_cmd_animate();
    true -> animating;
enddefine;

vars db_backtrace_show_all = false;

define db_cmd_backtrace();
    db_show_backtrace(
        not(debugger_X_interface) and db_try_nextreaditem("all")
        or db_backtrace_show_all
    );
enddefine;

define db_cmd_breakpoints();
    db_show_breakpoints();
enddefine;

define db_cmd_continue();
    returnunless(debugging_context());
    false -> stepping;
    true -> continue_program;
enddefine;

define db_cmd_set_default();
    db_read_default_command() -> debugger_default_command;
enddefine;

define db_cmd_show_default();
    db_show_default();
enddefine;

    ;;; This doesn't make sense as part of the X interface
define db_cmd_default();
    if db_nextreaditem() == termin then
        db_cmd_show_default();
    else
        db_cmd_set_default();
    endif;
enddefine;

define db_cmd_delete();
    db_delete(db_read_breakpoint_number());
enddefine;

define db_cmd_down();
    returnunless(debugging_context());
    if curr_frame_num == 1 then
        db_error(0, 'Already at bottom of call stack');
    endif;
    db_set_frame(curr_frame_num - 1);
    db_show_frame();
enddefine;

define db_cmd_frame();
    returnunless(debugging_context());
    db_set_frame(db_read_frame_number());
    db_show_frame();
enddefine;

define db_cmd_help();
    pop11_compile([help debugger]);
enddefine;

define db_cmd_ignoring();
    db_show_ignoring();
enddefine;

define db_cmd_ignore();
    if not(debugger_X_interface) and db_nextreaditem() == termin then
        chain(db_cmd_ignoring);
    endif;
    db_ignore(db_read_ignore_args(), true);
enddefine;

define db_cmd_unignore();
    db_ignore(db_read_ignore_args(), false);
enddefine;

define db_cmd_inspect();
    valof("inspect")(db_get_valof(db_read_path(false)));
enddefine;

define db_cmd_next();
    returnunless(debugging_context());
    if curr_exec_line == pdbr_last_line(curr_exec_pdbr) then
        true -> stepping;
    else
        set_temp_breakpoint(iscaller(sys_debugger) + 1);
        false -> stepping;
    endif;
    true -> continue_program;
enddefine;

define db_cmd_none =
    identfn(%%);
enddefine;

define db_cmd_pop11();
    db_compile_program();
enddefine;

define db_cmd_print();
    db_show_values(db_read_paths());
enddefine;

define db_cmd_quit();
    false ->> stepping -> animating;
    true -> continue_program;
enddefine;

define db_cmd_return();
    returnunless(debugging_context());
    db_compile_line();
    true -> stepping;
    exitfrom(caller(iscaller(sys_debugger) + 1));
enddefine;

define db_cmd_skip();
    returnunless(debugging_context());
    set_temp_breakpoint(iscaller(sys_debugger) + 2);
    false -> stepping;
    true -> continue_program;
enddefine;

define db_cmd_source();
    db_show_source();
enddefine;

define db_cmd_step();
    returnunless(debugging_context());
    true -> stepping;
    true -> continue_program;
enddefine;

define db_cmd_stopat();
    stop_at(db_read_file_and_line());
enddefine;

define db_cmd_stopin();
    stop_at(db_read_proc_spec());
enddefine;

define db_cmd_stop_here();
    if debugging_context() then
        stop_at(pdbr_file(curr_exec_pdbr), curr_exec_line);
    endif;
enddefine;

    ;;; This doesn't make sense as part of the X interface
define db_cmd_stop();
    if db_try_nextreaditem("at") then
        db_cmd_stopat();
    elseif db_try_nextreaditem("in") then
        db_cmd_stopin();
    elseif db_try_nextreaditem("here") then
        db_cmd_stop_here();
    elseif db_nextreaditem() == termin then
        db_cmd_breakpoints();
    else
        db_error(db_readitem(), 1,
            'Expected "in", "at" or "here" after "stop"');
    endif;
enddefine;

define db_cmd_trace();
    db_trace(db_read_proc_spec(), true);
enddefine;

define db_cmd_untrace();
    db_trace(db_read_proc_spec(), false);
enddefine;

define db_cmd_up();
    returnunless(debugging_context());
    db_set_frame(curr_frame_num + 1);
    db_show_frame();
enddefine;

define db_cmd_userstack();
    db_show_userstack(db_read_userstack_depth());
enddefine;

define db_cmd_where();
    returnunless(debugging_context());
    db_show_where(true);
enddefine;


/*
 * The command table
 */

constant
    db_command_table = [
    ;;; name           command procedure   argument description(s)
        {:              ^db_cmd_:           '<POP-11 statement>'    }
        {?              ^db_cmd_?           ^false                  }
        {abort          ^db_cmd_abort       ^false                  }
        {animate        ^db_cmd_animate     ^false                  }
        {backtrace      ^db_cmd_backtrace   '[all]'                 }
        {breakpoints    ^db_cmd_breakpoints ^false                  }
        {continue       ^db_cmd_continue    ^false                  }
        {default        ^db_cmd_default     '<command name>'        }
        {delete         ^db_cmd_delete      ['<breakpoint number>'
                                             'all']                 }
        {down           ^db_cmd_down        ^false                  }
        {frame          ^db_cmd_frame       '[<frame number>]'      }
        {help           ^db_cmd_help        ^false                  }
        {ignore         ^db_cmd_ignore      '[<procedure spec>]'    }
        {inspect        ^db_cmd_inspect     '<identifier>'          }
        {next           ^db_cmd_next        ^false                  }
        {none           ^db_cmd_none        undef                   }
        {pop11          ^db_cmd_pop11       ^false                  }
        {print          ^db_cmd_print       '<identifiers>'         }
        {quit           ^db_cmd_quit        ^false                  }
        {return         ^db_cmd_return      '<expression>'          }
        {skip           ^db_cmd_skip        ^false                  }
        {source         ^db_cmd_source      ^false                  }
        {step           ^db_cmd_step        ^false                  }
        {stop           ^db_cmd_stop        ['at [<file> : ] <line>'
                                             'in <procedure spec>'
                                             'here']                }
        {stopat         ^db_cmd_stopat      undef                   }
        {stopin         ^db_cmd_stopin      undef                   }
        {trace          ^db_cmd_trace       '<procedure spec>'      }
        {unignore       ^db_cmd_unignore    '<procedure spec>'      }
        {untrace        ^db_cmd_untrace     '<procedure spec>'      }
        {up             ^db_cmd_up          ^false                  }
        {userstack      ^db_cmd_userstack   '[<depth>]'             }
        {where          ^db_cmd_where       ^false                  }
    ],
;

;;; db_validate_command:
;;;     validate a command name, returning the corresponding command procedure
;;;     if it's OK, otherwise raise an error.

define db_validate_command(name);
    lvars name;
    unless isword(name) then
        db_error(name, 1, 'Command name (a word) expected');
    endunless;
    lvars entry, n = 0;
    fast_for entry in db_command_table do
        if name = entry(1) then
            return(entry(2));
        elseif isstartstring(name, entry(1)) and entry(3) /== undef then
            entry, n + 1 -> n;
        endif;
    endfor;
    if n == 0 then
        mishap(name, 1, 'No such command');
    elseif n > 1 then
        ;;; replace each entry on the stack with entry(1), the command name
        lvars i;
        for i to n do
            subscr_stack(i)(1) -> subscr_stack(i);
        endfor;
        mishap(n, 'Ambiguous command name abbreviation');
    else
        () -> entry;
        entry(2);
    endif;
enddefine;


/*
 * Top level
 */

define vars debugger_toplevel(reason);
    lvars reason, saved_stacklength = stacklength();
    dlocal db_level = db_level + 1, curr_frame_num = 1;

    define lconstant command_loop();
        dlocal proglist_state =
            proglist_new_state(
                if reason == "script" then cucharin else charin endif);
        repeat
            false -> continue_program;
            dlocal db_command_line = db_read_command_line();
            db_validate_command(db_readitem())();
            quitif(continue_program);
        endrepeat;
    enddefine;

    define dlocal prmishap(msg, culprits);
        lvars msg, culprits;
        sysprmessage(destlist(culprits), msg, 'MISHAP -', 2);
        erasenum(stacklength() - saved_stacklength);
        chainfrom(command_loop, command_loop);
    enddefine;

    define dlocal interrupt();
        db_pr('*** Interrupted ***\n');
        erasenum(stacklength() - saved_stacklength);
        chainfrom(command_loop, command_loop);
    enddefine;

    if reason == "breakpoint" then
        db_printf('[Breakpoint] ');
        db_show_where(true);
    endif;
    command_loop();
enddefine;

/*
 * Main entry point to the debugger
 */

define sys_debugger(pdbr, line, reason);
    lvars pdbr, line, reason;
    if reason == "script" or reason == "interrupt" or not(pdbr) then
        debugger_toplevel(reason);
    else
        ;;; in a debugging context (e.g. called from checkpoint)
        db_display_sourcefile(pdbr_file(pdbr), line);
        if reason == "animating" then
            db_pause();
        else
            debugger_toplevel(reason);
        endif;
    endif;
enddefine;

vars procedure pop_debugger = sys_debugger;  ;;; a user-redefineable hook


/*
 * Execute a script of debugger commands
 */

define global syntax debugger;
    dlocal debugger_default_command = "none";
    sys_debugger(false, false, "script");
    db_try_nextreaditem("enddebugger") -> ;
    ";" :: proglist -> proglist;
enddefine;

global constant syntax enddebugger;


/*
 * Setting a breakpoint on a procedure from outside the debugger
 */

define global macro stopin;
    define lconstant do_stopin();
        dlocal $-debugger$-db_command_line = proglist;
        stop_at(db_read_proc_spec());
        db_command_line -> proglist;
    enddefine;
    [^do_stopin();].dl;
enddefine;

define global ved_stopin();
    dlocal db_command_line = pdtolist(incharitem(stringin(vedargument)));
    if vedargument = nullstring then vederror('Stop where?') endif;
    stop_at(db_read_proc_spec());
enddefine;


/*
 * A version of interrupt which invokes the debugger
 */

define debugger_interrupt();
    dlocal stepping = true;
    if debugger_debugging == true or debugger_debugging == "execute" then
        sys_debugger(false, false, "interrupt");
    else
        setpop();
    endif;
enddefine;

endsection; ;;; $-debugger


/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Oct 29 1993
        Added new commands down and up.
--- Simon Nichols, Oct 14 1993
        Added new command source.
        Replaced db_get_where_info with the more general db_get_source_info.
        Renamed db_display_context to db_display_sourcefile.
--- Simon Nichols, Oct 11 1993
        Rewritten to separate out code which does I/O (i.e. read commands
        and display information) from code which computes and gathers the
        information. In particular, each debugger command is now implemented
        by a separate procedure: if that procedure needs to do I/O, it calls
        redefinable procedures to get it done. The aim is to provide support
        for an X interface with the maximum of code sharing.
        A fair number of minor bugs have been fixed, among them:
        o breakpoints on nested procedures were not being reported by the
          "breakpoints" command
        o displaying a stack frame of a procedure with a dlocal active
          variable resulted in a mishap
        o the "animate" command did not work
--- Simon Nichols, Dec 18 1992
        Introduced new, user assignable procedure variable -debugger_pr-,
        whose default value is -syspr-. Changed -db_pr- and -db_printf- to
        locally assign -debugger_pr- to -pr-. Also changed all calls to
        -db_printf- use field specifier %p rather than %P.
--- Simon Nichols, Jul  6 1992
        Changed -display_context- to dlocal -pop_pr_quotes- <false>, to
        ensure source lines are not printed with quotes outside of VED.
--- Simon Nichols, Jul  1 1992
        Changed -db_frame-/-print_vars- to print names of weakref variables
        correctly.
--- Simon Nichols, Feb 20 1992
        Added "quit" and "inspect" commands.
        Changed the "print" command to take multiple arguments.
        The 3rd arg. to -sys_debugger- and -pop_debugger- is now a word
        indicating the reason why the debugger has been invoked.
        When -debugger_toplevel- is invoked at a breakpoint, it now prints
        a message stating the file and line number at the breakpoint.
        If a file is located in a tree rooted at the current directory, its
        name is now displayed relative to the current directory.
        Combined -ved_display_context- and -tty_display_context- into a
        single procedure -display_context-.
        Changed -db_where- to call -display_context-.
--- Simon Nichols, Oct  9 1991
        Added call of ``sys_clear_input(pop_charin_device)'' after ":" and
        "pop11" commands to clear any newline that might be buffered.
--- Simon Nichols, Oct  2 1991
        Changed db_where to locally set -frame_num- to 1, thereby ensuring
        it always gets the correct line number.
--- Simon Nichols, Sep 27 1991
        Changed -db_print- to use -db_identof-.
        Changed -do_command- to use -set_temp_breakpoint-.
--- Simon Nichols, Sep  9 1991
        Changed accesses to pdprops to use recursive_front(pdprops).
--- Simon Nichols, Jul 18 1991
        "stop" without arguments behaves like "breakpoints".
        "stop here" stops at the current line in the current file.
        "ignore"/"unignore" without arguments display a list of the
        procedures being ignored.
--- Simon Nichols, Jul 15 1991
        Changed -ved_display_context- to locally set -wvedalwaysraise- true.
--- Simon Nichols, Jul  8 1991
        Changed the "ignore" command to ignore nested procedures unless the
        argument "only" is given.
 */
