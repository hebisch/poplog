/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:           C.unix/lib/lib/shell_compile.p
 > Purpose:        Compile by sending characters to a shell subprocess
 > Author:         Roger Evans, Aug 1983 (see revisions)
 > Documentation:  HELP *CSH_COMPILE, *SH_COMPILE, *IMCSH, *IMSH
 > Related Files:  LIB *CSH_COMPILE, *SH_COMPILE
 */
compile_mode :pop11 +strict;

section $-lib => shell_compile;

include ved_declare.ph;
include sigdefs.ph;

lvars read_buffer = false;

lconstant macro (
    VED_LOADED = [testdef vedprocess],
    VED_WEAK = [weakref[vedprocess]],
);

define lconstant kill_shell(proc_id, pid_id);
    lvars proc_id, pid_id;
    if idval(pid_id).isinteger then
        sys_send_signal(idval(pid_id), SIG_KILL) ->
    endif;
    false ->> idval(pid_id) -> idval(proc_id);
enddefine;

define lconstant send_process_proc(prompt_id, fixed_prompt_id, proc_id,
                                    pid_id, set_prompt, contn_prompt,
                                    exec_file, exec_args);
    lvars
        prompt_id, fixed_prompt_id, proc_id, pid_id, set_prompt, contn_prompt,
        exec_file, exec_args,
        pop_in_pipe, pop_out_pipe,      ;;; pop's end of the pipes,
                                        ;;; read & write resp.
        shell_in_pipe, shell_out_pipe    ;;; shell's end of the pipes.
        prompt = idval(fixed_prompt_id), ;;; prompt of shell
        promptlen = datalength(prompt), ;;; length of prompt
        str, nchars, nc1, ncp, input, inputlen, ss1, ss2;

    dlocal VED_WEAK vedlmr_print_in_file;

    define lconstant putpipe(s);
        lvars s;
        unless last(s) == `\n` do
            s <> '\n' -> s
        endunless;
        syswrite(pop_out_pipe, s, s.datalength);
    enddefine;

    define lconstant prstring(s);
        lvars s;
        appdata(s, cucharout)
    enddefine;

    define dlocal interrupt();
        kill_shell(proc_id, pid_id);
        suspend(false,1);
    enddefine;

    syspipe(false) -> pop_in_pipe -> shell_out_pipe;
    syspipe(false) -> shell_in_pipe -> pop_out_pipe;
    unless isstring(read_buffer) then inits(512) -> read_buffer endunless;

    if VED_LOADED and VED_WEAK vedediting then
        VED_WEAK vedputmessage('PLEASE WAIT')
    endif;

    if sys_fork(false) ->> idval(pid_id) then
        /*   read the initial output until a prompt, then
         *   suspend and wait for inputs from VED
         */
        sysclose(shell_in_pipe);
        sysclose(shell_out_pipe);
        ;;; ensure that prompt is constant
        putpipe(set_prompt <> '"' <> prompt <> '"\n');
        nullstring -> input;
        ;;; read in stuff till prompt
        repeat
            sysread(pop_in_pipe, read_buffer, 512) -> nchars;
            input <> substring(1,nchars,read_buffer) -> input;
            quitif(issubstring(prompt,input))
        endrepeat;
        suspend(true,1);

        repeat
            () -> str;
            /* Send input from ved */
            putpipe(str);

            /* Now read any output produced and return it */

           /*
            * This bit (believe it or not!) forces output into the current
            * file (locally) if its been diverted globally to another file.
            */
            if VED_LOADED and isstring(VED_WEAK vedlmr_print_in_file) then
                VED_WEAK vedcurrent -> VED_WEAK vedlmr_print_in_file;
            endif;

            nullstring -> input;
            repeat
                sysread(pop_in_pipe, read_buffer, 512) -> nchars;
                input <> substring(1,nchars, read_buffer) -> input;

               /*
                * shell_send uses the shell prompt to detect when the shell wants
                * some more input.  It is therefore not suitable for use with
                * interactive programs.
                */
                datalength(input) -> inputlen;
                if inputlen fi_>= promptlen
                and issubstring(prompt,
                        datalength(input) fi_- promptlen fi_+ 1 ->> ncp,
                        input) == ncp
                then
                    prompt -> idval(prompt_id);
                    prstring(substring(1,ncp fi_- 1, input));
                    quitloop;
                elseif inputlen >= 2
                and issubstring(contn_prompt,
                        datalength(input) fi_- 1 ->> nc1, input) == nc1
                then
                    contn_prompt -> idval(prompt_id);
                    prstring(substring(1,nc1 fi_- 1, input));
                    quitloop;
                elseif inputlen > promptlen then
                    inputlen fi_- promptlen -> inputlen;
                    prstring(substring(1, inputlen, input));
                    substring(inputlen fi_+ 1, promptlen, input) -> input;
                endif;
            endrepeat;
            suspend(true,1);
        endrepeat;
    else
        /* child: reroute io and exec shell */
        shell_in_pipe -> popdevin;
        shell_out_pipe -> popdevout;
        shell_out_pipe -> popdeverr;
        sysexecute(exec_file, exec_args, false);
    endif;
    sysclose(pop_in_pipe);
    sysclose(pop_out_pipe);
    false -> idval(proc_id);
enddefine;

define shell_send(line, prompt_id, fixed_prompt_id, proc_id, pid_id,
                        set_prompt, contn_prompt, exec_file, exec_args);
    lvars   line, prompt_id, fixed_prompt_id, proc_id, pid_id,
            set_prompt, contn_prompt, exec_file, exec_args;
    /* start pop/shell process if necessary and send string to it */
    if line == false then
        kill_shell(proc_id, pid_id);
        return;
    endif;
    check_string(line);
    unless idval(proc_id).isliveprocess then
        consproc( prompt_id, fixed_prompt_id, proc_id, pid_id, set_prompt,
                    contn_prompt, exec_file, exec_args, 8, send_process_proc)
                            -> idval(proc_id);
        returnunless(runproc(0, idval(proc_id)))
    endunless;
    runproc(line, 1, idval(proc_id)) -> ;
enddefine;

;;; N.B. This is a subsystem compiler, so must set cucharin dlocally
define shell_compile(cucharin, send_line);
    lvars c, prompt_id = frozval(1,send_line), send_line;
    dlocal cucharin, popprompt, poplinewidth = false;
   /*
    * Get chars from repeater, and build string for each line
    * (terminated by linefeed); send lines to shell process.
    */
    send_line('\n');
    if VED_LOADED and VED_WEAK vedediting then
        nullstring -> VED_WEAK vedmessage
    endif;
    repeat
        idval(prompt_id) -> popprompt;
        send_line(
            consstring(#|
                repeat
                    cucharin() -> c;
                    quitif(c == termin)(2);
                    c;
                    quitif(c == `\n`);
                endrepeat
            |#))
    endrepeat;
    send_line(%false%) <> popexit -> popexit;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 22 1996
        Changed section to $-lib
--- John Gibson, Apr 21 1994
        Uses new sys_fork
--- John Gibson, Dec  1 1992
        Created this library from code in csh_compile & sh_compile
 */
