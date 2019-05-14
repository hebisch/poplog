/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.unix/lib/lib/ved_vsh.p
 > Purpose:        SHELL-IN-VED-WINDOW package
 > Author:         Roger Evans, Dec 10 1986 (see revisions)
 > Documentation:  HELP *VSH
 > Related Files:  LIB *PTYFORK, LIB *VEDMUX, LIB *MUX, LIB *CSH_FILE
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +oldvar;

/*  LIB VED_VSH          R.Evans November 1986

    UNSUPPORTED -- SEE DISCLAIMER IN HELP FILE

    VSH is a more advanced version of CSH_FILE, allowing a ved buffer
    to appear like a shell window
    See HELP VSH.
*/

uses vedmux    ;;; routines for multiplexing input with VED
uses ptyfork   ;;; pseudo-tty fork interface

section $-library => ved_vsh vedvshdefaults
                     ved_vctrl ved_vintr ved_veof ved_vsusp
                     ved_vquit ved_vend;

/* user definable procedure for vsh file defaults */
define global vedvshdefaults;
    false -> vedwriteable;
    false -> vedbreak;
    8 -> vedindentstep;
enddefine;

/* general VSH io buffer */
lvars vsh_buff = inits(512);

/*  a VSH child record is a 4-element vector consisting of:
        the pty device (io) to talk to the VSH process
        the vedbuffer for the VSH window
        the last line output (into the ved buffer) - assumed to contain
            prompt (if any): ie to be ignored if found on the next line
            of input
        a device opened on the slave tty
*/
vars child ved_vintr;

/* tidy up when VSH gets killed */
define vsh_terminate(child);
    sysclose(child(4));
    false -> mux_entry(child(1));
    sysclose(child(1));
enddefine;

/* Handler for output from VSH process. This is put into the
   multiplexed input table (see MUX), and called whenever VSH sends output to
   poplog */
define vsh_output(child);
    vars child;  ;;; vsh_timeout uses dynamic binding
    lvars n m s d old wasonstatus;
    define interrupt;
        ved_vintr();    ;;; interrupt vsh, then just come back
    enddefine;
    /* switch to VSH window */
    vedcurrentfile -> old;
    vedselect(child(2));
    vedinitfile();
    if vedonstatus ->> wasonstatus then
        vedstatusswitch();
    endif;
    /* fetch all the available input and dump it into ved buffer */
    child(1) -> d;
    repeat;
        sysread(d,vsh_buff,512) -> n;
        if n == 0 then
            /* end of file - probably never see this */
            vsh_terminate(child);
        else
            substring(1,n,vsh_buff) -> s;
            /* could do more terminal emulation functions here ... */
            /* convert <CR> into <SPACE> - <LF> is used for line feeds */
            1 -> m;
            while locchar(`\r`,m,s) ->> m do ` ` -> s(m) endwhile;
            vedinsertstring(s);
        endif;
        quitif(n < 512); ;;; no more data available
    endrepeat;
    vedtrimline();
    /* save last line of output */
    copy(vedthisline()) -> child(3);
    if wasonstatus then
        vedstatusswitch();
    endif;
    /* go back to previous window */
    vedselect(old);
    vedinitfile();
    vedcheck();
    vedsetcursor();
    sysflush(poprawdevout);
enddefine;

/* handle input from ved to VSH process - this procedure is the
   'popcompiler' for the VSH window.
   Input is passed straight through to the VSH process, except for
   a prompt string if detected. Don't wait for output, since its handled
   asynchronously. */
define vsh_input(repeater,child);
    lvars repeater child c d last;
    /* check for saved prompt string */
    if iscaller(ved_lmr) and vvedmarkhi == vvedmarklo then
        child(3) -> last;
        if issubstring_lim(last,1,1,false,vedbuffer(vedline)) then
            /* throw away prompt characters */
            repeat datalength(last) times repeater() -> endrepeat;
        endif;
    endif;
    child(1) -> d;  ;;; fetch out device for talking to VSH
    repeater() -> c;
    if c == termin then
        /* no characters - just send a newline */
        `\n` -> vsh_buff(1);
        syswrite(d, vsh_buff, 1);
    else
        /* this loop could be optimised ... */
        repeat;
            c -> vsh_buff(1);
            syswrite(d,vsh_buff,1);
            repeater() -> c;
            quitif(c == termin);
        endrepeat;
    endif;
    sysflush(d);
    /* go to end of file - ready for output */
    vedendfile();
enddefine;

/* start up VSH - run once in context of a new vsh buffer */
define vsh_startup(file,args);
    lvars n d c file args;
    /* create child process using ptyfork */
    if ptyfork(file,args,false) ->> n then
        -> d;
    else
        vederror('Can\'t make VSH subprocess');
    endif;
    /* kick it to avoid input block */
    ` ` -> vsh_buff(1);
    syswrite(d,vsh_buff,1);
    sysflush(d);
    /* create vsh child record - open the device on slave tty (n) to stop
       POPLOG hangup if you send 'eof' to VSH subprocess */
    {% d; vedcurrentfile,'',sysopen(n,0,false)%} -> c;
    /* set up compiler and output reader */
    vsh_input(%c%) -> popcompiler;
    vsh_output(%c%) -> mux_entry(d);
    /* turn on input multiplexing if necessary */
    ved_mux();
enddefine;

/* cut up vedargument into a list of strings */
define vsh_parsearg(s);
    lvars s i j;
    [%  unless s = vednullstring then
            1 -> i;
            while locchar(` `,i,s) ->> j do
                substring(i,j-i,s);
                j+1 -> i;
            endwhile;
            substring(i,datalength(s)-i+1,s);
        endunless;
    %]
enddefine;

/* main VSH command */
define global ved_vsh;
    lvars f l;
    vsh_parsearg(vedargument) -> vedargument;
    if vedargument == [] then
        unless systranslate('SHELL') ->> f then
            '/bin/csh' -> f;
        endunless;
        [^f] -> l;
    else
        /* explicit process args given on command line */
        hd(vedargument) -> f;
        vedargument -> l;
    endif;
    /* put startup procedure into ved's input stream */
    vsh_startup(%f,l%) :: ved_char_in_stream -> ved_char_in_stream;
    vededitor(vedvshdefaults, gensym("vsh") >< '');
enddefine;

/* now a few control utilities */

/* stop VSH tidily */
define global ved_vend;
    if pdpart(popcompiler) == vsh_input then
        vsh_terminate(frozval(1,popcompiler));
        compile -> popcompiler;
        vedputmessage('VSH process killed');
    endif;
enddefine;

/* send a control char to VSH - code for char is argument */
define vshctrl(c);
    lvars c d;
    if pdpart(popcompiler) == vsh_input then
        frozval(1,popcompiler)(1) -> d;
        unless isinteger(c) and c fi_< 255 then
            vederror('invalid control char: ' >< c);
        endunless;
        c -> subscrs(1,vsh_buff);
        sysflush(d);
        syswrite(d,vsh_buff,1);
        sysflush(d);
    else
        vederror('not a VSH window');
    endif;
enddefine;

/* send arbitrary control characters to VSH */
define global ved_vctrl;
    applist(compile(stringin('['><vedargument><']')),vshctrl);
enddefine;

/* the next few use the interrupt chars provided in the PTYFORK
   terminal structures */

/* send interrupt to VSH */
define global ved_vintr;
    vshctrl(pty_tchars(1));
enddefine;

/* send suspend to VSH */
define global ved_vsusp;
    vshctrl(pty_ltchars(1));
enddefine;

/* send quit to VSH */
define global ved_vquit;
    vshctrl(pty_tchars(2));
enddefine;

/* send eof to VSH */
define global ved_veof;
    vshctrl(pty_tchars(5));
enddefine;

/* send a space to VSH */
define global ved_vsp;
    vshctrl(32);
enddefine;

/* send a CR to VSH */
define global ved_vcr;
    vshctrl(13);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 29 1992
        Moved to C.unix
--- Aaron Sloman, Oct 24 1988
    Put in public non-autoloadable library
--- Roger Evans, Apr  9 1988 added ved_vsp and ved_vcr
--- Roger Evans, Dec 11 1986 modified so that if you lmr a blank line a
    newline is sent (instead of nothing at all)
*/
