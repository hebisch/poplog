/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.all/plog/src/io.p
 > Purpose:         Prolog: core I/O procedures.
 > Author:          Robert Duncan and Simon Nichols, Jan 1987. (see revisions)
 */

section prolog;

constant
    procedure ( prolog_new_proglist, ),
;

;;; =======================================================================


vars
    current_input = false,      ;;; The name of the current input stream
    current_output = false,     ;;; The name of the current output stream
    compiler_input = false,     ;;; The name of the current compiler stream
    procedure compilerin,       ;;; ... and its value.

    prolog_read_prompt = '|: ',
;


;;; Check_stream:
;;;     check for a valid stream name

define lconstant Check_stream(name) with_props false;
    lvars name, fname;
    unless isword(name)
    and (sysfileok(fast_word_string(name)) ->> fname)
    then
        mishap(name, 1, 'STREAM NAME NEEDED');
    endunless;
    consword(fname);
enddefine;

;;; input_files, output_files:
;;;     associate file names with streams

define lconstant prolog_stream(/* name, */ table) with_nargs 2;
    lvars table;
    table(Check_stream(/* name */));
enddefine;
;;;
define updaterof prolog_stream(/* stream, name, */ table) with_nargs 3;
    lvars table;
    /* stream */ -> table(Check_stream(/* name */));
enddefine;

define input_files =
    prolog_stream(% newassoc([]) %);
enddefine;

define output_files =
    prolog_stream(% newassoc([]) %);
enddefine;


;;; is_special_input:
;;;     returns a character repeater for the special input streams

define is_special_input(item);
    lvars item;
    if item == "user" then
        charin;
    elseif item == "inchan" then
        compilerin;
    elseif item == "errchan" then
        mishap("errchan", 1, 'CAN\'T see THE ERROR STREAM');
    elseif isprocedure(item) then
        item;
    elseif isstring(item) or isdevice(item) then
        discin(item);
    else
        false;
    endif;
enddefine;

;;; see(stream):
;;;     makes -stream- the current input.
;;;     -stream- must be either a procedure (a character repeater) or a word
;;;     (a stream name).

define see(stream);
    lvars stream, char_rep;
    unless is_special_input(stream) or input_files(stream) ->> char_rep then
        ;;; input_files has checked that stream is a word
        discin(word_string(stream)) ->> char_rep -> input_files(stream);
    endunless;
    char_rep -> cucharin;
    prolog_new_proglist(cucharin) -> proglist;
    stream -> current_input;
enddefine;

;;; closein(stream):
;;;     "closes" the named stream by removing it from the list of open files,
;;;     so that the next "see" on the file will open it afresh.
;;;     If -stream- is the current input stream, input is switched to "user".
;;;     It's impossible to close either the user stream or the compiler
;;;     stream (even if the compiler stream has a name other than "inchan"),
;;;     although the compiler stream will simulate closure by reverting to
;;;     "user".

define closein(stream);
    lvars stream;
    unless stream == "user" then
        unless stream == "inchan" or isprocedure(stream) then
            false -> input_files(stream);
        endunless;
        if stream == compiler_input then "inchan" -> compiler_input endif;
        if stream == current_input then see("user") endif;
    endunless;
enddefine;

;;; is_special_output:
;;;     returns a character consumer for the special output streams

define is_special_output(item);
    lvars item;
    if item == "user" then
        charout;
    elseif item == "inchan" then
        mishap("inchan", 1, 'CAN\'T tell THE COMPILER STREAM');
    elseif item == "errchan" then
        cucharerr;
    elseif isprocedure(item) then
        item;
    elseif isstring(item) or isdevice(item) then
        discout(item);
    else
        false;
    endif;
enddefine;

;;; tell(stream) :
;;;     makes -stream- the current output.
;;;     -stream- must be either a procedure (a character consumer) or a word
;;;     (a stream name).

define tell(stream);
    lvars stream, char_con;
    unless is_special_output(stream) or output_files(stream) ->> char_con then
        ;;; output_files has checked that stream is a word
        discout(word_string(stream)) ->> char_con -> output_files(stream);
    endunless;
    char_con -> cucharout;
    stream -> current_output;
enddefine;

;;; closeout(stream):
;;;     "closes" the named stream by removing it from the list of open files,
;;;     so that the next "tell" on the file will open it afresh.
;;;     If -stream- is the current output stream, output switches to "user".
;;;     It's impossible to close the user stream or the error stream,
;;;     although the error stream will simulate closure by reverting to
;;;     "user".

define closeout(stream);
    lvars stream, charcon;
    unless stream == "user" then
        unless stream == "errchan" then
            if isprocedure(stream) then
                stream(termin);
            elseif output_files(stream) ->> charcon then
                charcon(termin);
                false -> output_files(stream);
            endif;
        endunless;
        if stream == current_output then tell("user") endif;
    endunless;
enddefine;


;;; prolog_rename :
;;;     renames or deletes a file (simulating DEC10 "rename/2").

define prolog_rename(old, new);
    lvars old, new;

    define lconstant Check_file(name) with_props false;
        lvars name, msg;
        if name == "user" then
            'USER'
        elseif name == "errchan" then
            'ERROR'
        elseif name == "inchan" then
            'COMPILER'
        else
            false
        endif -> msg;
        if msg then
            mishap(name, 1, 'CAN\'T RENAME THE ' <> msg <> ' STREAM');
        endif;
        Check_stream(name) -> ;
    enddefine;

    Check_file(old);
    closein(old); closeout(old);
    if new == nil then
        sysdelete(fast_word_string(old)) -> ;
    else
        Check_file(new);
        closein(new); closeout(new);
        sys_file_move(fast_word_string(old), fast_word_string(new));
    endif;
enddefine;


;;; prolog_readline:
;;;     identical to POP -readline-, but sets the prompt first and supresses
;;;     the shell-escape mechanism.

define prolog_readline(prompt);
    lvars   prompt;
    dlocal  pop_readline_prompt = prompt sys_>< nullstring,
;;;         proglist = prolog_new_proglist(cucharin),
            poplastchar = 0;
    readline();
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        Added is_special_input/is_special_output. Moved prolog_c*ucharin to
        "iopreds.p" and prolog_s*afereadline to "obsolete.p".
--- Rob Duncan, Aug  8 1989
        Sectionised and added #_INCLUDEs for POPC; moved in
        -prolog_read_prompt- from "parse.p".
--- Rob Duncan, Aug 31 1988
        Added -Check_stream-; introduced calls to -fast_word_string- to cut
        down copying of stream names; made -prolog_rename- use -sysdelete-
        and -sys_file_move-; replaced -vednullstring- with -nullstring-.
 */
