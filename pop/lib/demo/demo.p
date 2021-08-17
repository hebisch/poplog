;;; --- Copyright University of Sussex 1986.  All rights reserved. ---------
;;; File:           C.all/lib/demo/demo.p
;;; Purpose:        Run Demonstration programs
;;; Author:         Aaron Sloman (DCL version 1982) Tom Khabaza POP version (see revisions)
;;; Documentation:  C.all/lib/demo/*.lis
;;; Related Files:


/* DEMO procedure runs poplog demos in pop/lib/demo */

lconstant VMS = (hd(sys_os_type) == "vms");

lconstant TERM = (if VMS then 'TT' else '/dev/tty' endif);

define lconstant Run_image(file);
    lvars item file string;

    define lconstant File_trans(file);
        lvars file;
        (if VMS then ' /' else ' -' endif) <> file
    enddefine;

    'pop11' -> string;
    if isstring(file) then
        string <> File_trans(file) -> string
    else
        until null(file) do
            string <> File_trans(fast_destpair(file) -> file) -> string
        enduntil
    endif;
    sysobey(string)
enddefine;

define lconstant Run_file(file);
    lvars file;
    sysobey('pop11 ' <> file)
enddefine;


vars procedure (options demo);

define op_int;
    sysflush(popdevout);
    clearstack();
    chainto(demo, options);
enddefine;

define quit_int;
    clearstack();
    exitto(demo);
enddefine;

define query_int;
    vars input interrupt;
    quit_int -> interrupt;
    clearstack();
    pr('\n\nDo you want to finish?\n    Type yes or no and press RETURN: ');
    sysflush(popdevout);
    readline() -> input;
    if input matches [== no ==]
    or input matches [== NO ==]
    or input matches [== No ==]
    or input matches [== n ==]
    or input matches [== N ==]
    then
        chainto(demo, options);
    else
        exitto(demo);
    endif;
enddefine;

define silent_pause();
    vars popprompt; '' -> popprompt;
    repeat quitif(charin()==`\n`); endrepeat;
enddefine;

define pause();
    pr('PRESS THE RETURN KEY TO CONTINUE...');
    silent_pause();
enddefine;

define qtype(file);
    lvars inchar file;
    vars interrupt poplinewidth poplinemax;
    80 ->> poplinewidth -> poplinemax;

    op_int -> interrupt;

    discin(sysfileok(file)) -> file;

    until (file() ->> inchar) == termin do
        charout(inchar);
    enduntil;
    sysflush(popdevout);
;;; syswaitout();
;;; syssleep(200);  ;;; wait 2 seconds - buffers prob. empty by then
enddefine;

define type(file);
    pr('\n\n--------------------------\n');
    qtype(file);
    pause();
enddefine;


vars pop_longstrings;
true -> pop_longstrings;

define test_keys;
    vars count interrupt;
    op_int -> interrupt;
    0 -> count;

pr('\n
    Welcome to the Sussex University POPLOG system.

    This is a demonstration program for beginners.

    First you need to learn some very basic procedures.

    There is a big key marked \'RETURN\' near the right of the keyboard.
    You will need to use it a lot.

    Please press it once for practice, then let go immediately.
');

    silent_pause();
pr('
    One more for practice - don\'t hold it down: just a light tap
');
    silent_pause();

pr('
    Good.
    It is important to know how to interrupt things.
    You do this by holding down the key marked CTRL (on left),
    and then pressing the C key once (don\'t hold it down). Try that
');

    pr(%'\nGood\n'%)<>op_int -> interrupt;

    until count >= 3 do
        silent_pause();
pr('
    Not quite try again: Hold down CTRL and press the C key once
');
        count + 1 -> count;
    enduntil;
    op_int();
enddefine;

define explain;
    type('$usepop/pop/lib/demo/explain.lis');
    type('$usepop/pop/lib/demo/explain1.lis');
    type('$usepop/pop/lib/demo/explain2.lis');
    type('$usepop/pop/lib/demo/explain3.lis');
enddefine;



define bye();
    pr('\n\n\nBYE hope you have enjoyed yourself.\
    \nSuggestions for improvement welcome\
    \nType "demo" and press RETURN to continue\n\n');
enddefine;

define options;
    vars reply interrupt;

    repeat
        op_int -> interrupt;

        ;;; print menu file
        qtype('$usepop/pop/lib/demo/options.lis');

        query_int -> interrupt;
        pr('\nPlease indicate your choice and then press the RETURN key: ');
        sysflush(popdevout);
        readline() -> reply;
        if null(reply) then
            nextloop;
        else
            hd(reply) -> reply;
        endif;

        op_int -> interrupt;

        if reply = 0 then
            explain()
        elseif reply = 1 then
            Run_file('ttt')
        elseif reply = 2 then
            type('$usepop/pop/lib/demo/eliza1.lis');
            Run_image('eliza');
            type('$usepop/pop/lib/demo/eliza2.lis')
        elseif reply = 3 then
            type('$usepop/pop/lib/demo/stroppy.lis');
            Run_image('stroppy')
        elseif reply = 4 then
            Run_image('hangman')
        elseif reply = 5 then
            Run_file('startved')
        elseif reply = 6 then
            Run_image('kitchen')
        elseif reply = 7 then
            Run_file('teachpop')
        elseif reply = 8 then
            Run_image('msblocks')
        elseif reply = 9 then
            Run_image(['prolog' 'logic'])
        elseif reply = 10 then
            Run_image('strips')
        elseif reply = "X" or reply = "x" then
            return
        endif
    endrepeat
enddefine;


define demo();
    ;;; make sure input comes from terminal
    vars proglist;
    define prmishap();
        pr('\nsorry something has gone wrong\n');
        sysexit();
    enddefine;
    pdtolist(incharitem(discin(TERM))) -> proglist;
    test_keys();  ;;; when ^c happens in test-keys, we'll go to options
    bye();
enddefine;

;;; --- Revision History ---------------------------------------------------
;;; --- John Williams, Jun  7 1988 - Fixed SFR 4192 (VMS compatibility)
