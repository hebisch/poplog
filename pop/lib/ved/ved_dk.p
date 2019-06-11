/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_dk.p
 >  Purpose:        define a key for ved. (uses vedsetkey)
 >  Author:         A.Sloman 1982 (see revisions)
 >  Documentation:  HELP * DK
 >  Related Files:  HELP VEDSET, LIB * VED_DM
 */
compile_mode :pop11 +strict;

;;; To make a certain key, or sequence of keys, cause a certain effect,
;;; type ENTER DK, then follow instructions.
;;; The file VEDINIT.P will be automatically extended to include your
;;; new command, unless you say you don't want it to be permanent.

;;; If you want to map the function key onto a named VED facility, give
;;; the logical name of the facility as the argument to "ENTER dk",
;;; e.g. "ENTER dk linedelete" or "ENTER dk enter m"
;;; "ENTER dk ved foo". etc.
;;; See HELP LOGICAL_KEYS

section;

define vars ved_dk;
    lvars   vedfile, vedflag, keyname, original_vederror = vederror,
            dk_error = false;

    ;;; TO CATCH MISHAPS FROM -vedsetkey-
    define dlocal prmishap(string, culprits);
        lvars string, culprits;
        lvars culprits_string = nullstring;

        unless culprits == [] then
            allbutfirst(1, allbutlast(1, culprits sys_>< nullstring)) sys_>< ': '
                -> culprits_string;
        endunless;

        culprits_string sys_>< string -> dk_error;

    enddefine;
    ;;;
    define dlocal vederror(string);
        lvars string;
        unless iscaller(ved_lmr) do;
            original_vederror(string);
        endunless;
    enddefine;

    ;;; see if user has specified a keyname
    if vedargument = nullstring then
        false
    elseif vedargument = 'return' or vedargument = 'RETURN' then
        'veddocr'
    elseif isstartstring('ved ', vedargument) then
        ;;; Treat it as a definition of an 'ENTER' command
        'enter ' sys_>< vedargument -> vedargument;
        chain(ved_dk);
    elseif isstartstring('ved', vedargument) then
        vedargument
    elseif isstartstring('enter ', vedargument)
    or isstartstring('ENTER ', vedargument)
    then
        allbutfirst(6, vedargument) -> keyname;
        ;;; if it contains a space it is a VED command with an
        ;;; argument, so partially apply veddo
        if strmember(`\s`, keyname) then
            vedinsertstring(%'veddo(%\'' sys_>< keyname sys_>< '\'%)' %)
        else
            ;;; no argument, just invoke ved_<name>
            'ved_' sys_>< keyname
        endif
    elseif strmember(`\s`, vedargument) then
        ;;; Treat it as a definition of an 'ENTER' command
        'enter ' sys_>< vedargument -> vedargument;
        chain(ved_dk)
    else
        'ved' sys_>< vedargument
    endif -> keyname;

    if keyname then
        ;;; check that it's a valid name
        unless isprocedure(keyname)
        or isprocedure(valof(consword(keyname))) then
            vederror('NO VED PROCEDURE CALLED: ' sys_>< keyname)
        endunless;
    endif;

    vedputmessage('WANT IT TO BE PERMANENT? y/n');
    vedscreenbell();
    vedinascii() -> vedfile;

    if vedfile == `y` or vedfile == `Y` then
        true -> vedflag;
        if readable('$poplib/vedinit.p') or not(readable('vedinit.p')) then
            sysfileok('$poplib/vedinit.p')
        else
            'vedinit.p'
        endif
    else
        false -> vedflag;
        sysfileok('dk' sys_>< poppid)
    endif -> vedfile;

    vedsetonscreen(vedopen(vedfile),
        'DEFINING KEY ' sys_>< if vedflag then 'IN 'sys_>< vedfile else 'NOW' endif);

    vedendfile();
    false -> vedbreak;
    vedlinebelow(); vedmarklo();
    vedinsertstring(';;; \{b}Inserted by <ENTER> dk\n');
    vedinsertstring('define :ved_runtime_action;\n\tvedsetkey(\'');

    ;;; read in codes for the key(s)
    lvars start_column = vedcolumn;
    vedinkeys('PRESS KEY(S) THEN PRESS <ESC> THREE TIMES');

    ;;; CHECK THAT THE USER TYPED SOMETHING
    if vedcolumn == start_column do;
        false -> vedchanged;  ved_rrq();
        vederror('CANNOT DEFINE -- NO KEY(S) SUPPLIED');
    endif;

    ;;; Finish the call of vedinput and finish procedure definition.
    if keyname then
        ;;; User specified required function
        vedinsertstring('\', ');
        if isprocedure(keyname) then keyname()
        else
            vedinsertstring(keyname)
        endif;
    else
        vedinsertstring('\', \'');
        lvars start_column = vedcolumn;
        vedinkeys('TYPE WHAT YOU WANT KEY TO DO, THEN <ESC> <ESC> <ESC>');
        ;;; CHECK THAT THE USER TYPED SOMETHING
        if vedcolumn == start_column do;
            false -> vedchanged; ved_rrq();
            vederror('CANNOT DEFINE -- NO FUNCTION SUPPLIED');
        endif;
        vedcharinsert(`'`);
    endif;
    vedinsertstring(');\nenddefine;\n');

    ;;; load the procedure
    ved_lmr();
    ;;; return to previous file
    if vedflag then
        ved_wq()
    else
        false -> vedchanged; ved_rrq();
    endif;
    if dk_error then vederror(dk_error) endif;
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Gibson, Apr 21 1994
        Made it generate Ved runtime action procedures
--- Adrian Howard, Sep  9 1992
        Cleaned up handling of errors.
--- John Gibson, Jun  8 1991
        Uses vedusewindows == "pw*m"
--- Aaron Sloman, Dec 16 1990
    Extended to allow 'dk ' instead of 'dk enter ' in commands like
        ENTER dk ved ....
--- Aaron Sloman, Dec 14 1990
    Undid minor "bug" fix at end.
--- Aaron Sloman, Dec 13 1990
    Made it cope with enter commands with arguments, e.g.
        ENTER dk enter ved foo
--- Aaron Sloman, Dec 13 1990
    Modified to allow argument specifiying which  VED function to use.
    Prevented it writing temporary file.
--- Aaron Sloman, Apr  4 1989
    Made vedbreak false, to fix bug reported by Jason Handby.
--- Aaron Sloman, Feb  5 1989
    Added call of -sysfileok- to expand $poplib/vedinit.p
 */
