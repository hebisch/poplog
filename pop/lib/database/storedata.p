/* --- The University of Birmingham 1994.  --------------------------------
 > Author:          Aaron Sloman, Dec  3 1994
 >  File:           $poplocal/local/auto/storedata.p
 >  Purpose:        storing database in a file
 >  Author:         Aaron Sloman
 >  Documentation:  HELP * storedata
 */


/*
This replaces LIB STOREDATA located in
    $usepop/pop/lib/database/storedata.p
which can cause compilation problems when the saved database is very
large.
=======================================================================

HELP STOREDATA                                     Aaron Sloman Dec 1994
LIB STOREDATA

storedata(<filename>)

                    SAVING A DATABASE IN A DISC FILE

If you wish to save the current state of your database (see the DATABASE
demo) in a file, then you will find the procedure STOREDATA useful.

It takes the name of a disc file as argument. For example, if you wish to
store the current database in a file called 'mydata.p', type:

    storedata("mydata");

or, equivalently:

    storedata('mydata.p');

later, e.g. in another programming session, you can restore the
contents of the database by typing:

    load mydata.p;

This can be used for writing programs which learn by interacting with
the user.  What they learn in one session can be stored in a file and
used in the next session.

*/

section;

;;; some text to go at the top of the file.
lconstant headerstring =
';;; FILE CREATED BY STOREDATA\
\
;;; Instruction to read in rest of file and create database\
[% until null(proglist) do\
     if hd(proglist) == "[" then listread()\
     else readitem()\
     endif\
    enduntil\
%] -> database;\
\
'
;


define storedata(filename);
    lvars filename, item;

    dlocal
        pop_pr_quotes = false,
        cucharout = discout(filename);

    ;;; Put code at top of file to read in rest of file
    pr(headerstring);

    ;;; Ensure strings are printed with quotes
    true -> pop_pr_quotes;
    ;;; print each item in the database separately, each starting on
    ;;; a new line
    for item in database do
        pr(item);       
        cucharout(`\n`);
    endfor;

    cucharout(termin);

enddefine;

endsection;
