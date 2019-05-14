/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lib/stripvedfile.p
 > Purpose:         Strip attribute control chars etc from a VED file
 > Author:          John Gibson, Mar 16 1992 (see revisions)
 > Documentation:   Self-documenting
 */

#_TERMIN_IF not(pop_runtime)

;;; USAGE:
;;;
;;;     pop11 stripvedfile [ -o <outfile> ] <file>
;;;
;;; or (more efficiently),
;;;
;;;     basepop11 %noinit $popliblib/stripvedfile [ -o <outfile> ] <file>
;;;
;;; Reads VED file <file> and outputs it as plain text to standard output
;;; (or <outfile> if -o supplied).

procedure();
    lvars
        infile  = poparglist(1),
        outfile = popdevout,
        procedure (repeater, consumer)
        ;
    dlocal vednotabs = false, vedindentstep = 1;

    if infile = '-o' then
        poparglist(2) -> outfile;
        poparglist(3) -> infile;
    endif;

    vedfile_line_repeater(infile) -> repeater;
    vedfile_line_consumer(outfile, 1) -> consumer;  ;;; 1 = trans to plain ASCII

    until consumer(dup(repeater())) == termin do enduntil;
endprocedure();


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF
--- John Gibson, Oct  5 1992
        Put inside a procedure with vednotabs false etc.
 */
