/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/auto/synonym.p
 *  Purpose:        for defining synonym as macros.  Superseded by SYSSYNONYM
 *  Author:         Aaron Sloman, 1982
 *  Documentation:  HELP * SYNONYM, * SYSSYNONYM
 *  Related Files:
 */

;;; SYNONYM PROC PROCEDURE will make "PROC" a macro identifier, which
;;; when encountered by the compiler will replace itself with the word
;;; "PROCEDURE".

section $-library => synonym;

define global macro synonym;
    lvars x y;
    readitem() -> x; readitem() -> y;
    dl([section $-library => ^ x ;
        global vars macro ^ x ;    ;;; declare the first word as a macro
        " ^ y " -> nonmac ^ x ;
        endsection;])
                                    ;;; give it a procedure, which just
                                    ;;; puts the second word on the stack
enddefine;

endsection;
