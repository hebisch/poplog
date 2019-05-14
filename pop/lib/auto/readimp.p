/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 >  File:           C.all/lib/auto/readimp.p
 >  Purpose:        read either an imperative or imperative sequence
 >  Author:         Allan Ramsay, 1983 (see revisions)
 >  Documentation:  HELP * FORMS
 >  Related Files:  LIB * IMPREAD, *IMPSEQREAD
 */
compile_mode:pop11 +strict;


;;; Procedure to read either an imperative sequence (statement sequence) or
;;; an imperative (expression sequence) - used by impread and impseqread.
;;; An imperative is an expression sequence (separated by commas only).
;;; An imperative sequence allows ";","=>" and "==>" also to separate
;;; statements.

section;

define global readimp(seq);
    lvars seq;
    proglist_read_by(if seq then pop11_comp_stmnt_seq
                     else pop11_comp_expr_seq
                     endif)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  3 1991
        Changed to use new -proglist_read_by-
--- John Gibson, Nov  5 1990
        Tidied up
 */
