/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_clause.p
 > Purpose:         LIB * FORMAT_PRINT ~; directive
 > Author:          John Williams, Oct 16 1985 (see revisions)
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

uses format_print;

section $-lisp$-fpr;

define vars f_clause() with_nargs 2;
    ->; ->;
    misplaced_directive(`;`)
enddefine;


procedure() with_nargs 2;
    f_clause()
endprocedure -> f_proc(`;`);

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
 */
