/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/include/ast.ph
 > Purpose:         Flag bits for AST procedures and facilities using them
 > Author:          John Gibson, Apr 13 1994 (see revisions)
 > Documentation:   REF * SIGNALS
 */

#_TERMIN_IF DEF AST_INCLUDED

section;

iconstant macro (

    ;;; AST Procedure flags, specified by an AST procedure argument
    ;;; of the form conspair(ast_p, flags)

    ASTP_ERROR_DELETE       = 2:1e0,    ;;; delete from queue on mishap etc
    ASTP_BLOCK_RECURSIVE    = 2:1e1,    ;;; block recursive calls
    ASTP_BLOCK_IN_EXTERNAL  = 2:1e2,    ;;; block inside external calls
    ASTP_BLOCK_NEVER        = 2:1e3,    ;;; never block (n.b. DANGEROUS)
    ;;; pair arg is temp, can garbage after use
    ASTP_TEMP_PAIR          = 2:1e4,
    ;;; ast_p is temp closure, can garbage after use
    ASTP_TEMP_CLOSURE       = 2:1e5,

    ;;; Flags to sys_timer

    TIMER_VIRTUAL           = 2:1e0,    ;;; Virtual timer (updater only)
    TIMER_CANCEL            = 2:1e1,    ;;; Cancel on get (accessor only)
    TIMER_REPEAT            = 2:1e2,    ;;; Repeating timer (updater only)
);

iconstant AST_INCLUDED = true;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 20 1996
        Added ASTP_TEMP_PAIR and ASTP_TEMP_CLOS
 */
