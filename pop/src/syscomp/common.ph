/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/syscomp/common.ph
 > Purpose:
 > Author:          John Gibson, Jun  5 1989 (see revisions)
 */

/* -------------------------------------------------------------------------

            COMMON INCLUDE FILE FOR POPC, POPLINK, POPLIBR

--------------------------------------------------------------------------*/

compile_mode:pop11 +strict;

12 -> item_chartype(`\\`, itemread);    ;;; make \ an alphabeticiser


#_IF pop_debugging

    lconstant macro (
        f_hd        = "hd",
        f_tl        = "tl",
        f_dest      = "dest",
        f_front     = "front",
        f_back      = "back",
        f_destpair  = "destpair",
        f_subv      = "subscrv",
        f_subs      = "subscrs",
        f_sysread   = "sysread",
        f_syswrite  = "syswrite",
        );

#_ELSE

    lconstant macro (
        f_hd        = "fast_front",
        f_tl        = "fast_back",
        f_dest      = "fast_destpair",
        f_front     = "fast_front",
        f_back      = "fast_back",
        f_destpair  = "fast_destpair",
        f_subv      = "fast_subscrv",
        f_subs      = "fast_subscrs",
        f_sysread   = "fast_sysread",
        f_syswrite  = "fast_syswrite",
        );

#_ENDIF

iconstant macro (

    ;;; Values in current_asm_segment
    ASMSEG_UNDEF        = 0,
    ASMSEG_WRITEABLE    = 1,
    ASMSEG_NONWRITEABLE = 2,
    ASMSEG_CODE         = 3,
);



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 11 1995
        Added f_front, f_back, f_destpair
--- John Gibson, Feb  9 1995
        Added ASMSEG_ values
 */
