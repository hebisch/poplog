/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/src/xved_declare.ph
 > Purpose:         Main include file for xved sources
 > Author:          John Gibson, Sep  9 1992 (see revisions)
 */

#_TERMIN_IF DEF XVED_DECLARE_INCLUDED

compile_mode :pop11 +strict;

section $-xved;

include xpt_coretypes.ph;

lconstant macro (

    CHECK_SCROLLBAR = [if testdef xved_check_scrollbar then
                            weakref xved_check_scrollbar()
                       endif],
    CHECK_BUFF_MENU = [if testdef xved_check_buffer_menu then
                            weakref xved_check_buffer_menu()
                       endif],

    ;;; Subscripts in the xvedwin_screendatavec vector
    SDVEC_BUFFER        = 1,
    SDVEC_CHARMODE      = 2,
    SDVEC_INSERT_MODE   = 3,
    SDVEC_SCROLLREGION  = 4,
    SDVEC_SCREEN_LINE   = 5,
    SDVEC_SCREEN_COL    = 6,
    SDVEC_LEN   = 6,
);

define lconstant DO_XVED_FOR(p_prefix, list_idname);
    lvars item, decl = "lconstant", lname, idname, params, props;
    if (readitem() ->> idname) == "updaterof" then
        idname -> decl;
        readitem() -> idname
    endif;
    p_prefix <> idname -> lname;
    [% until (readitem()->>item) == ";" do item enduntil %] -> params;
    pop11_define_props(idname, idname, decl =="updaterof") -> props;
    [define ^decl ^lname ^^params with_props ^props ; ^^proglist]
                                                        -> proglist;
    pop11_comp_expr();
    unless decl =="updaterof" then
        writeable
        conspair(sys_current_val(lname), sys_use_current_ident(idname) ->)
                        :: sys_current_val(list_idname)
                        -> sys_current_val(list_idname);
    endunless;
    sys_grbg_list(params)
enddefine;

define :define_form lconstant XVED_FOR;
    DO_XVED_FOR("XVED_FOR_", "xvedidents")
enddefine;

lconstant XVED_DECLARE_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 15 1997
        Split off XVED_FOR procedure so it can be used in xvedscreen.p
--- John Gibson, May  2 1997
        Moved ST_ macros in from xvedvedtraps.p
--- John Gibson, Jun  4 1993
        xved*_for changed to XVED_FOR and uses pair to store swapped value
--- John Gibson, Dec 19 1992
        Added CHECK_SCROLLBAR, CHECK_BUFF_MENU
 */
