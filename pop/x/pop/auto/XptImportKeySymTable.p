/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptImportKeySymTable.p
 > Purpose:         Import keycode-to-keysym tables
 > Author:          Adrian Howard, Dec  2 1991
 > Documentation:   REF *XT_KEYBOARD
 > Related Files:   LIB *FAST_XT_KEYBOARD, INCLUDE *XPT_XTYPES.PH
 */


compile_mode:pop11 +strict;
section;


include xpt_constants.ph;


define global XptImportKeySymTable
        = XptImportAny(%XDT_KEYSYMTABLE%)
enddefine;


endsection;
