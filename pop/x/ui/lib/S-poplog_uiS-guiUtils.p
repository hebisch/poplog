/* --- Copyright University of Sussex 1999.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-guiUtils.p
 > Purpose:         Various common utilities used by the Poplog UI
 > Author:          Julian Clinton, August 1991 (see revisions)
 > Documentation:
 > Related Files:   gui*.p, xol*.p, xm*.p
*/
compile_mode :pop11 +strict;

section $-poplog_ui;

include sigdefs.ph;


/* ---------------------------------------------------------------
    Interrupt
   --------------------------------------------------------------- */

define guiInterrupt = sys_raise_signal(%SIG_INT%) enddefine;

/* ---------------------------------------------------------------
    Arg List
   --------------------------------------------------------------- */

define ConsArgList(len);
    lvars len = len fi_>> 1;
    nc_consXptArgList((), len), len
enddefine;


/* ---------------------------------------------------------------
    Type checking
   --------------------------------------------------------------- */

define Check_type(item, allow_false, word);
    lvars item, allow_false, word;
    unless not(item) and allow_false then
        unless dataword(item) == word then
            mishap(item, 1, sprintf(word, '%p needed'));
        endunless;
    endunless;
enddefine;

define Check_word   = Check_type(%"word"%)    enddefine;
define Check_integer= Check_type(%"integer"%) enddefine;

define Check_string(item, allow_false);
    unless not(item) and allow_false then
        unless isstring(item) then
            mishap(item, 1, 'string needed');
        endunless;
    endunless;
enddefine;

define Check_list(item, allow_false);
    lvars item, allow_false, dword;
    unless not(item) and allow_false then
        unless (dataword(item) ->> dword) == "pair" or dword == "nil" then
            mishap(item, 1, 'list needed');
        endunless;
    endunless;
enddefine;

constant guiUtils = true;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Mar 19 1999
        Added Julian's fix for Check_string allowing string16
--- John Gibson, Jun 28 1993
        Changes for POPC
--- Jonathan Meyer, Sep  2 1991 Added EXIT_ACTION
 */
