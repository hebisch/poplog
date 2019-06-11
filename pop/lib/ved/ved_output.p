/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_output.p
 > Purpose:        Controlling output from VED_LMR
 > Author:         A.Sloman, June 1983 (see revisions)
 > Documentation:  HELP * LMR/OUTPUT
 > Related Files:
 */


/*
    ENTER OUTPUT foo.p
    will make the file 'foo.p' the default output file for use
    with load marked range facilities, ENTER LMR, or CTRL-D or <ESC> C.
*/

section;

define global ved_output;
    unless vedargument = nullstring do
        if vedargument = '.' then
            true -> vedlmr_errs_in_file;
            true        ;;; print in current file
        elseif vedargument = '^' then
            false       ;;; don't print in file
        else
            sysfileok(vedargument)
        endif -> vedlmr_print_in_file
    endunless;
    if isstring(vedlmr_print_in_file) then
        'OUTPUT WILL GO TO: ' <> vedlmr_print_in_file
    elseif vedlmr_print_in_file = true then
        'OUTPUT WILL GO TO CURRENT FILE'
    else
        'OUTPUT IN NO FILE'
    endif;
    vedputmessage()
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  4 1989
        Replaced -vednullstring- with -nullstring-
--- John Williams, May 28 1986 - rearranged code; removed '9999 -> veddoitlimit'
--- Aaron Sloman, Jan 19 1986 - changed to use sysfileok(vedargument)
*/
