/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:           C.all/lib/ved/vedoverlayblock.p
 > Purpose:        "Yank" a vector of strings with spaces into VED
 > Author:         Aaron Sloman, Dec 1989 (see revisions)
 > Documentation:  HELP * VEDBLOCKS HELP * VEDOVERLAYBLOCK
 > Related Files:  LIB * VEDYANKBLOCK, * VEDREFRESHBLOCK, * VEDCUTBLOCK,
 >                      * VEDFILLBLOCK
 */
compile_mode :pop11 +strict;

;;; Insert a vector of strings, of the kind produced by vedcutblock,
;;; into VED buffer, except that spaces in the strings indicate bits
;;; of VED buffer to leave unchanged.

section;

uses vedrefreshblock;

define vedoverlayblock(vector);
    ;;;  Starting at current location insert NON SPACE characters from
    ;;;; strings stored in the vector in current line and subsequent lines,
    ;;; always starting from the same column
    lvars string_index, string, char, vec_index, vector,
        col1 = vedcolumn, line1 = vedline, stringlim,
        veclen = datalength(vector),
        oldchanged = vedchanged,
        oldediting = vedediting;
    dlocal vedbreak = false,vedautowrite = false, vedstatic = true;
    if veclen = 0 then return endif;
    subscrv(1,vector) ->;   ;;; generate error if not a vector
    fast_for vec_index from 1 to veclen do
        fast_subscrv(vec_index,vector) -> string;
        ;;; check that it is a non-empty string
        unless isstring(string) then
            mishap(string, 1,
              'ELEMENT ' sys_>< vec_index sys_>< ' OF VECTOR IS NOT A STRING')
        endunless;
        datalength(string) -> stringlim;
        false -> vedediting;    ;;; normal refresh often too slow
        fast_for string_index from 1 to stringlim do
            fast_subscrvedstring(string_index, string) -> char;
            if char == `\s` then vedcharright() else vedcharinsert(char)
            endif
        endfor;
        oldediting -> vedediting;
        vedrefreshblock(vedline,col1,vedline,vedcolumn, false);
        vedchardown(); col1 -> vedcolumn;
    endfast_for;
    vedjumpto(line1,col1);
    if oldchanged then oldchanged + 1 else 1 endif -> vedchanged;
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 10 1995
        Changed to use fast_subscrvedstring
 */
