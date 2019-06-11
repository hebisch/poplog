/*  --- Copyright University of Sussex 1987.  All rights reserved. ---------
 > File:           C.all/lib/ved/vedyankblock.p
 > Purpose:        "Yank" a vector of strings in VVEDBLOCKDUMP into VED
 > Author:         Aaron Sloman, May 17 1987 (see revisions)
 > Documentation:  HELP * VEDYANKBLOCK
 > Related Files:  LIB * VEDREFRESHBLOCK, * VEDCUTBLOCK, * VEDFILLBLOCK
 */
compile_mode :pop11 +strict;

;;; Insert a vector of strings, of the kind produced by vedcutblock,
;;; into VED buffer.
;;; Will overwrite existing contents if vedstatic is true.

section;

uses vedrefreshblock;

define vedyankblock(vector);
    ;;;starting at current location insert strings stored in the vector
    ;;;in current line and subsequent lines, always starting from
    ;;; the same column
    lvars n,vector,col1=vedcolumn,line1=vedline,
        string,
        len=datalength(vector),
        oldchanged=vedchanged,
        oldediting=vedediting;
    dlocal vedbreak = false, vedautowrite = false;

    if len = 0 then return endif;
    subscrv(1,vector) ->;   ;;; generate error if not a vector
    fast_for n from 1 to len do
        fast_subscrv(n,vector) -> string;
        false -> vedediting;    ;;; normal refresh often too slow
        vedinsertstring(string);
        oldediting -> vedediting;
        vedrefreshblock(vedline,col1,vedline,vedcolumn, not(vedstatic));
        vedchardown(); col1 -> vedcolumn;
    endfast_for;
    vedjumpto(line1,col1);
    if oldchanged then oldchanged + 1 else 1 endif -> vedchanged;
enddefine;


endsection;
