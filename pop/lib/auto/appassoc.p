/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/appassoc.p
 > Purpose:         Apply a procedure to each entry in an assoc list
 > Author:          Aaron Sloman, Feb 1983 (see revisions)
 > Documentation:   HELP * ASSOC
 > Related Files:   LIB * ASSOC
 */

compile_mode :pop11 +strict;

section;

define global appassoc(assoc_list, pdr);
    lvars assoc_list, procedure pdr, item;
    if isclosure(assoc_list)
    and ispair(frozval(1, assoc_list) ->> item)
    and fast_front(item) == "assoc"
    then
        copylist(fast_back(item)) -> assoc_list;
        for item in assoc_list do
            pdr(dl(item))
        endfor
    elseif isproperty(assoc_list) then
        appproperty(assoc_list, pdr)
    else
        mishap(assoc_list, 1, 'ASSOC STRUCTURE NEEDED')
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan  3 1996
        Now copes with properties (for convenience - see BR rudil.16).
 */
