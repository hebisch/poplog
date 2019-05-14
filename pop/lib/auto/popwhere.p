/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/auto/popwhere.p
 >  Purpose:        find where a procedure is defined in source or library
 >  Author:         Chris Slymon, June 1983 (see revisions)
 >  Documentation:  HELP * POPWHERE
 >  Related Files:  LIB * SOURCEFILE, * POPINDEX
 */
compile_mode:pop11 +strict;


section;

define global popwhere(item);
    lvars item, entry_list;
    unless atom(popindex(item) ->> entry_list) then
        pr('\nIndex entries starting with "' >< item >< '" -\n\n');
        for item in entry_list do
            tabs(1);
            pr(item);
            pr(newline);
        endfor;
    else
        pr('\n    No index entries starting with "' >< item >< '"\n');
    endunless;
enddefine;

endsection;
