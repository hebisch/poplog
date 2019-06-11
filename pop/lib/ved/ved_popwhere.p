/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 *  File:           C.all/lib/ved/ved_popwhere.p
 *  Purpose:        find where a procedure is defined in source or library.
 *  Author:         Chris Slymon, June 1983 (see revisions)
 *  Documentation:  HELP * POPINDEX
 *  Related Files:  LIB * POPINDEX
 */

compile_mode: pop11 +strict;
section;

define global vars ved_popwhere();
    lvars entry entry_list;
    dlocal pop_pr_quotes = false;
    if vedargument = '' then
        vederror('No argument');
    endif;
    unless atom(popindex( vedargument) ->> entry_list) then
        if listlength(entry_list) == 1 then
            vedputmessage( hd( entry_list));
        else
            pr('\nIndex entries starting with "' sys_>< vedargument sys_>< '" -\n\n');
            for entry in entry_list do
                tabs(1);
                pr(entry);
                pr(newline);
            endfor;
        endif
    else
        vederror('No matches found in index');
    endunless;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  8 1992
        o Now compiles under +strict
        o Tidied a little
 */
