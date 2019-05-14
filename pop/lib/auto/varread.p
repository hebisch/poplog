/*  --- Copyright University of Sussex 1991. All rights reserved. ----------
 >  File:           C.all/lib/auto/varread.p
 >  Purpose:        reads an identifier name
 >  Author:         Allan Ramsay, November 7 1983 (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define global varread() -> idname;
    lvars idname;
    unless isword(sys_read_path(itemread(), false, false) ->> idname) then
        mishap(idname, 1, 'WORD NEEDED')
    endunless;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Sep 16 1991
        Changed to use sys_read_path.
--- Mark Rubinstein, Sep 27 1985 - lvarsed and sectioned.
 */
