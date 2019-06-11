/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:           C.all/lib/ved/ved_lib.p
 > Purpose:        Autoloadable extension to VED (load library)
 > Author:         Unknown, ??? (see revisions)
 > Documentation:
 > Related Files:
 */

section;

define lconstant liberror(item);
    lvars item;
    unless isstring(item) then -> item; endunless;
    vederror(item);
enddefine;

define global ved_lib();
dlocal prmishap = liberror;
    vedputmessage('LOADING LIB ' sys_>< vedargument);
    loadlib(vedargument)
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Ian Rogers, Jan 10 1990 - Changed error handling
--- John Williams, Jun  5 1987 - uses 'sys_><' to get standard printing
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
