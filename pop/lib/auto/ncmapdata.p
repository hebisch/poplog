/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           C.all/lib/auto/ncmapdata.p
 *  Purpose:        return object with pdr applied to every element (non copy)
 *  Author:         Mark Rubinstein, Jul 26 1985 (see revisions)
 *  Documentation:  HELP * MAPDATA
 *  Related Files:  LIB * MAPDATA
 */

section;

define global constant procedure ncmapdata(item, pdr);
    lvars item, pdr;
    fill(appdata(item, pdr), item)
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul  4 1987
        Rewrote to use new -fill-.
 */
