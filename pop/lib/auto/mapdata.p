/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/auto/mapdata.p
 >  Purpose:        make a new structure mapping given object with given pdr.
 >  Author:         Mark Rubinstein and Tom Khabaza, Jun 1985 (see revisions)
 >  Documentation:  HELP * MAPDATA
 >  Related Files:  LIB * NCMAPDATA
 */
compile_mode:pop11 +strict;

section;

define global mapdata(item, pdr);
    lvars item, pdr;
    appdata(item, pdr);
    if isword(item) then
        ;;; can't use -fill- with words
        consword(datalength(item))
    else
        fill(copy(item))
    endif
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jul  4 1987
        Rewrote to use -fill- (which uses new updater of -explode-).
--- Mark Rubinstein, Jul 26 1985
        fixed to work with words as well.
--- John Williams, Jun 27 1985 -
        modified to use new procedures ISVECTORCLASS
        and ISRECORDCLASS. Also fixed bug that made it consider zero field
        record structures as vectors.
 */
