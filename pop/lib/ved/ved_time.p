/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/ved/ved_time.p
 *  Purpose:        because everybody always wants this! (everbody loves it!)
 *  Author:         Jonathan Laventhol, Nov 2 1983 (see revisions)
 *  Documentation:
 *  Related Files:
 */

section;

define global ved_time();
    vedputmessage(sysdaytime())      
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Nov 12 1985 - sectionised.
 */
