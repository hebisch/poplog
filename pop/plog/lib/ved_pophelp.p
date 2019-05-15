/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/lib/ved_pophelp.p
 *  Purpose:        looks in pop help files and then prolog help files
 *  Author:         Aaron Sloman, Sep 1983
 *  Documentation:
 *  Related Files:
 */

section;

define global constant ved_pophelp();
    vedsysfile("vedhelpname",
               vedhelpdirectory::vedhelplist,
               vedhelpdefaults);
enddefine;

endsection;
