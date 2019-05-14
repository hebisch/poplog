/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/auto/deadproc.p
 *  Purpose:        test if a process is dead
 *  Author:         Unknown, ???
 *  Documentation:  HELP * DEADPROC
 *  Related Files:
 */

section;

define global constant deadproc() with_nargs 1;
    not(isliveprocess());
enddefine;

endsection;
