/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/auto/getline.p
 *  Purpose:        print a message then call readline
 *  Author:         Aaron Sloman, May 1980
 *  Documentation:  HELP * GETLINE
 *  Related Files:  LIB * REQUESTLINE.
 */
section;

define global getline(x)-> result;
lvars x;
    x=>
    readline() -> result
enddefine;

endsection;
