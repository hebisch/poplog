/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/turtle/sysdisplay.p
 *  Purpose:        Like display, but takes a picture as input
 *  Author:         Aaron Sloman, Jan 1983
 *  Documentation:  TEACH * TURTLE
 *  Related Files:  LIB * TURTLE
 */

section;

define global sysdisplay(picture);
    pr(newline);
    display()
enddefine;

endsection;
