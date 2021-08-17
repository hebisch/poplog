/*  --- Copyright University of Sussex 1996. All rights reserved. ----------
 >  File:           C.all/lib/turtle/tjump.p
 >  Purpose:        like jumpt except standardises location to centre of grid
 >  Author:         Aaron Sloman, Jan 25 ???? (see revisions)
 >  Documentation:  HELP * TJUMP, TEACH * TURTLE
 >  Related Files:  LIB * TURTLE
 */

;;; tjump is like jump, in the turtle package, except that it
;;; standardises the location to the centre of a grid square, to minimise
;;; "funny" lines produced by 45 degree jumps. However, it will have other
;;; nasty effects in some circumstances

section;

define global procedure tjump(x);
lvars x;
    jump(x);
    0 -> Xdelta; 0 -> Ydelta;
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  3 1985 - sectionised and lvarsed.
 */
