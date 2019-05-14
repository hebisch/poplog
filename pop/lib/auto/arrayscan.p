/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 *  File:           C.all/lib/auto/arrayscan.p
 *  Purpose:        Applies proc to all possible lists of numbers within bounds
 *  Author:         Aaron Sloman, June 1982 (see revisions)
 *  Documentation:  REF * ARRAYS
 *  Related Files:
 */
compile_mode:pop11 +strict;

section;

define global arrayscan(Bounds, P);
    ;;;  Given a list of Bounds, as required for newanyarray, and
    ;;; a procedure which takes a list of co-ordinates, apply the procedure
    ;;; to all possible values of the co-ordinates within the bounds.
    ;;; The coordinates are produced last dimension changing fastest.
    lvars L=listlength(Bounds);
    dlvars Coordinates=[], Coords, Bounds, procedure P;

    unless L mod 2 == 0 then
        mishap(Bounds, 1, 'ODD NUMBER OF ARRAY BOUNDS')
    endunless;
    L div 2 -> L;

    ;;; build a list for co-ordinates
    until L == 0 do
        conspair(0,Coordinates) -> Coordinates; L fi_- 1 -> L
    enduntil;

    define lconstant subarrayscan();
        lvars Index, Upper;
        dlocal Bounds, Coords;
        if Bounds == [] then
            P(copylist(Coordinates))
        else
            fast_back(Coords) -> Coords;
            fast_destpair(fast_destpair(Bounds)) -> Bounds -> Upper -> Index;
            repeat
                Index -> fast_front(Coords);
                subarrayscan();
            quitif(Index == Upper);
                Index fi_+ 1 -> Index;
            endrepeat;
        endif
    enddefine;

    conspair(0, Coordinates) -> Coords;
    subarrayscan();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul  3 1989
        Added +strict
--- John Gibson, Aug  2 1987
        Tidied up
--- A. Sloman  Aug 19 1986
        speeded up and made to produce less garbage.
        Could be even faster if fast_copylist existed.
        Checks for even number of bounds
--- Richard Bignell, Aug 19 1986
        added comment to indicate the ordering
        of coordinates given to the supplied procedure
*/
