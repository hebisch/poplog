/*  --- Copyright University of Sussex 1990.  All rights reserved. ---------
 >  File:           C.all/lib/auto/linearfit.p
 >  Purpose:        returns slope and y intercept for a given list of pairs.
 >  Author:         Dave Hogg, Feb 1982 (see revisions)
 >  Documentation:  REF * LINEARFIT
 >  Related Files:
 */

;;; Give linearfit a list of pairs representing co-ordinates of points.
;;; Returns slope (m) and Y-intercept (c) for the best line through the points
;;; Produces an error if the line is (nearly) vertical.

;;; Note: this is unoptimised - e.g. works on dynamic lists

section;

define global linearfit (pairs) -> m c;
    lvars y, x, a=0, b=0, e=0, f=0, count=0, p, pairs, m, c, det;

    for p in pairs do
        destpair(p) -> y -> x;
        x*x + a -> a;
        x + b -> b;
        x*y + e -> e;
        y + f -> f;
        count fi_+ 1 -> count;
    endfor;

    b -> c;
    a*count - b*c -> det;

    (count*e - b*f)/det -> m;
    (a*f - c*e)/det -> c;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Sep 17 1990
    Corrected comments and description in header. Re-ordered the
    lvars to improve efficiency and re-named some to improve
    clarity.
--- Poplog System, Nov 29 1988 - Changed "list_length" to "listlength"
                                    (cf ALPHA 8)
 */
