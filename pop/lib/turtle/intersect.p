/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           $usepop/master/C.all/lib/turtle/intersect.p
 |  Purpose:        Given four points, A B C D, find where line AB intersects C D
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:  HELP * INTERSECT, TEACH * TURTLE
 |  Related Files:
 */

section;

define global intersect(a, b, c, d);
    lvars a b c d;
    define intersect(xa, ya, xb, yb, xc, yc, xd, yd);
        lvars xa ya xb yb xc yc xd yd yab xab ycd xcd;
        yb - ya -> yab;
        xb - xa -> xab;
        yc - yd -> ycd;
        xc - xd -> xcd;
        (yd * xab * xcd - ycd * xab * xd + yab * xcd * xa - xab * xcd * ya)
        / (yab * xcd - xab * ycd)
    enddefine;
    [%intersect(a(1), a(2), b(1), b(2), c(1), c(2), d(1), d(2)),
         intersect(a(2), a(1), b(2), b(1), c(2), c(1), d(2), d(1))%]
enddefine;

endsection;
