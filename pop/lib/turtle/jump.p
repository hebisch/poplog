/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           $usepop/master/C.all/lib/turtle/jump.p
 |  Purpose:        provide turtle JUMP facility.
 |  Author:         A.Sloman & S. Hardy circa 1978
 |  Documentation:  TEACH * TURTLE
 |  Related Files:
 */

section;

define global procedure jump(amount);
lvars amount;
    jumpto(Newposition(amount))
enddefine;

endsection;
