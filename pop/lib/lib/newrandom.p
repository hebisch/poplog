/* --- Copyright University of Sussex 1998.  All rights reserved. ---------
 > File:           C.all/lib/lib/newrandom.p
 > Purpose:        Random number generater
 > Author:         David Young (inserted by A.Sloman), Dec 9 1986 (see revisions)
 > Documentation:  HELP * RANDOM
 > Related Files:  $usepop/master/C.all/src/random.p
 */
compile_mode :pop11 +strict;

/*
Replacement for the built in POPLOG random number generator. Based on:

Wichmann, B.A., and Hill, I.D., 'A Pseudo-random Number Generator', National
Physical Laboratory Report DITC 6/82, June 1982.

It is slower than the version of RANDOM built into the system, but gives a
far more satisfactory series of numbers.

The algorithm requires only 16 bit integers, yet manages a cycle length of
about 30000 ** 3, which suffices for many applications.

The numbers are carefully chosen and cannot be changed
without a deep understanding of the structure of the generator.

The integer -> real -> integer sequence for random(n) where n is an
integer may seem inefficient but is required for a properly uniform
distribution.

Alternatively, the NAG Fortran library uses the algorithm

    ((13 ** 13) * seed) mod (2 ** 59) -> seed

which has a cycle length of 2 ** 59 but this needs sensitive
implementation on each machine if it is not to use bigintegers.

*/

section;

uses-now int_parameters;

sysunprotect("random");
sysunprotect("random0");

;;; SEEDS
vars ranseed ranseed2 ranseed3;

;;; ranseed is initialised randomly. Must not be a biginteger.
sys_real_time() mod (pop_max_int + 1) -> ranseed;

;;; Give the other two arbitrary values to start with.
12345 -> ranseed2;
98765 -> ranseed3;

define lconstant Random();
    ;;; returns a uniformly distributed random number in the range 0.0
    ;;; to 1.0.
    lvars quot remain;
    ranseed fi_// 177 -> quot -> remain;
    171 fi_* remain fi_- 2 fi_* quot -> quot;   ;;; uses register
    while quot fi_<= 0 do
        quot fi_+ 30269 -> quot;
    endwhile;
    quot -> ranseed;
    ranseed2 fi_// 176 -> quot -> remain;
    172 fi_* remain fi_- 35 fi_* quot -> quot;
    while quot fi_<= 0 do
        quot fi_+ 30307 -> quot
    endwhile;
    quot -> ranseed2;
    ranseed3 fi_// 178 -> quot -> remain;
    170 fi_* remain fi_- 63 fi_* quot -> quot;
    while quot fi_<= 0 do
        quot fi_+ 30323 -> quot
    endwhile;
    quot -> ranseed3;
    fracof(ranseed/30269.0 + ranseed2/30307.0 + ranseed3/30323.0)
enddefine;


define random0(n);
    lvars n;
    n * Random();
    if isinteger(n) or isbiginteger(n) then
        intof()
    endif
enddefine;

define random(n);
    lvars n;
    n * Random();
    if isinteger(n) then
        intof() fi_+ 1
    elseif isbiginteger (n) then
        intof() + 1
    endif
enddefine;

constant newrandom = true;      /* for uses */


endsection;

/* --- Revision History ---------------------------------------------------
--- David S Young, Jul 16 1998
        ranseed initialised to int and not bigint (report from Aaron Sloman)
--- John Williams, May 26 1998
        Defines identifier newrandom for the benefit of uses.
--- Aaron Sloman, Dec  9 1986
        Slightly optimised, made to check more,  and prepared for library
*/
