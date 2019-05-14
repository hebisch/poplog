/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/lib/S-in_arrayS-runtime.p
 > Purpose:         Run-time procedures for * IN_ARRAY
 > Author:          David S Young, Jan 31 1994 (see revisions)
 > Documentation:   HELP * IN_ARRAY
 > Related Files:   LIB * IN_ARRAY
 */

compile_mode:pop11 +strict;

section $-in_array;

/*
-- General non-fast argument checking ---------------------------------
*/

define check_region(narrs, region) -> ndim;
    ;;; Checks that the region has an even length, that the arrays
    ;;; all have dimensions equal to half this length, and that the
    ;;; region is enclosed by each of the arrays.
    ;;; Arrays expected on the stack.
    ;;; Returns the number of dimensions.
    lvars narrs, region, ndim = length(region) / 2;
    lvars array, i1, i2,
        test1 = nonop fi_>,
        test2 = nonop fi_<;
    unless ndim.isinteger then
        mishap(region, 1, 'Region needs even number of elements')
    endunless;
    repeat narrs times
            -> array;       ;;; off stack
        unless pdnargs(array) == ndim then
            mishap(region, array, 2,
                'Region and array have different dimensions')
        endunless;
        for i1, i2 in boundslist(array), region do
            if test1(i1, i2) then
                mishap(region, array, 2, 'Region not inside array')
            endif;
            (test1, test2) -> (test2, test1)        ;;; swap
        endfor
    endrepeat
enddefine;

define min_region(array, narrs) -> (ndim, region);
    ;;; Checks all the arrays have the same no. dimensions, and
    ;;; returns the intersection of their boundslists
    ;;; and the number of dimensions.
    ;;; Must be narrs arrays on the stack, narrs >= 1.
    lvars array, narrs, region, ndim = pdnargs(array);
    lvars i1, i2,
        op1 = fi_max,
        op2 = fi_min;
    copylist(boundslist(array)) -> region;
    repeat narrs - 1 times
            -> array;       ;;; off stack
        unless pdnargs(array) == ndim then
            mishap(array, 1, 'Arrays have different dimensions')
        endunless;
        for i1, i2 in region, boundslist(array) do
            op1(i1, i2);         ;;; on stack
            (op1, op2) -> (op2, op1)
        endfor -> dl(region);       ;;; update region
    endrepeat
enddefine;

/*
-- Checking for 1-D equivalence ---------------------------------------
*/

define arrayvec_index(/* coords, */ arr) /* -> index */;
    ;;; Returns the index into the arrayvector for the coordinates
    ;;; of the array given, or <false> if the array is arrayed by
    ;;; column and has more than one dimension.
    lvars arr;
    lvars n = pdnargs(arr);
    if arr.isarray_by_row or n == 1 then
        lvars d, x, x0, x1,
            m = 1,          ;;; multiplier for current coord
            ( , i) = arrayvector_bounds(arr),
            bl = boundslist(arr);
        for d from n by -1 to 1 do
            ;;; We subscript the stack to avoid having to build a structure
            subscr_stack(d) -> x;       ;;; current coord
            fast_destpair(fast_destpair(bl)) -> (x0, x1, bl);
            i fi_+ m fi_* (x fi_- x0) -> i;     ;;; jump for index
            m fi_* (x1 fi_- x0 fi_+ 1) -> m;    ;;; jump for next index
        endfor;
        ;;; Remove the args
        erasenum(n);
        i /* -> index */
    else
        erasenum(n);
        false /* -> index */
    endif
enddefine;

define arrays_1d(narrs, region, startpts) -> rsize;
    ;;; If the region is continuous in all the arrays, and they
    ;;; are all arrayed by row, fill the startpoints vector, and return
    ;;; the size of the region.
    ;;; Otherwise, return false.
    ;;; Arrays are expected to be on the stack before narrs.
    lvars narrs, region, startpts, rsize;
    lvars iarr, arr, r, i1, i2;
    ;;; Get size of region
    region -> r;
    1;
    until r == [] do
        * (- (fast_destpair(r) -> r) + (fast_destpair(r) -> r) + 1)
    enduntil -> rsize;

    for iarr from narrs by -1 to 1 do
            -> arr;         ;;; get it off the stack
        ;;; Get first and last indices of region in array (using
        ;;; odd then even elements of region)
        region -> r;
        arrayvec_index(
            until r == [] do
                fast_destpair(r) -> r; fast_back(r) -> r
            enduntil,
            arr) -> i1;
        region -> r;
        arrayvec_index(
            until r == [] do
                fast_back(r) -> r; fast_destpair(r) -> r
            enduntil,
            arr)-> i2;
        unless i1 and i2 and i2 - i1 + 1 == rsize then
            ;;; Non-continuous or by column and dims > 1
            false -> rsize;
            erasenum(iarr - 1); ;;; remove spare arguments
            quitloop ;;; and give up
        endunless;
        i1 -> startpts(iarr)
    endfor
enddefine;

/*
-- For initialising the run-time coordinates vector -------------------
*/

define getcoords(region, coords, mincoords, maxcoords, ndim);
    lvars region, coords, mincoords, maxcoords, ndim;
    lvars i;
    dl(region);        ;;; put it all on the stack
    for i from ndim by -1 to 1 do
            -> maxcoords(i);
            ->> mincoords(i) -> coords(i)
    endfor;
    coords(1) - 1 -> coords(1);     ;;; start point
enddefine;

/*
-- To avoid re-loading ------------------------------------------------
*/

constant runtime = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- David S Young, Feb  1 1994
        Added constant $-in_array$-runtime to avoid reloading
 */
