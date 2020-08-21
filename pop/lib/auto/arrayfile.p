/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/arrayfile.p
 > Purpose:         Reading and writing bit-packed arrays to/from disk
 > Author:          David Hogg, January 1989 (see revisions)
 > Documentation:   HELP * ARRAYFILE
 */
compile_mode:pop11 +strict;

section;

weak constant procedure key_of_dataword;

lconstant bytes_in_word = sys_word_bits/8;

define lconstant Nbytes(item);
    lvars item;
    (datasize(item) fi_- 2) * bytes_in_word
enddefine;

lconstant Infoblk = inits(512);


define global arrayfile(file) -> array;
    lvars array = false, file;
    lvars dev bounds by_row key min_sub procedure (Numread) type vec;

    ;;; allow optional second (array) argument so that it can read into
    ;;; an existing array
    if isarray(file) then
        file -> (file, array)
    endif;

    sysopen(file, 0, true, `N`) -> dev;
    sysread(dev, 1, Infoblk, 512) ->;
    incharitem(stringin(Infoblk)) -> Numread;

    /* Get array bounds */
    [% repeat (Numread() * 2) times Numread() endrepeat %] -> bounds;

    /* Get array type */
    Numread() -> type;
    if type = termin then
        ;;; for compatability with old datafiles that didn't record type
        "string" -> type
    endif;
    key_of_dataword(type) -> key;
    unless key do
        mishap(type, 1, 'Unknown vectorclass')
    endunless;

    /* Get array_by_row */
    Numread() -> by_row;
    if by_row = 0 then
        false
    else
        ;;; covers by_row = 1 or by_row = termin (i.e. not specified in file)
        true
    endif -> by_row;

    /* Create (or check) array */
    if array then
        unless boundslist(array) = bounds
        and datakey(arrayvector(array)) == key
        and isarray_by_row(array) == by_row
        do
            mishap(file, array, 2, 'Array not compatible with file data')
        endunless;
        arrayvector_bounds(array) -> min_sub ->;
        unless min_sub == 1 do
            mishap(array, 1, 'Array must not be offset in arrayvector')
        endunless
    else
        newanyarray(bounds, key, by_row) -> array
    endif;

    /* Read data from file into -arrayvector- of the array */
    arrayvector(array) -> vec;
    sysread(dev, 1, vec, Nbytes(vec)) ->;
    sysclose(dev);
enddefine;


define updaterof arrayfile(array, file);
    lvars array file;
    lvars dev vec;
    dlocal cucharout, pr = sys_syspr, pop_pr_radix = 10;

    define lconstant Stringout(s);
        lvars i = 1, s;
        procedure(/* byte */) with_nargs 1;
            (/* byte */) -> fast_subscrs(i, s);
            i fi_+ 1 -> i
        endprocedure
     enddefine;

    /* Write array bounds, type, and by_row into Infoblk */
    Stringout(Infoblk) -> cucharout;
    spr(length(boundslist(array)) div 2);
    applist(boundslist(array), spr);
    arrayvector(array) -> vec;
    spr(dataword(vec));
    spr(if isarray_by_row(array) then 1 else 0 endif);
    cucharout(0);   ;;; in case of overlap with previous use of Infoblk

    /* Write Infoblk and -arrayvector- of array to file */
    syscreate(file, 1, true) -> dev;
    syswrite(dev, 1, Infoblk, 512);
    syswrite(dev, 1, vec, Nbytes(vec));
    sysclose(dev);
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct  2 1992
        Added declaration for key_of_dataword
--- John Williams, Apr 27 1992
        Tidied up for installation in masters
        Now records whether array is by row or column
--- David S Young, Jan  3 1992
        A file can now be read into an existing array
--- David S Young, Aug  2 1991
        Infoblk made file lconstant (was lvar in each procedure)
        cucharout made dlocal (was vars)
        Stringout changed to use lvar instead of ref to hold index,
            closure now implicit rather than explicit
        arguments made lvars
        section ... endsection added
--- David Young, Feb 12 1990
        Changed header comment format so that newmaster transport can work
*/
