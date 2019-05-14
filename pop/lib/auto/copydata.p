/* --- Copyright University of Sussex 1998.  All rights reserved. ---------
 > File:           C.all/lib/auto/copydata.p
 > Purpose:        recursive data structure copier
 > Author:         Roger Evans, Aug 22 1985 (see revisions)
 > Documentation:  HELP * COPYDATA
 */

compile_mode :pop11 +strict;

section;

define copydata(x);
    lvars key = datakey(x), i, vec, procedure subscr_p, maxsub, minsub;
    if key == weakref prologvar_key then
        weakref[prologvar_key] prolog_newvar()
    elseif class_field_spec(key) then
        appdata(x,
            procedure(y);
                if y == x and datakey(y) /== weakref prologvar_key then
                    ;;; Mustn't print circular structure!
                    mishap(0, 'TRYING TO COPY A CIRCULAR ' sys_>< dataword(y))
                else
                    copydata(y)
                endif
            endprocedure);
        if isvectorclass(x) then datalength(x) endif;
        fast_apply(class_cons(key))
    elseif isarray(x) then
        copy(x) -> x;
        arrayvector(x) -> vec;
        arrayvector_bounds(x) -> (maxsub, minsub);
        array_subscrp(x) -> subscr_p;
        fast_for i from minsub to maxsub do
            copydata(subscr_p(i, vec)) -> subscr_p(i, vec)
        endfor;
        x
    elseif isproperty(x) then
        copy(x)
    else
        x
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 16 1998
        Fixed to recursively copy the elements of arrays (cf BR rudil.17)
--- John Gibson, Dec 28 1995
        Weakened prolog references
--- John Williams, Oct 27 1995
        Now copies properties (cf. BR isl-fr.4579)
--- John Williams, Dec  3 1991
        Now copies arrays (cf. BR rudil.11)
--- John Williams, Jan 16 1991
        Changed -class_spec- to -class_field_spec-
*/
