/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/isinstance.p
 > Purpose:        Check if object is an instance of specified time.
 > Author:         Mark Rubinstein, Apr 18 1986
 > Documentation:  HELP * FLAVOUR_LIBRARY, TEACH * FLAVOUR
 > Related Files:  LIB * FLAVOURS
 */


section $-flavour => isinstance;

define global isinstance(inst, flave);
lvars inst flave;
    unless isflavour_instance(inst) do return(false) endunless;
    unless flave do return(true) endunless;
    if isword(flave) then
        flavourrecord_of(flave) -> flave
    elseif isflavour_instance(flave) then
        I_frecord(flave) -> flave
    endif;
    unless isflavourrecord(flave) do
        mishap('Flavour or name of flavour needed', [^flave])
    endunless;
    fast_lmember(flave, f_standardpreclist(i_frec(inst)));
enddefine;

endsection;
