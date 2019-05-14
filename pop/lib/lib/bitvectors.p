/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.all/lib/lib/bitvectors.p
 > Purpose:        Define a bit vectorclass (as used by Clisp)
 > Author:         John Williams, Oct  2 1986
 > Documentation:  HELP * BITVECTORS
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

defclass bitvector :1;

procedure(bv);
    lvars i;
    cucharout(`<`);
    pr("bitvector");
    cucharout(`\s`);
    fast_for i from 1 to datalength(bv) do
        cucharout(fast_subscrbitvector(i, bv) fi_+ `0`)
    endfast_for;
    cucharout(`>`);
endprocedure -> class_print(bitvector_key);

constant bitvectors = true;

endsection;
