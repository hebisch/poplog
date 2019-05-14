/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/auto/fast_deref.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *FASTPROCS
 */

section;

    ;;; while you have a reference with non-false contents get its contents
    ;;; if the contents are false return the reference.
define global constant procedure fast_deref(ref) -> ref;
    lvars ref, refcont;
    while isref(ref) and (fast_cont(ref) ->> refcont) do
        refcont -> ref
    endwhile
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  8 1988
        Moved out of system
 */
