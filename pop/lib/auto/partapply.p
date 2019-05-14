/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/auto/partapply.p
 > Purpose:         Create closure from list of frozvals (moved out of system)
 > Author:          John Gibson, May  8 1988
 > Documentation:   REF *PROCEDURE
 */

section;

global constant procedure partapply = destlist <> consclosure;
"partapply" -> pdprops(partapply);

endsection;
