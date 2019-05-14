/* --- Copyright University of Sussex 1987. All rights reserved. ----------
 > File:            C.all/lib/auto/syssynonym.p
 > Purpose:         Associate words with the same identifier
 > Author:          John Gibson, Nov 24 1987
 > Documentation:   REF *IDENT
 */

section;

define global constant procedure syssynonym(token1, token2);
    lvars token1, token2;
    identof(token2) -> identof(token1)
enddefine;

endsection;
