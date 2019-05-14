/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:           C.all/lib/auto/set_global_valof.p
 > Purpose:        Set the value of a dynamic variable in all contexts
 > Author:         Aaron Sloman, May 21 1987 (Originally John Williams) (see revisions)
 > Documentation:  REF * IDENT/set_global_valof
 > Related Files:
 */

;;; N.B. This file must not use compile_mode, since it's used by compile_mode!
;;; compile_mode :pop11 +strict;

section;

define global constant procedure set_global_valof(val, word);
    lvars n = 1, pdr, word, val;
    while (caller(n) ->> pdr) do
        if isdlocal(word, pdr) then
            val -> caller_valof(word, n)
        endif;
        n fi_+ 1 -> n
    endwhile;
    val -> caller_valof(word, false)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 22 1995
        Tidied up
 */
