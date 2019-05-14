/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/class_fast_access.p
 > Purpose:         Construct non-checking record access/update procedures
 > Author:          John Williams, Feb 20 1990 (see revisions)
 > Documentation:   REF * CLASS_FAST_ACCESS
 > Related Files:
 */

compile_mode:pop11 +strict;

section;

lconstant Fast
    = newanyproperty([[^front ^fast_front]
                      [^back ^fast_back]
                      [^cont ^fast_cont]
                      [^ $-lisp$-char_code ^ $-lisp$-fast_char_code]],
                    16, 1, 16, false, false, "tmpboth", false, false);


define global class_fast_access(n, key) -> fast;
    lvars slow, spec;
    unless (Fast(class_access(n, key) ->> slow) ->> fast) do
        class_field_spec(key) -> spec;
        cons_access(
            lblock
                lvars i = 1, item;
                [% for item in spec do
                    unless item == ">->" do
                        i == n;
                        i + 1 -> i
                    endunless
                endfor %]
            endlblock,
            spec, false, 0) (n)
        ->> Fast(slow) -> fast;
        if isword(pdprops(slow) ->> slow) then
            "fast_" <> slow -> pdprops(fast)
        endif
    endunless
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 16 1991
        Changed -class_spec- to -class_field_spec-
--- John Williams, Jan 15 1991
        Completely re-written using -cons_access-
 */
