/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/sprintf.p
 > Purpose:         Constructing formatted strings (by analogy with *PRINTF)
 > Author:          Mark Rubinstein, Oct 18 1985 (see revisions)
 > Documentation:   HELP * SPRINTF
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define lconstant Get_sprintf_args(s) -> l;
    lvars i = 1, l = [], s, item, type, dlen = datalength(s);
    lconstant printf_arg_chars = 'pPsScbdix';
    until i fi_>=  dlen do
        if fast_subscrs(i, s) == `%` and (i fi_< dlen) then     ;;; % found
            if (fast_subscrs(i fi_+ 1, s) ->> type) == `%` then ;;; second %
                i fi_+ 1 -> i
            elseif strmember(type, printf_arg_chars) then
                conspair(->> item, l) -> l;             ;;; get item off stack
                if type == `s` and isstring(item) then  ;;; recurse on string
                    Get_sprintf_args(item) nc_<> l -> l
                endif
            endif
        endif;
        i fi_+ 1 -> i
    enduntil
enddefine;


define sprintf(s);
    lvars l, s, grbg_l;
    dlocal cucharout;

    if islist(s) then
        s -> l;
        false -> grbg_l;                    ;;; must not garbage l
        unless isstring(->> s) then
            mishap(s, 1, 'STRING NEEDED')
        endunless;
    elseif isstring(s) do
        fast_ncrev(Get_sprintf_args(s)) -> l;
        true -> grbg_l                      ;;; l can be garbaged
    else
        mishap(s, 1, 'LIST (OR STRING) EXPECTED')
    endif;

    identfn -> cucharout;   ;;; can cause problems with traced procedures
    consstring(#| printf(s, unless l == [] then l endunless) |#);

    if grbg_l then
        sys_grbg_list(l)
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 14 1996
        Removed dlocal setting of pr in sprintf (should behave exactly the
        same as printf)
--- John Williams, Jan 26 1993
        Added `S` to printf_arg_chars (cf. BR johnw.1049)
--- Adrian Howard, Aug 20 1992
        Removed -dlocal- of -pop_pr_quotes- to -false- since this caused
        the resultant string to be different from that produced by -printf-
        in certain situations.
--- John Williams, May 26 1992
        Fixed BR aarons.108, also added `P` to -printf_arg_chars-
--- Simon Nichols, May 16 1991
        Changed -sprintf- to dlocal -pr- to be -syspr- instead of
        -sys_syspr-. This is to ensure that both -printf- and -sprintf-
        produce the same printing representation of an object when its
        -class_print- has been changed. See bugreport isl-fr.4338.
--- John Gibson, May 14 1989
        Replaced procedure-local -vars- with -dlocal-
 */
