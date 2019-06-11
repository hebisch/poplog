/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/vedwordcount_text.p
 >  Purpose:        word counter for ved
 >  Author:         Tom Khabaza, Mar 20 1985 (see revisions)
 >  Documentation:  HELP * VEDWORDCOUNT_TEXT
 >  Related Files:
 */
compile_mode :pop11 +strict;

/*  This wc command for ved uses more or less the same rules of
 *  itemisation as the unix wc command, and is also fairly fast,
 *  averaging approx. 2.4 cpu seconds for a 2000 word file, as
 *  opposed to 4.5 if the poplog itemiser is used.
 *  (These are Systime 8750 timings.)
 */

section;

define vedwordcount_text(lo, hi);
    lvars c, n, token, i, j, l, hi, lo;
    if hi > vvedbuffersize or lo < 1 or lo > hi then
        mishap('Illegal bounds for vedwordcount_text', [^lo ^hi]);
    endif;
    vedpositionpush();
    0 -> n;
    fast_for i from lo to hi do
        ;;; token flag indicates whether currently in a token
        false -> token;
        i -> vedline;
        vedsetlinesize();
        vedthisline() -> l;
        fast_for j from 1 to vvedlinesize do
            ;;; added test for backspace. A.S. Jan 1988
            if ((fast_subscrs(j,l) ->> c) fi_<= `\s` and c /== `\b`)
            or c fi_>= 127
            then
                ;;; if we've come to the end of a token, increment count
                if token then
                    false -> token;
                    n fi_+ 1 -> n;
                endif;
            else
                true -> token;
            endif;
        endfast_for;
        ;;; end of line is also token seperator
        if token then n fi_+ 1 -> n; endif;
    endfast_for;
    vedpositionpop();
    vedputmessage(
        if lo == 1 and hi == vvedbuffersize then
            'File'
        else
            'Range'
        endif sys_>< ' word count is ' sys_>< n sys_>< ' words.');
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan 23 1988 added test for backspace, to make it agree
    with Unix wc for underlined words.

--- Aled Morris, Aug 19 1986  Changed >< to sys_><, and added fast integer
    loops and tests, increasing speed by a factor of 2 or 3
*/
