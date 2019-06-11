/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/ved/vedfindbracket.p
 > Purpose:         Find matching bracket in VED
 > Author:          John Williams, Oct 21 1988 (see revisions)
 > Documentation:   REF * vedfindbracket
 > Related Files:   LIB * VED_GBL, LIB * VED_GEL, LIB * VED_MP, etc.
 */

compile_mode :pop11 +strict;

section;

/* Searches for bra using movepdr to move the cursor.
    Signals an error if endtest is ever true.
    Matches intervening non-quoted instances of bra and ket
*/

define vedfindbracket_quotes =
    newproperty([[lisp '"|'] [ml '"'] [scheme '"']], 32, '\'"`', "perm")
enddefine;


define vedfindbracket(bra, ket, endtest, movepdr);
    lvars c, n, q, bra, ket, quotes, procedure (endtest, movepdr);
    dlocal vedpositionstack;
    vedpositionpush();

    define dlocal interrupt();
        vedpositionpop();
        veddointerrupt();
    enddefine;

    returnif(vedcurrentchar() == bra);
    vedfindbracket_quotes(subsystem) -> quotes;
    false -> q;
    0 -> n;
    repeat
        if endtest() then
            vedpositionpop();
            vederror('Matching "' <> consstring(q or ket, 1) <> '" not found')
        endif;
        movepdr();
        vedcurrentchar() -> c;
        if q then
            if c == q then
                false -> q
            endif
        elseif c == bra then
            quitif(n == 0);
            n fi_+ 1 -> n
        elseif c == ket then
            n fi_- 1 -> n
        elseif strmember(c, quotes) then
            c -> q
        endif
    endrepeat
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, May  1 1997
        Subsystem->quote character mappings now held in property
        vedfindbracket_quotes.
--- John Williams, Sep 20 1995
        Added special quote characters for Scheme
            (as requested by Robin Popplestone).
--- Robert John Duncan, Oct 24 1994
        Added special quote characters for ML (containing '"' only)
--- John Williams, Jun 30 1993
        Added ` to set of Pop-11 quote characters (cf. BR robertg.2)
--- John Gibson, Jan 13 1993
        popcom*piler -> subsystem
 */
