/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_jrefmr.p
 > Purpose:         Justification and indentation in REF files
 > Author:          Aaron Sloman, Sep 16 1990 (see revisions)
 > Documentation:   REF * REFFORM /ved_jrefmr
 > Related Files:
 */


compile_mode:pop11 +strict;
section;

define ved_jrefmr();
    dlocal vedleftmargin=strnumber(vedargument) or 8, vedlinemax=72;
    lvars lo=vvedmarklo, hi=vvedmarkhi;

    ;;; -true- IF -string- CONTAINS GRAPHIC CHARACTERS
    define lconstant Is_picture(string);
        lvars string, char;
        fast_for char in_string string do;
            returnif(strmember(
                char,
                '\Gle\Gre\Gte\Gbe\Gtl\Gtr\Gbl\Gbr\Glt\Grt\Gtt\Gbt\G|\G+\G-'
            ))(true);
        endfast_for;
        return(false);
    enddefine;

    ;;; CAN JUSTIFY IF NOT A PICTURE, AND NO FORMAT SPACES
    define lconstant Can_justify(n);
        lvars n;
        lvars line=vedbuffer(n), len=line.datalength;
        not(len == 0 or len > 0 and
            (strmember(subscrs(1, line), '\Sf\Sp') or Is_picture(line))
        );
    enddefine;

    ;;; RIGHT JUSTIFY LINES
    define lconstant Justify(vvedmarklo, vvedmarkhi) /* -> END */;
        dlocal vvedmarklo, vvedmarkhi;
        ved_jj();
        return(vvedmarkhi);     ;;; END OF THE JUSTIFIED SECTION
    enddefine;

    if lo>hi then
        vederror('NO MARKED RANGE');
    else
        lvars jstart=false, jend, can_j, n=lo;
        until n > hi do;
            if Can_justify(n) ->> can_j then
                jstart or n -> jstart;
            endif;
            if (not(can_j) and jstart) or (can_j and n == hi) then
                (n==hi and n) or n-1 -> jend;
                Justify(jstart or jend, jend) -> vvedmarkhi;
                hi-(jend-vvedmarkhi) -> hi;
                vvedmarkhi -> n;
                false -> jstart;
            endif;
            n+1 -> n;
        enduntil;
    endif;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 16 1993
        Tidied, made +strict, no skips over pictures and lines with format
        spaces
 */
