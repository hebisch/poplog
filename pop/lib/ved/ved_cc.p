/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_cc.p
 >  Purpose:        Display information about current character
 >  Author:         A.Sloman 1982 (see revisions)
 >  Documentation:  HELP * VEDCOMMS/ved_cc
 >  Related Files:  LIB *VED_IC
 */
compile_mode:pop11 +strict;

section;

define vars ved_cc;
    lvars vc = vedcurrentvedchar(), dc = vc, c, l;
    if ispair(vc) then front(vc) -> dc endif;
    dc && 16:FFFF -> c;

    if vedargument = 'd' then
        ;;; display embedded data
        if vc == dc then
            'NO DATA'
        elseif isstring(back(vc)) then
            '\'' <> back(vc) <> '\''
        else
            back(vc) sys_>< nullstring
        endif;
        vedputmessage();
        return
    endif;

    lconstant
        names = [
            `\s`    'space'
            `\t`    'tab'
            `\Nt`   'trailing newline'
            `\St`   'trailing space'
            `\Sf`   'format-control space'
            `\Sp`   'prompt-marker space'
            `\Ss`   'Ved no-break space'
            16:9A   'Ved hair space'        ;;; = `\Sh`
            `\Sn`   'ISO Latin no-break space'
            `\^?`   'DEL'
        ],

        graph_names = [
            `\Gle` 'leftend'
            `\Gre` 'rightend'
            `\Gte` 'topend'
            `\Gbe` 'bottomend'

            `\Gtl` 'topleft'
            `\Gtr` 'topright'
            `\Gbl` 'bottomleft'
            `\Gbr` 'bottomright'

            `\Glt` 'lefttee'
            `\Grt` 'righttee'
            `\Gtt` 'toptee'
            `\Gbt` 'bottomtee'

            `\G-`  'horizontal'
            `\G|`  'vertical'
            `\G+`  'cross'

            `\Go`  'degree'
            `\G#`  'diamond'
            `\G.`  'dot'
        ];

    vedputmessage(consstring(#|
        explode('CODE = '),
        dest_characters(c),
        explode(', i.e. '),
        if fast_lmember(c, names) ->> l then
            explode(subscrl(2, l))
        elseif fast_lmember(c, graph_names) ->> l then
            explode('GRAPH '), explode(subscrl(2, l))
        elseif c < `\s` then
            explode('CTRL '), c + `@`
        else
             `"`, c, `"`
        endif;

        if dc /== c then
            explode(' (ATTR = '),
            if dc &&/=_0 `\[b]` then explode('bold,') endif,
            if dc &&/=_0 `\[u]` then explode('underline,') endif,
            if dc &&/=_0 `\[i]` then explode('italic,') endif,
            if dc &&/=_0 `\[f]` then explode('blink,') endif,
            if dc &&/=_0 `\[A]` then explode('Active,') endif,
            if dc &&/=_0 `\[7]` then
                explode('colour '),
                ((dc && `\[7]`) >> #_< integer_leastbit(`\[7]`) >_#) + `0`,
                `,`
            endif,
            () -> ;
            `)`
        endif;

        if vc /== dc then explode('(HAS DATA)') endif
    |#))
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Apr 24 1999
        Added Ved hair space
--- John Gibson, Oct 19 1995
        Added display of embedded data with -d flag etc
--- John Gibson, Oct 10 1995
        Added printing of active attribute
--- John Gibson, Jan 20 1995
        Added `\Nt` (trailing newline)
--- John Gibson, Feb 21 1992
        Rewritten to show graphics characters and attributes
 */
