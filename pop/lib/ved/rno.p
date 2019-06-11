/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 >  File:           C.all/lib/ved/rno.p
 >  Purpose:        some rno commands for use with lib PAGE
 >  Author:         Roger Evans, Apr 1983 (see revisions)
 >  Documentation:  HELP * PAGE
 >  Related Files:  LIB * PAGE
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
commands in this package:

    .header     specify header in file (just like <enter> header)
    .footer     like .header
    .sp         blank line - optional integer follows for multiple blank lines

    .tidy       brackets for ved_j tidying
    .endtidy

    .fill       brackets for ved_jj tidying
    .endfill

    .ul         brackets for ved_ul underlining
    .endul

    .title      text on remainder of line is inserted and underlined

    .ctitle     remaining text on line is inserted, centred and underlined

*/

vars rno; if isundef(rno) then undef -> rno endif;

;;;uses page;

/* ************************************************************************
RNORANGE is a utility for bracket-type rno commands.  It has a rather crude
action at present.  E.g.  with the following:

.tidy
text..........
.sp 3
text ..........
.endtidy

The first .tidy looks for its matching .endtidy (or for the end of buffer if
there isn't one); when it encounters the .sp it inserts a .tidy command below
it, then goes back and tidies the text above it; the .sp command is then
obeyed; and finally the remaining text is tidied.  All this is necessary so
that the calculation of page placings is not thrown out of joint.

NB if there is no matching end bracket the command is assumed to hold good for
the rest of the file
*************************************************************************** */

define rnorange(proc, startstring, endstring);
    vars vedscreenmark;
    '.' >< endstring -> endstring;
    vedmarkpush();
    `\s` -> vedscreenmark;
    vedmarklo();
    until vedatend() do
        if vedlinestart('.') then
            if vedthisline() = endstring then
                vedlinedelete()
            else
                vedlinebelow();
                1 -> vedcolumn;
                vedinsertstring('.' >< startstring);
                vedcharup()
            endif;
            quitloop
        else
            vednextline()
        endif
    enduntil;
    vedcharup();
    vedmarkhi();
    proc();
    vedjumpto(vvedmarklo + 1, 1);
    vedmarkpop();
    vedcheck()
enddefine;

define rno_header(vedargument);
    ved_header()
enddefine;

define rno_footer(vedargument);
    ved_footer()
enddefine;

define rno_sp(n);
    if n = vednullstring then 1 -> n
    elseunless (strnumber(n) ->> n) then rno_error();
    endif;
    repeat n times vedlineabove() endrepeat;
enddefine;

define rno_tidy();
    rnorange(ved_j, 'tidy', 'endtidy')
enddefine;

define rno_endtidy();
    /* if obey_rno() will only call this if there isn't a matching '.tidy' */
    rno_error()
enddefine;

define rno_fill();
    rnorange(ved_jj, 'fill', 'endfill')
enddefine;

define rno_endfill();
    rno_error()
enddefine;

define rno_ul();
    rnorange(ved_ul, 'ul', 'endul')
enddefine;

define rno_endul();
    rno_error()
enddefine;

define rno_title(text);
dlocal vedbreak = false;
    vars n m;
    vedlineabove();
    vedinsertstring(text);
    datalength(text) -> n;
    0;
    for m from 1 to n do
        if subscrs(m,text) == ` ` then ` ` else `_` endif;
    endfor;
    vedinsertstring(consstring(n+1));
    13 -> subscrs(n+1,vedthisline());   ;;; must do this AFTER vedinsertstring
    vedscreenleft();
enddefine;

define rno_ctitle(text);
dlocal vedbreak = false;
    vars n m;
    vedlineabove();
    vedinsertstring(text);
    vedmarkhi(); vedmarklo();
    ved_ac();
    vedtextright();
    vedthisline() -> text;
    datalength(text) -> n;
    0;
    for m from 1 to n do
        if subscrs(m,text) == ` ` then ` ` else `_` endif;
    endfor;
    vedinsertstring(consstring(n+1));
    13 -> subscrs(n+1,vedthisline()); ;;; must do this AFTER vedinsertstring
    vedscreenleft();
enddefine;

/*  --- Revision History ---------------------------------------------------
--- Julian Clinton, April 1991 - added fix for long .title bug (isl-fr.4311)
--- David Roberts, August 1983 - modified
 */
