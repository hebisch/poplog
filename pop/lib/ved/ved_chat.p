/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_chat.p
 > Purpose:         Change character attributes
 > Author:          John Gibson, Feb 15 1992 (see revisions)
 > Documentation:   REF *VEDCOMMS
 */
compile_mode :pop11 +strict;

section;

define vars ved_chat;
    lvars c, args, scopechar, attr, prev, scope;
    dlocal  vedstatic, vvedpromptchar = false,
            ;;; insert active continuation markers across lines
            vedcharinsert_acont = true;

    lconstant COLOURMASK = `\[7]`;

    define lconstant change_mode(char);
        lvars char, org = char;
        if ispair(char) then fast_front(char) -> char endif;
        if attr &&/=_0 COLOURMASK and char &&/=_0 COLOURMASK then
            ;;; replace an existing colour with the new one first
            (char &&~~ COLOURMASK) || (attr && COLOURMASK) -> char
        endif;
        if c == `+` then
            char || attr
        elseif c == `-` then
            char &&~~ attr
        elseif c == `~` then
            char ||/& attr
        else
            (char && 16:FFFF) || attr
        endif;
        if ispair(org) then () -> fast_front(org), org endif
    enddefine;

    sysparse_string(vedargument, false) -> args;
    unless listlength(args) == 2
    and (dl(args) -> (scope, attr),
            member(scope, ['c' 'w' 'l' '-c' '-w' '-l' 'r' 's' 'f']))
    and attr /= nullstring
    then
        vederror('USAGE: chat <scope> <attributes>')
    endunless;

    if (subscrs(1,scope) ->> scopechar) == `-` then
        ;;; do previous entity
        subscrs(2,scope) -> scopechar;
        true
    else
        false
    endif -> prev;

    subscrs(1,attr) -> c;
    if c == `+` or c == `-` or c == `~` then
        allbutfirst(1, attr) -> attr
    endif;
    unless strnumber('`\\[' <> attr <> ']`') ->> attr then
        vederror('INVALID ATTRIBUTES')
    endunless;

    if scopechar == `c` then
        ;;; character
        if prev and vedcolumn /== 1 then vedcharleft() endif;
        if prev or vedcolumn <= vvedlinesize then
            true -> vedstatic;
            vedcharinsert(change_mode(vedcurrentvedchar()))
        else
            vednextline()
        endif

    elseif scopechar == `w` then
        ;;; word
        lvars go_next = false;
        if prev then
            if vedcolumn == 1 then
                vedcharup();
                vedtextright();
                true -> go_next;
            elseif vedcolumn > vvedlinesize then
                vedtextright()
            endif;
            if vedatitemstart(vedcolumn, vedthisline(), vvedlinesize) then
                vedstartwordleft()
            endif
        endif;
        vedconvertword(identfn, change_mode, 1, true);
        if go_next then vednextline() endif

    elseif scopechar == `l` then
        ;;; line
        if prev and vedline /== 1 then vedcharup() endif;
        vedconvertline(identfn, change_mode, 1)

    elseif scopechar == `s` then
        ;;; selection (XVed only)
        vedconvertseln(identfn, change_mode)

    elseif scopechar == `r` then
        ;;; marked range
        vedconvertrange(identfn, change_mode)

    else
        ;;; whole file
        vedmarkpush(), false -> vvedmarkprops;
        ved_mbf(), ved_mef();
        vedconvertrange(identfn, change_mode);
        vedmarkpop()
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug  6 1996
        Added f option for whole file
--- John Gibson, Nov  3 1995
        Added dlocal vedcharinsert_acont
--- John Gibson, Oct 30 1995
        Changed to use vedcurrentvedchar
--- John Williams, Oct 26 1994
        Added s option (current Xved selection) as per BR adrianh.74
--- John Gibson, Dec  1 1993
        Fixed -w case when before first word on line
--- John Gibson, Jan  7 1993
        Added `~` option on attributes
--- John Gibson, Jun 23 1992
        Changed to allow -c, -w, -l for scope to mean do previous entity
        rather than current.
--- Simon Nichols, Mar 20 1992
        Changed error message to say "chat" rather than "ca".
 */
