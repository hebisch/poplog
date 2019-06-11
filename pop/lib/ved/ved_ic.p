/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_ic.p
 >  Purpose:        used to insert a character into the current location
 >  Author:         A.Sloman 1982 (see revisions)
 >  Documentation:  HELP * IC , HELP VEDCOMMS/ved_ic
 >  Related Files:  LIB * ved_cc
 */
compile_mode :pop11 +strict;

;;; USAGE:
;;;
;;;     ENTER ic <character>
;;;
;;; where <character> can be an integer or a character constant, or something
;;; valid as a character constant when prefixed with \. <character> can also
;;; be CTRL X to mean Ctrl-X, or DEL or ESC.

section;

define ved_ic;
    lvars c, data;
    if vedargument = nullstring then
        ved_cc();
        return
    elseif vedargument = 'dd' then
        ;;; delete any embedded data
        vedcurrentdchar() -> vedcurrentvedchar();
        return
    elseif isstartstring('d=', vedargument)
    or isstartstring('d:', vedargument) then
        ;;; set embedded data to string/expression
        if skipchar(`\s`,3,vedargument) ->> c then
            allbutfirst(c-1, vedargument)
        else
            nullstring
        endif -> data;
        if vedargument(2) == `:` then
            pop11_compile(stringin(data)) -> data;
            unless isstring(data) or isword(data) then
                vederror('ILLEGAL VALUE FOR EMBEDDED DATA')
            endunless
        endif;
        conspair(vedcurrentdchar(), data) -> vedcurrentvedchar();
        return
    endif;

    unless strnumber(vedargument) ->> c then
        ;;; try as a char constant with \ prefixed
        strnumber('`\\' <> vedargument <> '`') -> c
    endunless;

    unless c then
        ;;; ensure all upper case
        ncmapdata(vedargument,lowertoupper)->;
        if isstartstring('CTRL ', vedargument) then
            vedargument(6) && 16:1F
        elseif vedargument = 'DEL' then
            `\^?`
        elseif vedargument = 'ESC' then
            `\e`
        elseif vedargument = 'FF' or vedargument = 'FORM FEED' then
            `\^L`
        else
            vederror('USAGE: ic <character>');
        endif -> c
    endunless;

    if c == `\n` then
        vedcharinsert(`*`);
        c -> vedthisline()(vedcolumn - 1);
        vedrefreshrange(vedline,vedline,undef)
    else
        vedcharinsert(c);
    endif;
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Feb 13 1992
        Changed to test argument as a character constant with \ prefixed
--- Aaron Sloman, Oct 26 1986 made to move right after inserting ^M
        Removed struppercase
--- Mark Rubinstein, Jan 30 1986 - fixed to give a sensible error if the
    argument is not recognized (previously gave a STE mishap).
--- Aaron Sloman, March 1985 - modified to cope with inserting `\r` or `\n`
 */
