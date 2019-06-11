/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_newindex.p
 > Purpose:         Create a table of contents for a documentation file
 > Author:          John Gibson, Feb  6 1993 (see revisions)
 > Documentation:   REF * ved_newindex
 */
compile_mode :pop11 +strict;

section $-ved => ved_newindex;

uses ved_g;

lconstant index_header =
'         \{b}CONTENTS - (Use <ENTER> g to access required sections)'
;

define lconstant get_header_line(vline, index_sp, last_ul) -> last_ul;
    lvars   n, m, c, t, vline, string = subscrv(vline,vedbuffer),
            len = datalength(string), indent, textstart, last_ul,
            num, dot = false, index_sp, ul_string, ul_len, itext_len, itext,
            hlen, save_last_ul;

    returnif(len == 0);
    if isnumbercode(fast_subscrs(1,string)) then
        for n from 2 to len do
            fast_subscrs(n,string) -> c;
            quitunless(isnumbercode(c) or c == `.`);
            if c == `.` then true -> dot endif
        endfor;
        n-1 -> n;
        if dot then
            returnif(n > 5);    ;;; DD.DD max
            6, 13
        else
            returnif(n > 2);    ;;; DD max
            3-n, 7
        endif
    elseif isstartstring('...', string) then
        3 -> n;
        12, 17
    else
        return
    endif -> (indent, textstart);
    n+1 -> m;
    returnunless((skipchar(`\s`,m,string) ->> t) and t /== m);

    last_ul -> save_last_ul;
    vline+1 -> last_ul;
    subscrv(vline+1,vedbuffer) -> ul_string;
    vedusedsize(ul_string) -> ul_len;

    max(0, min(len, ul_len)-(t-1)) -> itext_len;
    subdstring(t,itext_len,string) -> itext;
    if vedusedsize(itext) /== itext_len then
        vedusedsize(itext) -> itext_len;
        subdstring(1,itext_len,itext) -> itext
    endif;
    subdstring(1,n,string) -> num;
    n+2+itext_len -> hlen;

    ;;; leave index line(s) on stack
    define lconstant act_explode =
        appdata(% nonop ||(%`\[A]`%) %)
    enddefine;

    datalength(num) -> n;
    if index_sp and textstart == 7 then nullstring endif;   ;;; major heading
    consvedstring(#|
        dupnum(`\[A]\s`, indent);
        act_explode(num);
        conspair(subscr_stack(n), '(SN)g') -> subscr_stack(n);
        dupnum(`\[A]\s`, textstart-(m+indent));
        act_explode(itext)
    |#);

    ;;; update buffer line, plus graphics underline/overline
    consvedstring(#|
        act_explode(num);
        conspair(subscr_stack(n), '(S)g;;;back to index') -> subscr_stack(n);
        dup(`\[A]\s`);
        explode(itext),
        explode(allbutfirst(min(len,hlen), string))
    |#) -> subscrv(vline,vedbuffer);

    returnif(ul_len >= 70);         ;;; leave full-width underline

    consdstring(dupnum(subscrdstring(1,ul_string), hlen), hlen) ->> ul_string
                                    -> subscrv(vline+1,vedbuffer);
    vline-1 -> vline;
    if vline > 0 and vline /== save_last_ul
    and isstartstring('\G-\G-\G-',subscrv(vline,vedbuffer))
    then
        copy(ul_string) -> subscrv(vline,vedbuffer)
    endif
enddefine;

define vars ved_newindex;
    lvars   n, m, line, list, index_sp = undef, index_had_sp = false,
            oldchanged = vedchanged, thisline = vedline, last_ul;
    dlocal  vvedmarkprops, vedpositionstack, vedautowrite = false;

    if vedargument = 'nosp' then
        false -> index_sp
    elseif vedargument = 'sp' then
        true -> index_sp
    endif;

    vedpositionpush();
    vedmarkpush(); false -> vvedmarkprops; vedmarkpush();

    ;;; remove existing index entries if they exist
    vedtopfile();
    if find_new_indexline() then
        vedmarklo(); vedline -> thisline;
        0 -> n;
        repeat
            vednextline();
            if vvedlinesize == 0 then
                n+1 -> n
            else
                quitunless(is_new_indexline(vedthisline()));
                if n /== 0 then true -> index_had_sp endif;
                0 -> n
            endif
        endrepeat;
        repeat n times vedcharup() endrepeat;
        vedmarkhi();
        ved_d();
        vedjumpto(thisline, 1);
        vedcharup(); vedcharup();
        if index_sp == undef then index_had_sp -> index_sp endif
    elseif vedteststartsearch(index_header) ->> line then
        vedjumpto(line,1)
    else
        ;;; Assume header should go below current line
        thisline -> vedline; vedsetlinesize();
        vedlinebelow(); copy(index_header) -> vedthisline();
        vedrefreshrange(vedline,vedline,false);
    endif;

    vedpositionpush();
    ;;; collect section header lines
    vedtopfile();
    0 -> last_ul;
    [%  unless index_sp then nullstring endunless;
        for n from 2 to vvedbuffersize do
            nextunless(isstartstring('\G-\G-\G-',subscrv(n,vedbuffer)));
            get_header_line(n-1, index_sp, last_ul) -> last_ul
        endfor
    %] -> list;

    ;;; Suppress printing of whole new index
    dlocal vedediting = false;
    vedpositionpop();  vedpositionpush();
    for line in list do
        vedlinebelow();
        line -> vedthisline();
    endfor;
    vednextline();
    unless vvedlinesize == 0 then vedlineabove() endunless;
    vedmarkpop(); vedpositionpop();
    if oldchanged then oldchanged + 1 else 1 endif -> vedchanged;
    chain(vedalignscreen)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 22 1996
        get_header_line now makes index lines (and the section number part of
        each header line) be text actions with command <ENTER> g.
--- John Gibson, Jan 31 1995
        Changed so that index lines are truncated to the width of underlining
        given for the header (thus non full-width headers can have stuff
        to the right of the underlining which will not appear in the index
        line)
--- John Gibson, Sep 21 1993
        Changed get_header_line to allow max 5 chars in minor heading
        number part instead of 4 (i.e. allows DD.DD)
--- John Gibson, May 21 1993
        ... header lines now have 2 spaces between the ... and the text
--- John Gibson, May  7 1993
        Added missing uses for ved_g
 */
