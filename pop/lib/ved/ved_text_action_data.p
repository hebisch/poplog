/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_text_action_data.p
 > Purpose:         Find embedded data in active text
 > Author:          John Gibson, Nov  4 1995
 > Documentation:   REF * VEDPROCS
 */
compile_mode :pop11 +strict;

section;

define ved_text_action_data(line, col) -> (data, line, col);
    lvars   Col = col, char, start = false, Line = line, string = line,
            data = false, len, line, col;

    false ->> line -> col;
    unless isstring(Line) then
        returnif(Line > vvedbuffersize);
        subscrv(Line,vedbuffer) -> string
    endunless;
    datalength(string) -> len;
    returnunless(0 < Col and Col <= len);

    while fast_subscrdstring(Col,string) &&/=_0 `\[A]` do
        quitif((Col ->> start) == 1);
        Col fi_- 1 -> Col
    endwhile;
    returnunless(start);

    ;;; find first char in active segment with associated data
    start -> Col;
    while Col fi_<= len do
        fast_subscrvedstring(Col,string) -> char;
        if ispair(char) then
            sys_grbg_destpair(char) -> (char, data);
            if char &&/=_0 `\[A]` then
                Col -> col
            else
                false -> data
            endif;
            quitloop
        else
            quitunless(char &&/=_0 `\[A]`);
            char fi_&& 16:FFFF -> char;
            unless col or char == `\s` or char == `\t` then
                Col -> col
            endunless;
            Col fi_+ 1 -> Col
        endif
    endwhile;

    Line -> line;
    returnif(data /= nullstring);

    ;;; nullstring continuation marker -- look for preceding active segment
    start -> Col;
    repeat
        while Col fi_> 1 do
            Col fi_- 1 -> Col;
            fast_subscrdstring(Col,string) -> char;
            if char &&/=_0 `\[A]` then
                chain(Line, Col, ved_text_action_data)
            else
                char fi_&& 16:FFFF -> char;
                returnunless(char == `\s` or char == `\t`)
            endif
        endwhile;
        returnif(isstring(Line) or Line == 1);
        Line fi_- 1 -> Line;
        subscrv(Line,vedbuffer) -> string;
        vedusedsize(string) fi_+ 1 -> Col
    endrepeat
enddefine;

endsection;
