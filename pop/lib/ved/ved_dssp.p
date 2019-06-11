/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_dssp.p
 > Purpose:         Display special space characters in the current file
 > Author:          John Gibson, Apr 20 1993
 > Documentation:   REF * VEDCOMMS
 */
compile_mode :pop11 +strict;

lconstant special_spaces = [
    {%  ident vedscreennobreakspacemark,    `\G.`     %}
    {%  ident vedscreenformatspacemark,     `\[b5]f`  %}
    {%  ident vedscreenpromptspacemark,     `\[b7]p`  %}
    {%  ident vedscreentrailspacemark,      `\[b3]S`  %}
];

define vars ved_dssp;
    lvars ss, id, set;
    if vedargument = 'on' then
        true
    elseif vedargument = 'off' then
        false
    else
        ;;; toggle current state
        not(is_vedfile_local(hd(special_spaces)(1), ved_current_file))
    endif -> set;

    for ss in special_spaces do
        ss(1) -> id;
        set -> is_vedfile_local(id, ved_current_file);
        if set then ss(2) -> idval(id) endif
    endfor;

    if vedfileisonscreen(ved_current_file) then
        vedrefresh();
        vedputmessage(if set then 'SPECIAL SPACES DISPLAYED'
                      else 'SPECIAL SPACES NOT DISPLAYED'
                      endif)
    endif
enddefine;
