/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/ved/vedbarchart.p
 > Purpose:         Draw horizontal bar charts in Ved
 > Author:          John Williams, Apr 12 1996
 > Documentation:   HELP * VEDBARCHART
 > Related Files:   Used by LIB * PROFILE_GRAPHICAL
 */

section;

compile_mode:pop11 +strict;

global vars
    vedbarchart_thickness   =   2,
    vedbarchart_gap         =   1,
    vedbarchart_paint       =   `\{7}\s`,
    vedbarchart_label_attr  =   `\{b}`,
    ;


define lconstant Bar_strings(width, paint, label);
    lvars first = true;
    repeat vedbarchart_thickness times
        consdstring
            (#| repeat width times paint endrepeat;
                if first and label then
                    `\s`, `\s`;
                    appdata(label, nonop || (% vedbarchart_label_attr %))
                endif
            |#);
        false -> first
    endrepeat
enddefine;


define lconstant Chart_strings(header, values);
    lvars v, label, paint;
    if isprocedure(header) then
        header()
    elseif isstring(header) then
        header
    endif;
    for v in values do
        false ->> paint -> label;
        if iscompound(v) then
            if length(v) > 2 then v(3) -> paint endif;
            v(2) -> label;
            v(1) -> v
        endif;
        repeat vedbarchart_gap times nullstring endrepeat;
        Bar_strings(v, paint or vedbarchart_paint, label)
    endfor
enddefine;


define vedbarchart(name, header, values);

    define lconstant Get_buff(_);
        {% Chart_strings(header, values) %}
    enddefine;

    if vedpresent(name) then
        vededit(name);
        vedqget(vededit(% consref({^name ^Get_buff}), vedhelpdefaults %))
    else
        vededit(consref({^name ^Get_buff}), vedhelpdefaults)
    endif
enddefine;


define vedbarchart_scale(values) -> values;
    lvars v, l, max_v = 0, max_l = 0, scale, pair;
    if isboolean(values) then
        if values then copydata() endif -> values
    endif;
    for v in values do
        front(values) -> v;
        if iscompound(v) then
            datalength(v(2)) + 2 -> l;      ;;; two spaces before label ...
            v(1) -> v
        else
            0 -> l
        endif;
        if v > max_v then v -> max_v endif;
        if l > max_l then l -> max_l endif
    endfor;
    if max_v > (vedscreenwidth - 2 - max_l) then
        (vedscreenwidth - 2 - max_l) / max_v -> scale;
        for pair on values do
            front(pair) -> v;
            if iscompound(v) then
                round(v(1) * scale) -> v(1)
            else
                round(v * scale) -> front(pair)
            endif
        endfor
    endif
enddefine;


endsection;
