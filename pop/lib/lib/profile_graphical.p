/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/lib/profile_graphical.p
 > Purpose:         Modifies LIB PROFILE to display results as bar chart
 > Author:          John Williams, Apr 12 1996
 > Documentation:   HELP * PROFILE_GRAPHICAL
 > Related Files:   LIB * PROFILE, LIB * VEDBARCHART
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

compile_mode:pop11 +strict;

uses profile;
uses vedbarchart;

vars profile_graphical = true;

define vars profile_graph_paint(p);
    vedbarchart_paint
enddefine;


define profile_graph(cpu, gc, counts, _);
    lvars p, c, values, v, total = 0, n, pair, room;
    dlocal vedbarchart_label_attr = 0, vedbarchart_thickness = 1;
    lconstant BOLD = appdata(% nonop fi_||(% `\{b}` %) %),
              ITALIC = appdata(% nonop fi_||(% `\{i}` %) %);

    ;;; set up list of values
    if isproperty(counts) then
        [% for p, c in_property counts do {^c ^p ^pop_undef} endfor %]
    else
        ;;; undocumented feature used by Lisp interface to LIB PROFILE
        counts
    endif -> values;

    ;;; get total number of interrupts
    for v in values do v(1) + total -> total endfor;

    ;;; sort by count
    if profile_show_max > 0 then
        nc_listsort(values, procedure(x, y); x(1) > y(1) endprocedure)
    else
        []
    endif -> values;

    ;;; truncate to profile_show_max, create percentages, labels, paint
    1 -> n;
    for pair on values do
        front(pair) -> v;
        v(1) -> c;
        round(c / total * 100) ->> c -> v(1);
        v(2) -> p;
        unless isword(p) or isstring(p) then p >< nullstring -> p endunless;
        consdstring(#| BOLD(p), `\s`, `(`, dest_characters(c), `%`, `)` |#)
            -> v(2);
        if datalength(v) > 2 and v(3) == pop_undef then
            profile_graph_paint(p) -> v(3)
        endif;
        if n == profile_show_max then
            [] -> back(pair);
            quitloop
        endif;
        n + 1 -> n
    endfor;

    define lconstant Italic(s) -> s;
        if vedusewindows == "x" then
            consdstring(ITALIC(s), datalength(s)) -> s
        endif
    enddefine;

    ;;; draw the bar chart
    vedbarchart(
        'profile output',
        procedure();
            Italic('Number of interrupts: ' sys_>< total);
            Italic('CPU time: ' sys_>< (cpu / 100.0)
                    sys_>< ' seconds, GC time: '
                    sys_>< (gc / 100.0) sys_>< ' seconds')
        endprocedure,
        vedbarchart_scale(values))
enddefine;


define vars profile_report(cpu_time, gc_time, counts, active_counts);
    if isundef(profile_display) then
        ;;; new style printing
        if profile_graphical then
            profile_graph
        else
            profile_print
        endif (cpu_time, gc_time, counts, active_counts)
    else
        ;;; old style, kept for compatibility
        profile_display(cpu_time, counts);
    endif;
enddefine;


endsection;
