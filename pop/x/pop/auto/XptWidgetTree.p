/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptWidgetTree.p
 > Purpose:         Generate hierarchy of widgets
 > Author:          Tom Khabaza, Ian Rogers, Apr 24 1991
 > Documentation:   REF *XptWidgetTree
 > Related Files:   LIB *XptChildren
 */
compile_mode :pop11 +strict;

section;

define XptWidgetTree(widget);
    lvars c, widget;
    [% widget, for c in XptChildren(widget) do XptWidgetTree(c) endfor %];
enddefine;

endsection;
