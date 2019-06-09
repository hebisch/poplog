/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptLoadWidgetSet.p
 > Purpose:         Load a complete widget set in one batch
 > Author:          Jonathan Meyer, Nov 30 1990 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING      ;;; redundant for POPC

uses exload;
uses XptWidgetSet;

section $-typespec_utils => XptLoadWidgetSet;

define global constant XptLoadWidgetSet(name);
    lvars name ws widget members memspace;
    dlocal exload_isbatching = true; ;;; turn on exload batching

    if name.islist then
        for members in name do XptLoadWidgetSet(members) endfor;
        return;
    endif;
    unless name.isword then
        mishap(name,1,'WORD NEEDED');
    endunless;

    ;;; ensure there is enough memory
    popmemlim - popmemused -> memspace;
    2 ** 28 -> popmemlim;

    ;;; fetch the widget set
    XptWidgetSet(name) -> ws;
    if (ws("WidgetSetMembers") ->> members) == [] then
        mishap(name,1,'Cannot find any widgets to load for this widgetset');
    else
        for widget in members do
            ws(widget)->;
        endfor;
    endif;

    min(2**28, memspace + popmemused + 50000) -> popmemlim;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, May 23 1991
        -exload_isbatching- now an active variable.
--- Jonathan Meyer, Feb  9 1991 Added better guard to popmemlim
 */
