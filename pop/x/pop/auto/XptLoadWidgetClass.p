/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptLoadWidgetClass.p
 > Purpose:         Loading widgetclasses
 > Author:          John Gibson, Mar 30 1993 (see revisions)
 > Documentation:   REF * XT_LIBS;
 */
compile_mode:pop11 +strict;

include xpt_constants.ph;

section;

uses
    exload,
    widgetclass_key,
;
uses-by_name from widgetclass_key (XptPopImportWidgetClassPtr);

define macro XptLoadWidgetClass;
    lvars   n, len, wcs, item, mark, objfiles, declarator, idname, wc, pair,
            wsprefix, attributes, p;
    dlocal pop_autoload = false;

    $-typespec_utils$-get_exload_initial("XLINK_EXLIBS", true)
                                -> (mark, objfiles, declarator, attributes);

    [%  repeat
            itemread() -> item;
            quitif(item == ";");
            if item == termin then mishap(0,'UNEXPECTED END OF INPUT'); endif;
            nextif(item == ",");
            item -> idname;
            fast_word_string(item) -> item;
            datalength(item) -> len;
            fast_for n to len do
                quitunless(islowercode(subscrs(n,item)))
            endfor;
            if (allbutlast(len-n+1,item) ->> wsprefix) /= nullstring then
                lowertoupper(subscrs(1,wsprefix)) -> subscrs(1,wsprefix)
            endif;
            class_cons(widgetclass_key)(
                conspair(XDT_WIDGETCLASS, allbutfirst(n-1,item)),
                null_external_ptr,
                false,
                wsprefix
            ) -> wc;

            true -> pop_autoload;
            sys_current_val("XptPopImportWidgetClassPtr") -> p;
            false -> pop_autoload;
            conspair(wc, p(%wc%)) -> pair;

            {% idname, pair, if pop11_try_nextitem("<-") then
                                readitem()  ;;; external name
                             else
                                idname sys_>< 'Class'
                             endif %}
        endrepeat
    %] -> wcs;

    [   exload ^mark ^objfiles ^^attributes (type data)
        %   if declarator then declarator endif,
            for item in wcs do
                explode(item) -> (idname, pair, item);
                idname, "#_<", pair, ">_#", "<-", item, ","
            endfor
        %
        endexload;
    ] nc_<> proglist -> proglist;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1996
        Added (type data) attribute to exload
--- John Gibson, May 10 1993
        Changed to use XLINK_EXLIBS instead of XTB*ASELIBS
 */
