/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpt_atomcache.p
 > Purpose:         Gets atoms/atom names, caching results
 > Author:          Jonathan Meyer, Feb 15 1991 (see revisions)
 > Documentation:   REF *XPT_ATOMCACHE
 > Related Files:
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_constants;

XptLoadProcedures xpt_atomcache
    lvars
        XFree
        XInternAtom
        XGetAtomName;

lconstant per_display = newassoc([]);

define lconstant Coerce_word(exptr);
    lvars exptr string;
    exacc_ntstring(exptr) -> string;
    if string == termin then
        false
    else
        ;;; free external string and build word
        exacc (1) raw_XFree(exptr);
        consword(string);
    endif;
enddefine;

define updaterof Coerce_word =
    fast_word_string <> updater(XptCoerceTmpString)
enddefine;


define XptInternAtom(dpy, name, only_if_exists) -> xatom;
    lvars dpy, name, only_if_exists, prop, xatom, known_name;
    l_typespec raw_XInternAtom(3):uint;

    ;;; type checking:
    XptCheckDisplayPtr(dpy) -> dpy;
    -> XptCoerceBoolean(only_if_exists) -> only_if_exists;
    if name.isstring then consword(name) -> name; endif;
    unless name.isword then mishap(name,1,'WORD NEEDED'); endunless;

    ;;; cached lookup
    returnif((per_display(dpy) ->> prop) and (prop(name) ->> xatom));

    ;;; new per_display prop
    unless prop then newassoc([]) ->> per_display(dpy) -> prop; endunless;

    ;;; Xlib lookup
    exacc raw_XInternAtom(dpy, ->Coerce_word(name), only_if_exists) -> xatom;
    returnif(xatom == 0)(false -> xatom);
    xatom -> prop(name);
enddefine;

define XptGetAtomName(dpy, xatom) -> name;
    lvars dpy xatom, known_xatom, name prop;
    l_typespec raw_XGetAtomName(2):exptr#Coerce_word;

    ;;; type checking
    XptCheckUnsignedInt(xatom) -> xatom;
    XptCheckDisplayPtr(dpy) -> dpy;

    ;;; cached lookup
    if per_display(dpy) ->> prop then
        fast_for name, known_xatom in_property prop do
            returnif(known_xatom == xatom);
        endfast_for;
    else
        newassoc([]) ->> per_display(dpy) -> prop;
    endif;

    ;;; Xlib lookup
    returnunless(exacc raw_XGetAtomName(dpy, xatom) ->> name);
    xatom -> prop(name);
enddefine;

constant xpt_atomcache = true;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- Jon Meyer, Aug  1 1991 Added xpt_atomcache = true for uses
 */
