/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/auto/fast_XptValue.p
 > Purpose:         Access and update widget resources by name
 > Author:          Tom Khabaza, Aug 14 1990, Jonathan Meyer, Sept 7 1990 (see revisions)
 > Documentation:   REF *XPT_RESOURCE
 > Related Files:   LIB *XptPopValue LIB *XptValue
 */
compile_mode :pop11 +strict;

uses $-Xpt$-ConsAccess;

section;

lconstant
    ;;; size of a value buffer in BYTES
    ;;; (NB: no checking for overflow, but all existing ones seem to be
    ;;; 1 word anyway ...)
    macro RESOURCE_BYTE_SIZE = SIZEOFTYPE(:dfloat),

    ;;; default coercion type
    DEFAULT_COERCE  = "int",
;


define lconstant Cached_coerce_info(spec) -> p;
    lvars spec, p;
    ;;; cache for coercion procedures
    define lconstant coercecache = newproperty([],16,false,false) enddefine;

    unless coercecache(spec) ->> p then
        ;;; build a new type access procedure -- the updater of this
        ;;; just returns the converted field value
        $-Xpt$-ConsAccess(spec) ->> p -> coercecache(spec)
    endunless
enddefine;

define fast_XptValue() with_nargs 3;
    lvars widget, name, coerce, vptr;
    ;;; check for optional arguments
    if isstring(dup()) then DEFAULT_COERCE endif -> (widget, name, coerce);

    ;;; fetch coercion proc and size
    Cached_coerce_info(coerce) -> coerce;

    ;;; get value and coerce it
    initexptr_mem(RESOURCE_BYTE_SIZE) -> vptr;
    fast_XtVaGetValues(widget, name, vptr, 2);
    fast_apply(vptr,coerce);
    sys_grbg_fixed(vptr);
enddefine;

define updaterof fast_XptValue() with_nargs 4;
    lvars data, widget, name, coerce;
    ;;; check for optional arguments
    if isstring(dup()) then DEFAULT_COERCE endif -> (data,widget,name,coerce);

    ;;; fetch coercion proc -- updater just returns the converted field value
    Cached_coerce_info(coerce) -> coerce;
    fast_XtVaSetValues( widget, name, data -> coerce(), 2); ;;; 2 arguments
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 12 1995
        Changed to use $-Xpt$-ConsAccess to produce access procedure.
--- John Gibson, Sep  6 1992
        Changed to use initexptr_mem and sys_grbg_fixed
--- Jonathan Meyer, Apr 21 1992
        o Changed detection of optional argument to be more efficient
        o Made XptValue use a free list of pointers for
          storing the result, so that it is re-entrant.
        o Changed size(data) to fast_apply(data, size), destpair to
          fast_destpair, and exacc :uint valueptr to exacc [fast] ...
        o Removed code to support R3 toolkit (no longer any hosts
          that use it)
--- Adrian Howard, Nov  4 1991 : xt_general --> xpt_general
--- John Gibson, Nov  3 1991
        Uses xt_general instead of xpt_generaltypes
--- Jonathan Meyer, Aug 19 1991 tidies, removed string coercion to slow v.
--- Jonathan Meyer, Aug  2 1991 change to use exptr_init_fixed
--- Roger Evans, Feb  5 1991 substantially revised to get coercion right
--- Roger Evans, Nov 19 1990 trimmed down and re-installed
--- Jonathan Meyer, Oct 31 1990
        Added strict compile mode. Fixed variable declarations.
        Changed recognition of widget (done by -iswidget-) to test for
        not(words or strings). This allows fast_XptValue to be used with
        external pointer class objects in the widget position.
*/
