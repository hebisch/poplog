/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/auto/exval_to_popobj.p
 > Purpose:         Exval <-> pop object or string converters
 > Author:          John Gibson, Sep  3 1992 (see revisions)
 > Documentation:   REF *EXTERNAL_DATA
 */
compile_mode :pop11 +strict :vm -pentch;

section;

    ;;; property to hold on to fixed copies of objects
lconstant cache = newproperty([], 32, false, "tmparg");

define exval_to_popobj(/*exptr*/) with_nargs 1;
    ;;; just copy the external ptr
    copy()
enddefine;
;;;
define updaterof exval_to_popobj(obj);
    lvars obj, cached = cache(obj);
    if cached then
        cached
    elseif class_attribute(datakey(obj), "external_noconv")
    and not(is_fixed(obj)) then
        ;;; structure passed directly to external call or put straight into
        ;;; "exval" field -- must make fixed version
        copy_fixed(obj) ->> cache(obj)
    else
        obj
    endif
enddefine;

define exval_to_string(/*exptr*/) with_nargs 1;
    lvars string = exacc_ntstring();
    ;;; exacc_ntstring returns termin for a NULL -- we want false
    string /== termin and string
enddefine;
;;;
define updaterof exval_to_string(string);
    lvars string, cached;
    if string then
        check_string(string);
        if cache(string) ->> cached then
            cached
        else
            sys_encode_string_fixed(string);
            if dup() /== string then dup() -> cache(string) endif
        endif
    else
        false
    endif
enddefine;

define exval_to_bytestring(/*exptr*/) with_nargs 1;
    lvars string = exacc_ntstring((), false);           ;;; no decoding
    ;;; exacc_ntstring returns termin for a NULL -- we want false
    string /== termin and string
enddefine;
;;;
define updaterof exval_to_bytestring(string);
    lvars string, cached;
    if string then
        check_string(string);
        if cache(string) ->> cached then
            cached
        else
            sys_encode_string_fixed(string, false);     ;;; no encoding
            if dup() /== string then dup() -> cache(string) endif
        endif
    else
        false
    endif
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 13 1997
        Changed exval_to_string updater to use sys_encode_string_fixed;
        added exval_to_bytestring.
 */
