/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/vvedsrchstring.p
 > Purpose:         Backward compatability for VED searching mechanism
 > Author:          Jonathan Meyer, Sep 28 1993
 > Documentation:   REF *OBSOLETE
 */
#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +strict;
section;

include vedsearchdefs;

define active vvedsrchstring;
    subscrv(VEDSRCH_SEARCH_STRING, vedsearchdata);
enddefine;
;;;
define updaterof active vvedsrchstring(str); lvars str;
    unless str == false or str.isstring then
        mishap(str, 1, 'STRING or -false- NEEDED');
    endunless;
    if str /== subscrv(VEDSRCH_SEARCH_STRING, vedsearchdata) then
        str -> subscrv(VEDSRCH_SEARCH_STRING, vedsearchdata);
        false -> subscrv(VEDSRCH_SEARCH_P, vedsearchdata);
    endif;
enddefine;

;;; we remove the distinction between vvedsrchstring and vvedoldsrchdisplay
identof("vvedsrchstring") -> identof("vvedoldsrchdisplay");

define active vvedsrchsize;
    if vvedsrchstring then datalength(vvedsrchstring) else 0 endif;
enddefine;
;;;
define updaterof active vvedsrchsize with_nargs 1;
    () -> ;
enddefine;

define active vvedinstring;
    subscrv(VEDSRCH_SUBSTITUTE_STRING, vedsearchdata);
enddefine;
;;;
define updaterof active vvedinstring(str); lvars str;
    unless str == false or str.isstring then
        mishap(str, 1, 'STRING or -false- NEEDED');
    endunless;
    if str /== subscrv(VEDSRCH_SUBSTITUTE_STRING, vedsearchdata) then
        str -> subscrv(VEDSRCH_SUBSTITUTE_STRING, vedsearchdata);
        false -> subscrv(VEDSRCH_SEARCH_P, vedsearchdata);
    endif;
enddefine;

define active vvedanywhere;
    subscrv(VEDSRCH_ANYWHERE, vedsearchdata);
enddefine;
;;;
define updaterof active vvedanywhere with_nargs 1;
    () -> subscrv(VEDSRCH_ANYWHERE, vedsearchdata);
enddefine;

define active vedfoundline -> l;
    lvars (,,,l,) = ved_query_last_search();
enddefine;
;;; not sure if its wise to give the updater as well...
define updaterof active vedfoundline(n);
    lvars n, r;
    if (subscrv(VEDSRCH_RESULT, vedsearchdata) ->> r).isvector then
        n -> subscrv(3, r);
    endif;
enddefine;

define active vedfoundcol -> c;
    lvars (,,,,c) = ved_query_last_search();
enddefine;
;;; not sure if its wise to give the updater as well...
define updaterof active vedfoundcol(n);
    lvars n, r;
    if (subscrv(VEDSRCH_RESULT, vedsearchdata) ->> r).isvector then
        n -> subscrv(4, r);
    endif;
enddefine;

endsection;
