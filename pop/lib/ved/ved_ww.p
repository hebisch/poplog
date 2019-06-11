/* --- Copyright University of Birmingham 1997. All rights reserved. ------
 > File:            $poplocal/local/auto/ved_ss.p
 > Purpose:         Caseless ved searching
 > Author:          Aaron Sloman, Oct 30 1994 (see revisions)
 > Documentation:   HELP VED_SS
 > Related Files:   REF * VEDSEARCH, TEACH * VEDSEARCH
 */

section;

compile_mode :pop11 +strict;

global vars
    vedss_searchstring = false;

define lconstant vedcaseless_search(proc);
    ;;; This procedure localises ved_search_state and ensures that
    ;;; if no argument is given then the last value of vedss_searchstring
    ;;; will be used

    lvars proc;

    dlocal ved_search_state, vedargument, ved_on_status;

    if vedargument = nullstring then
        unless isstring(vedss_searchstring) then
            vederror('NO SEARCH STRING')
        endunless;
        vedss_searchstring -> vedargument;
        true -> ved_on_status;
        vedtextright();
        vedcharright();
        vedinsertstring(vedss_searchstring);
        false -> ved_on_status;
    else
        vedargument -> vedss_searchstring
    endif;
    proc();
enddefine;


define global vars ved_ss();
    vedcaseless_search(ved_search_or_substitute(%'-case', undef%))
enddefine;

define global vars ved_ww();
    vedcaseless_search(ved_search_or_substitute(%'-case -embed', undef%))
enddefine;


;;; backward versions
define global vars ved_bss();
    vedcaseless_search(ved_search_or_substitute(%'-case +back -wrap', undef%))
enddefine;

define global vars ved_bww();
    vedcaseless_search(
        ved_search_or_substitute(%'-case +back -wrap -embed', undef%))
enddefine;

define global vars ved_ssr();
    vedcaseless_search(
        ved_search_or_substitute(%'range -case -wrap', undef%))
enddefine;

define global vars ved_wwr();
    vedcaseless_search(
        ved_search_or_substitute(%'range -case -wrap -embed', undef%))
enddefine;

define global vars ved_bssr();
    vedcaseless_search(
        ved_search_or_substitute(%'range +back -case -wrap', undef%))
enddefine;

define global vars ved_bwwr();
    vedcaseless_search(
        ved_search_or_substitute(%'range +back -case -wrap -embed', undef%))
enddefine;

define global vars ved_ssp();
    vedcaseless_search(
        ved_search_or_substitute(%'procedure -case -wrap', undef%))
enddefine;

define global vars ved_wwp();
    vedcaseless_search(
        ved_search_or_substitute(%'procedure -case -wrap -embed', undef%))
enddefine;

define global vars ved_bssp();
    vedcaseless_search(
        ved_search_or_substitute(%'procedure +back -case -wrap', undef%))
enddefine;

define global vars ved_bwwp();
    vedcaseless_search(
        ved_search_or_substitute(%'procedure +back -case -wrap -embed', undef%))
enddefine;



endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Oct 24 1997
    Moved documentation to HELP * VED_SS
--- Aaron Sloman, Oct 24 1994
    Changed to use regexp facilities.
*/
