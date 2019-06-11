/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_prolog.p
 > Purpose:         View Prolog documentation & libraries outside Prolog
 > Author:          Rob Duncan, Oct 11 1990 (see revisions)
 > Related files:   C.all/plog/src/plogved.p
 */
compile_mode :pop11 +strict;

uses prolog_subsystem;

section;

define vars ved_prolog =
    veddo_in_subsystem(% "prolog" %)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 12 1993
        Now just uses new lib prolog_subsystem and veddo_in_subsystem
--- Robert John Duncan, Apr  6 1992
        Added src directories; included help directories in the teach list
        and vice versa.
--- John Williams, Oct 15 1990
        Also uses -define_dummy_subsystem-
 */
