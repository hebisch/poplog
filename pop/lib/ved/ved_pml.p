/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_pml.p
 > Purpose:         Gives access to PML documentation outside of PML
 > Author:          Rob Duncan, May 24 1990 (see revisions)
 > Related files:   C.all/pml/src/mlved.p
 */
compile_mode :pop11 +strict;

uses ml_subsystem;

section;

define vars ved_pml =
    veddo_in_subsystem(% "ml" %)
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 14 1993
        Now just uses new lib ml_subsystem and veddo_in_subsystem
--- Robert John Duncan, Apr  6 1992
        Added src directories; included help directories in the teach list
        and vice versa.
--- John Williams, Oct 15 1990
        Also uses -define_dummy_subsystem-
--- Robert John Duncan, Oct 11 1990
        Changed to use -veddo_in_dummy_subsystem-
--- Robert John Duncan, Aug  8 1990
        Revised for new LIB SUBSYSTEM
 */
