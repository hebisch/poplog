/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/lib/prolog_subsystem.p
 > Purpose:         Subsystem entries for "prolog" and "top"
 > Author:          John Gibson, Jan 12 1993 (see revisions)
 > Documentation:   REF *SUBSYSTEM
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF prolog_subsystem

section;

uses prologliblist;

;;; Convert a Unix directory path to OS-specific format
define lconstant macro DIR path;
    lvars path;
    path dir_>< nullstring;
enddefine;

vars
    prolog_helpdirs = [%
        DIR '$poplocal/local/plog/help/',
        DIR '$usepop/pop/plog/help/',
    %],
    prolog_teachdirs = [%
        DIR '$poplocal/local/plog/teach/',
        DIR '$usepop/pop/plog/teach/',
    %],
    prolog_refdirs = [%
    %],
    prolog_libdirs = [%
        ident prologliblist
    %],
    prolog_srcdirs = [%
        DIR '$usepop/pop/plog/src/',
    %],
;

lconstant
    prolog_search_lists =
        [
            vedhelpname   [[% ident prolog_helpdirs % help "prolog]
                           [% ident prolog_teachdirs % teach "prolog]]
            vedteachname  [[% ident prolog_teachdirs % teach "prolog]
                           [% ident prolog_helpdirs % help "prolog]]
            vedrefname    [[% ident prolog_refdirs % ref "prolog]]
            vedsrcname    [[% ident prolog_srcdirs % src]]
            vedlibname    [% ident prolog_libdirs %]
        ],
;

subsystem_add_new(
            "prolog",
            "prolog_subsystem_procedures",
            '.pl',
            '| ',
            prolog_search_lists,
            'Prolog'
);

subsystem_add_new(
            "top",
            "top_subsystem_procedures",
            '.pl',
            '?- ',
            prolog_search_lists,
            'Prolog'
);

constant
    top_subsystem       = "top",
    prolog_subsystem    = "prolog",
;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 22 1994
        Reversed the creation order of the subsystems "top" and "prolog"
        so that the "prolog" subsystem takes precedence.
--- Robert John Duncan, Jul 14 1993
        Moved all the option-processing stuff back to the "prolog.p" file
        for use only when building the Prolog system and simplified the
        initial definitions of searchlist vars. Added uses prologliblist.
--- John Gibson, Apr 26 1993
        Uses subsystem_add_new
 */
