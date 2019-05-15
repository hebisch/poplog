/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:           C.all/plog/src/macros.p
 > Purpose:        Prolog: macro table
 > Author:         Jonathan Laventhol, 1985(?) (see revisions)
 > Documentation:  PLOGHELP * MACRO
 */

section prolog;

weak vars procedure ( $-handlepopescape );

;;; ========================================================================

/*
 *  Macro Definitions
 */

;;; prolog_handlepopescape:
;;;     macro/command for handling ESC (for PWM)

define prolog_handlepopescape();
    if testdef handlepopescape then
        weakref handlepopescape();
    else
        ;;; return some literal representation of the escape character,
        ;;; but not the same word or it'll be remapped as a macro
        consword('\\^[');
    endif;
enddefine;


/*
 *  Expanding Macros
 */

constant procedure prolog_getitem;  ;;; forward

vars
    prolog_expand_macros = true,
        ;;; flag to allow suppression of macro expansion (see "itemise.p")
;

;;; prolog_macro_apply:
;;;     expands -item- as a macro.

define prolog_macro_apply(item);
    lvars item, old_stack;
    consvector(stacklength()) -> old_stack;
    if isprocedure(item) then
        item(repeat pdnargs(item) times prolog_getitem() endrepeat);
    elseif islist(item) then
        explode(item);
    else
        item;
    endif;
    repeat stacklength() times :: proglist -> proglist endrepeat;
    explode(old_stack);
enddefine;


/*
 *  Macro Table
 */

;;; prolog_macro:
;;;     maps atoms to macro values.

define prolog_macro =
    newproperty([[\^[ ^prolog_handlepopescape]], 8, false, "perm");
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, May 15 1992
        Weakened the reference to -handlepopescape-
--- Robert John Duncan, Jan 24 1991
        Exported action for ESC.
--- Simon Nichols, Jul 17 1990
        Removed all the predefined macros (except -esc-): they have been
        re-implemented as commands.
--- Rob Duncan, Aug  8 1989
    - sectionised and added #_INCLUDEs for POPC;
    - moved in -prolog_macro_apply- and -prolog_expand_macros- from
        "itemise.p";
    - added -do_vedcmd- (based on the PML version) to allow for calling
        ved etc. from inside immediate mode;
    - changed the macro definitions to use -valof- to delay autoloading
        until the macro is first used;
    - removed reference to -prolog_word_table_size-.
--- Rob Duncan, Mar 16 1988
    Renamed from plogmac.p
 */
