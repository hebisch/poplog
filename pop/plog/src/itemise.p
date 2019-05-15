/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:        C.all/plog/src/itemise.p
 > Purpose:     Prolog: itemisation
 > Author:      Simon Nichols & Robert Duncan, Feb. 1987 (see revisions)
 */


section prolog;

constant
    procedure ( prolog_macro_apply, ),
;

vars
    prolog_expand_macros,
;

;;; =======================================================================

/* Reading commands */

define Read_cmdname =
    readitem(%%);
enddefine;

define updaterof Read_cmdname(/*item*/);
    /* item */ :: proglist -> proglist;
enddefine;

define Read_cmdarg();
    dlocal popnewline = true;
    rdstringto([^termin ^newline]);
enddefine;


/* Item Buffer */

;;; item_buf:
;;;     a circular buffer of items read from the input stream for reporting
;;;     the locations of syntax errors.

lconstant
    ITEM_BUF_SIZE = 256,
    item_buf = writeable initv(ITEM_BUF_SIZE),
;

lvars
    item_buf_ptr = 0,
    item_buf_wrapped = false,
;

;;; insert_in_buf:
;;;     inserts an item into the next free position in -item_buf-.
;;;     Sets -item_buf_wrapped- to <true> once the buffer has been filled.

define lconstant insert_in_buf(/* item */) with_nargs 1;
    if item_buf_ptr == ITEM_BUF_SIZE then
        true -> item_buf_wrapped;
        1 -> item_buf_ptr;
    else
        item_buf_ptr fi_+ 1 -> item_buf_ptr;
    endif;
    /* item */ -> Subscrv(item_buf_ptr, item_buf);
enddefine;

;;; replace_in_buf:
;;;     replaces the current item in the item buffer.

define replace_in_buf(/* item */) with_nargs 1;
    /* item */ -> Subscrv(item_buf_ptr, item_buf);
enddefine;

;;; prolog_initbuf:
;;;     initialises the item buffer

define prolog_initbuf(item);
    lvars item;
    0 -> item_buf_ptr;
    false -> item_buf_wrapped;
    if item then insert_in_buf(item) endif;
enddefine;

;;; items_read:
;;;     returns the contents of -item_buf- in a list, together with the
;;;     flag showing whether the buffer wrapped round.

define items_read();
    lvars i;
    item_buf_wrapped;   ;;; return flag
    returnif(item_buf_ptr == 0)([]);
    if item_buf_wrapped then item_buf_ptr else 0 endif -> i;
    [%
        repeat
            if i == ITEM_BUF_SIZE then 1 else i fi_+ 1 endif -> i;
            Subscrv(i, item_buf);
            quitif(i == item_buf_ptr);
        endrepeat;
    %];
enddefine;

;;; prologstring:
;;;     used to represent occurrences of Prolog strings in the item buffer
;;;     (ordinary strings are used to stand for quoted atoms)

defclass lconstant prologstring {
    prologstring :full
};

procedure(s);
    lvars   s;
    dlocal  pop_pr_quotes = false, pr = syspr;
    printf(prologstring(s), '"%p"');
endprocedure -> class_print(prologstring_key);


/* Itemisation */

vars
    prolog_lastitem,
        ;;; the last item read
;

;;; prolog_getitem:
;;;     returns the next item from proglist, expanding prolog macros.

define prolog_getitem() -> item;
    lvars item;
    repeat
        readitem() -> item;
        returnunless(
            isword(item)
            and prolog_expand_macros
            and prolog_macro(item));
        prolog_macro_apply(prolog_macro(item));
    endrepeat;
enddefine;

;;; prolog_itemread:
;;;     as -prolog_getitem-, but adds the item read into the item buffer.

define prolog_itemread() -> item;
    lvars item = prolog_getitem();
    item -> prolog_lastitem;
    insert_in_buf(item);
enddefine;

;;; prolog_nextitem:
;;;     as -prolog_getitem-, but doesn't remove the item from proglist.

define prolog_nextitem() -> item;
    lvars item = prolog_getitem();
    conspair(item, proglist) -> proglist;
enddefine;

;;; prolog_try_nextitem:
;;;     examines the next item on proglist: if its equal to -item_needed-
;;;     it's removed and added into the item buffer.

define prolog_try_nextitem(/* item_needed */) with_nargs 1;
    lvars item = prolog_getitem();
    if /* item_needed */ == item then
        item -> prolog_lastitem;
        insert_in_buf(item);
        true;
    else
        conspair(item, proglist) -> proglist;
        false;
    endif;
enddefine;

;;; prolog_readstring:
;;;     reads a Prolog string, using -readitem- with the string quote
;;;     character changed.

define prolog_readstring();
    lvars itemrep, item;
    dlocal
        %item_chartype(`'`, readitem)% = 5, ;;; separator
        %item_chartype(`"`, readitem)% = 7; ;;; string quote
    if isincharitem(readitem) ->> itemrep then
        `"` -> nextchar(itemrep);
        readitem() -> item;
    elseif (readitem() ->> item) == """ then
        nullstring -> item;
    else
        unless isstring(item) then item sys_>< nullstring -> item endunless;
        unless readitem() == """ then
            mishap(item, 1, 'UTS: UNTERMINATED STRING');
        endunless;
    endif;
    replace_in_buf(consprologstring(item));
    datalist(item);
enddefine;


/* Switching to Prolog Itemisation */

;;; prolog_standard_repeater:
;;;     model itemiser for Prolog: used for its tables only

define prolog_standard_repeater = writeable
    incharitem(mishap(% 0, 'THIS PROCEDURE SHOULD NEVER BE CALLED' %));
enddefine;
    ;;; initialise the itemiser tables
    writeable frozval(2, prolog_standard_repeater) -> ;
    writeable frozval(3, prolog_standard_repeater) -> ;
    1 -> item_chartype(`_`,  prolog_standard_repeater);     ;;; alpha
    3 -> item_chartype(`@`,  prolog_standard_repeater);     ;;; symbol
    3 -> item_chartype(`.`,  prolog_standard_repeater);     ;;; symbol
    3 -> item_chartype(`\\`, prolog_standard_repeater);     ;;; symbol
    5 -> item_chartype(```,  prolog_standard_repeater);     ;;; special
    5 -> item_chartype(`!`,  prolog_standard_repeater);     ;;; special
    5 -> item_chartype(`|`,  prolog_standard_repeater);     ;;; special
    9 -> item_chartype(`%`,  prolog_standard_repeater);     ;;; comment

;;; prolog_new_proglist:
;;;     make a new proglist, with Prolog itemisation.

define prolog_new_proglist(charrep) -> Proglist;
    lvars charrep, itemrep, Proglist = [];
    unless isclosure(charrep) and datalength(charrep) /== 0
    and isdevice(fast_frozval(1, charrep)) and isclosed(charrep)
    then
        incharitem(charrep) -> itemrep;
        frozval(2, prolog_standard_repeater) -> frozval(2, itemrep);
        frozval(3, prolog_standard_repeater) -> frozval(3, itemrep);
        pdtolist(itemrep) -> Proglist;
    endunless;
enddefine;

;;; prolog_switch_itemiser:
;;;     converts the current -proglist- to/from Prolog itemisation.
;;;     In its base form, it extracts the itemiser on which -proglist- is
;;;     based (assuming there is one) and updates the character tables to use
;;;     Prolog character classification.
;;;     As an updater, it restores the original tables.

define prolog_switch_itemiser();
    lvars itemrep;
    if (isincharitem(readitem) ->> itemrep) then
        ;;; Return the itemiser and its tables as results.
        frozval(2, itemrep), frozval(3, itemrep), itemrep;
        frozval(2, prolog_standard_repeater) -> frozval(2, itemrep);
        frozval(3, prolog_standard_repeater) -> frozval(3, itemrep);
    else
        false, false, false;
    endif;
enddefine;

define updaterof prolog_switch_itemiser(table7, table8, itemrep);
    lvars table7, table8, itemrep;
    if itemrep then
        table7 -> frozval(2, itemrep);
        table8 -> frozval(3, itemrep);
    endif;
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        Put the creation of prolog_standard_repeater back in-line,
        and deleted the initialisation procedure. Removed nasty handling of
        "#" "_INCLUDE" (which is not the right way to do includes at all).
        Deleted p*itemread (never exported or used).
--- John Gibson, Dec  7 1990
        Replaced r*ecordclass with defclass
--- John Williams, Jul 18 1990
        Made -prolog_standard_repeater- writeable, as well as its frozvals.
--- Simon Nichols, Jul 17 1990
        Added procedures for reading command name and arguments.
--- Rob Duncan, Aug  8 1989
    - sectionised and added #_INCLUDEs for POPC;
    - added -items_read- as the only direct interface between the parser
        and the contents of the item buffer: the buffer itself is now
        local;
    - moved out macro expansion to "macros.p";
    - made -prolog_getitem- the primary itemisation procedure and
        changed -prolog_nextitem- etc. accordingly;
    - added provision for '#_INCLUDE' into -prolog_getitem-;
    - made -prolog_standard_repeater- a variable with an explicit
        initialisation procedure. The initialisation is still done if
        the file is being loaded from "prolog.p".
--- John Gibson, Jun 18 1989
        Added -writeable- for -item_buf- and -prolog_standard_repeater-
        tables.
--- Rob Duncan, Mar 16 1988
        Renamed from plogitem.p
 */
