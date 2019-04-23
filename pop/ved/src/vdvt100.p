/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/ved/src/vdvt100.p
 > Purpose:         VED: vt100 initialisation
 > Author:          Steve Hardy & John Gibson (see revisions)
 */


#_INCLUDE 'vddeclare.ph'

constant
        procedure (vedpushkey, vedpopkey, vedansiscreenxy, veduseterm,
        vedansisetscrollregion)
    ;

vars
        procedure (vedscreenxy, vedsetscrollregion, vedenterkey, vedredokey),
        vedescapetable, vedquerytable, vedterminalselect, vedterminalname
    ;


;;; vedvt100screeninteger:
;;;     outputs an integer as ascii decimal
;;;     (not used here any more, but kept for backward compatability)

define vedvt100screeninteger(integer);
    lvars integer;
    dlocal cucharout = vedscr_char_out, pop_pr_radix = 10;
    sys_syspr(integer);
enddefine;

    ;;; Called for graphics chars in the range 16:81 - 16:9D
    ;;; (currently only upto 16:92 are used)
define vedvt100screengraphtrans(char);
    lvars char, c;
    ;;;                123456789ABCDEF0123456789ABCD
    lconstant trans = 'qqqxmjvxlkwxtunf`~___________';
    fast_subscrs(char fi_- 16:80, trans) -> c;
    ;;; 2nd result true means graphics mode
    if c == `_` then char, false else c, true endif
enddefine;


;;; -- SCREEN CONTROL -----------------------------------------------------

define vedvt100screen();

    ;;; set terminal name
    "vt100" -> vedterminalname;
    false   -> vedterminalselect;

    ;;; set terminal attributes
    false   -> vednokeypad;

    ;;; basic screen control sequences
    '\^[[D' -> vvedscreencharleft;      ;;; Move cursor left
    '\^[[C' -> vvedscreencharright;     ;;; Move cursor right
    '\^[[A' -> vvedscreencharup;        ;;; Move cursor up
    '\^[[2J' -> vvedscreenclear;        ;;; Clear the screen
    '\^[[K' -> vvedscreencleartail;     ;;; Clear to right of cursor
    '\^[D'  -> vvedscreenscrollup;      ;;; Scroll region forwards
    '\^[M'  -> vvedscreenscrolldown;    ;;; Scroll region backwards
    '\^[(0' -> vvedscreengraphic;       ;;; Put terminal into graphic mode
    '\^[(B' -> vvedscreenalpha;         ;;; Put terminal into alphabetic mode
    '\^[[m'  -> vvedscreencharnormal;   ;;; Normal char mode
    '\^[[1m' -> vvedscreencharbold;     ;;; bold
    '\^[[4m' -> vvedscreencharunderline; ;;; underline
    '\^[[5m' -> vvedscreencharblink;    ;;; blink (shows as bold on xterm)
    '\^[[7m' -> vvedscreencharhighlight; ;;; highlight (inverse video)

    '\^[<\^[=\^[[?4l' -> vvedscreensetpad; ;;; ansi, alt key pad, fast scroll
    '\^[>'  -> vvedscreenresetpad;      ;;; key pad normal

    ;;; procedures for cursor motion and scrolling regions
    vedansiscreenxy -> vedscreenxy;
    vedansisetscrollregion -> vedsetscrollregion;

    ;;; translation for VED standard graphic symbols
    vedvt100screengraphtrans -> vedscreengraphtrans
enddefine;


;;; -- KEYBOARD -----------------------------------------------------------

lconstant

    gold_table = writeable [%
        `\^[`,  ;;; GOLD ESC
            writeable [%
                `O`,    ;;; GOLD ESC O
                    writeable [%
                        `A`,    vedtopfile,
                        `B`,    vedendfile,
                        `P`,    vedrefresh,
                        `Q`,    vedlineabove,
                        `R`,    vedlinebelow,
                        `S`,    vedsetstatic,
                        `w`,    vedchardelete,
                        `x`,    veddotdelete,
                        `y`,    procedure; vedcharright(), veddotdelete() endprocedure,
                        `t`,    vedclearhead,
                        `u`,    vedlinedelete,
                        `v`,    vedcleartail,
                        `q`,    vedpushkey,
                        `r`,    vedpopkey,
                        `s`,    vedexchangeposition,
                        `p`,    vedwordleftdelete,
                        `n`,    vedwordrightdelete,
                    %],
                `[`,    ;;; GOLD ESC [
                    writeable [%
                        `A`,    vedtopfile,
                        `B`,    vedendfile,
                    %],
            %],
        `\^?`,  ;;; GOLD DEL
            veddotdelete,
    %],


    esc_bra_table = writeable [%
        `A`,    vedscreenup,
        `B`,    vedscreendown,
        `C`,    vedtextright,
        `D`,    vedscreenleft,
    %],

    esc_esc_table = writeable [%
        `O`,    ;;; ESC ESC O
            writeable [%
                `q`,    vedchardownleftlots,
                `r`,    vedchardownlots,
                `s`,    vedchardownrightlots,
                `t`,    vedcharleftlots,
                `u`,    vedcharmiddle,
                `v`,    vedcharrightlots,
                `w`,    vedcharupleftlots,
                `x`,    vedcharuplots,
                `y`,    vedcharuprightlots,
            %]
    %],

    querytable = {%
        repeat 64 times undef endrepeat,
        vedscreenup,        ;;; A
        vedscreendown,      ;;; B
        vedtextright,       ;;; C
        vedscreenleft,      ;;; D
        repeat 8 times undef endrepeat,
        ident vedenterkey,  ;;; M
        undef, undef,       ;;; N, O
        gold_table,         ;;; P
        vedmarklo,          ;;; Q
        vedmarkhi,          ;;; R
        "ved_m",            ;;; S
        repeat 24 times undef endrepeat,
        vedstatusswitch,    ;;; l
        ident vedredokey,   ;;; m
        ident vedwordright, ;;; n
        undef,              ;;; o
        ident vedwordleft,  ;;; p
        vedchardownleft,    ;;; q
        vedchardown,        ;;; r
        vedchardownright,   ;;; s
        vedcharleft,        ;;; t
        vedcharmiddle,      ;;; u
        vedcharright,       ;;; v
        vedcharupleft,      ;;; w
        vedcharup,          ;;; x
        vedcharupright,     ;;; y
        repeat 6 times undef endrepeat
    %},
;

define vedvt100keys();
    `O` -> vedquery;
    querytable -> vedquerytable;
    copy(vedescapetable) -> vedescapetable;
    ident vedquerytable -> vedescapetable(`?`);
    ident vedquerytable -> vedescapetable(`O`);
    esc_bra_table -> vedescapetable(`[`);
    esc_esc_table -> vedescapetable(`\^[`);
enddefine;


;;; vedvt100:
;;;     set VED up for a vt100 terminal.
;;;     -id- is a dummy argument, kept for backward compatability.

define vedvt100(id);
    lvars id;
    veduseterm('vt100') -> ;
enddefine;


/* --- Revision History ---------------------------------------------------
--- John Williams, Oct 26 1995
        Removed mapping from ESC P to vedrefresh (BR isl-fr.4548).
--- John Gibson, Jan  5 1993
        Changed tables to use idents instead of words for some basic
        operations.
--- John Gibson, Feb  6 1992
        Added assignment to -vedscreengraphtrans- and removed assignments
        to individual graph char variables (which now have standard values).
--- John Gibson, Jan 23 1992
        New graphics chars
--- John Gibson, Dec 20 1991
        Added vvedscreenchar- mode sequences
--- Rob Duncan, Jan 17 1990
        Added screen scrolling sequences
--- Rob Duncan, Nov  8 1989
        Made -gold_table- etc. "writeable" so that they can be extended
        by -vedsetkey-
--- Rob Duncan, Nov  7 1989
        Separated screen and keyboard setup into -vedvt100screen- and
        -vedvt100keys- (as expected by -veduseterm- in "vdinitseq.p").
        and changed -vedvt100- just to call -veduseterm-.
        Factored out the special code for handling scrolling regions (see
        "vdscreen.p"): the same effect is now obtained by assigning
        -vedansisetscrollregion- to -vedsetscrollregion-.
--- John Gibson, Aug 16 1987
        Lconstant'ed, etc.
 */
