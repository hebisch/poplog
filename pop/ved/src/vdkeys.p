/* --- Copyright University of Sussex 2003. All rights reserved. ----------
 > File:            C.all/ved/src/vdkeys.p
 > Purpose:         VED: default key bindings
 > Author:          Rob Duncan, Aug 23 1989 (see revisions)
 */

#_INCLUDE 'vddeclare.ph'


constant
        procedure (vedpopkey, vedpushkey,
        vedstatusswitch, vedswapfiles, vedmarkfind,
        ved_re_backsearch, ved_end_im, ved_switchmode_im,
        vedfileselect, Sys$-Ved$-Im$-Input_keyboard_intr,
        vedsetwindow
        )
    ;

vars
        procedure (vedredokey, ved_q),
        vednormaltable, vedescapetable, vedquerytable, vedhardtabs,
    ;

;;; -----------------------------------------------------------------------

section $-Sys$-Ved => vednewkeys, vedoldkeys, veddefaultkeys;
constant
        procedure ( vedsetwindow );

    /*  For processing \^C as a character
    */
define lconstant keyboard_intr();
    if VDDEV_LOADED and VDDEV_WEAK vedprocswaiting() then
        VDDEV_WEAK Im$-Input_keyboard_intr()
    else
        keyboard_interrupt()
    endif
enddefine;

define lconstant weak_apply(p);
    lvars procedure p;
    unless isundef(p) then chain(p) endunless
enddefine;

define lconstant End_im =
    weak_apply(%VDDEV_WEAK ved_end_im%)
enddefine;
define lconstant Switchmode_im =
    weak_apply(%VDDEV_WEAK ved_switchmode_im%)
enddefine;

define lconstant tab_mode();
    not(vedhardtabs) -> vedhardtabs;
    vedputmessage(if vedhardtabs then '\{b}hard tabs' else '\{b}soft tabs' endif);
enddefine;


;;; -- NEW KEY BINDINGS ---------------------------------------------------

lconstant

    unkilltable = writeable [%  ;;; ESC ^K <key>
        `\^K`,  "ved_yankl",
        `\^A`,  "ved_yankw",
        `\^U`,  "ved_yankw",
        `\^E`,  "ved_yankw",
        `\^B`,  "ved_yankw",
        `\^W`,  "ved_yankw",
        `\^F`,  "ved_yankw",
        `\^D`,  "ved_yank",
        `\^P`,  "ved_splice",
    %],

    killtable = writeable [%    ;;; ^K <key>
        `\^K`,  vedlinedelete,
        `\^A`,  vedclearhead,
        `\^U`,  vedclearhead,
        `\^E`,  vedcleartail,
        `\^B`,  vedwordleftdelete,
        `\^W`,  vedwordleftdelete,
        `\^F`,  vedwordrightdelete,
        `\^D`,  "ved_d",
        `\^P`,  "ved_cut",
    %],

    newescapetable = {%     ;;; ESC <key>
        ;;; control characters
        vedtextleft,            ;;; ^A
        vedcharleftlots,        ;;; ^B
        keyboard_intr,          ;;; ^C
        Switchmode_im,          ;;; ^D
        vedscreenright,         ;;; ^E
        vedcharrightlots,       ;;; ^F
        vedstatusswitch,        ;;; ^G
        "ved_xup",              ;;; BS
        vedtabright,            ;;; TAB
        "ved_xdn",              ;;; LF
        unkilltable,            ;;; ^K
        undef,                  ;;; ^L
        ident vedredokey,       ;;; CR
        vedchardownlots,        ;;; ^N
        undef,                  ;;; ^O
        vedcharuplots,          ;;; ^P
        undef,                  ;;; ^Q
        veddotdelete,           ;;; ^R
        undef,                  ;;; ^S
        undef,                  ;;; ^T
        "ved_yankw",            ;;; ^U
        "ved_xdn",              ;;; ^V
        "ved_yankw",            ;;; ^W
        undef,                  ;;; ^X
        undef,                  ;;; ^Y
        End_im,                 ;;; ^Z
        repeat 5 times undef endrepeat,
                                ;;; ESC ^\ ^] ^^ ^_

        ;;; printing characters
        undef, undef, undef,    ;;; SP ! "
        vedsetstatic,           ;;; #
        repeat 10 times undef endrepeat,
                                ;;; $ % & ' ( ) * + , -
        vedchangecase,          ;;; .
        "ved_re_search",        ;;; /
        undef,undef,undef,      ;;; 0,1,2
        "vedfilecomplete",      ;;; 3
        repeat 8 times undef endrepeat,
                                ;;; 4-9 : ;
        vedtopfile,             ;;; <
        undef,                  ;;; =
        vedendfile,             ;;; >
        ident vedquerytable,    ;;; ?
        undef, undef, undef,    ;;; @ A B
        "ved_mcp",              ;;; C
        undef, undef,           ;;; D, E
        "vedfilecomplete",      ;;; F
        "vedendrange",          ;;; G
        undef, undef,           ;;; H I
        "ved_jjp",              ;;; J
        "vedhelpkey",           ;;; K
        undef,                  ;;; L
        vedmarkhi,              ;;; M
        "vedprevioushelp",      ;;; N
        vedlinebelow,           ;;; O
        vedpopkey,              ;;; P
        repeat 5 times undef endrepeat,
                                ;;; Q-U
        "ved_xup",              ;;; V
        repeat 4 times undef endrepeat,
                                ;;; W-Z
        undef,                  ;;; [
        ved_re_backsearch,      ;;; \
        repeat 4 times undef endrepeat,
                                ;;; ] ^ _ `
        undef,                  ;;; a
        ident vedwordleft,      ;;; b
        "ved_lcp",              ;;; c
        "vedloadline",          ;;; d
        vedfileselect,          ;;; e
        ident vedwordright,     ;;; f
        vedmarkfind,            ;;; g
        "vedgetsysfile",        ;;; h
        undef,                  ;;; i
        "ved_jp",               ;;; j
        "ved_hk",               ;;; k
        "ved_lcw",              ;;; l
        vedmarklo,              ;;; m
        "vednexthelp",          ;;; n
        vedlineabove,           ;;; o
        vedpushkey,             ;;; p
        ident ved_q,            ;;; q
        undef,                  ;;; r
        vedexchangeposition,    ;;; s
        undef,                  ;;; t
        "ved_ucw",              ;;; u
        vedscreenup,            ;;; v
        vedsetwindow,           ;;; w
        vedswapfiles,           ;;; x
        undef,                  ;;; y
        tab_mode,               ;;; z
        "vedprevpara",          ;;; {
        undef,                  ;;; |
        "vednextpara",          ;;; }
        undef,                  ;;; ~

        ;;; delete
        undef,                  ;;; DEL
    %},

    newnormaltable = {%     ;;; <key> alone

        ;;; control characters
        vedscreenleft,          ;;; ^A
        vedcharleft,            ;;; ^B
        keyboard_intr,          ;;; ^C
        "ved_lmr",              ;;; ^D
        vedtextright,           ;;; ^E
        vedcharright,           ;;; ^F
        vedenter,               ;;; ^G
        vedcharleft,            ;;; BS
        vedinsertvedchar,       ;;; TAB
        vednextline,            ;;; LF
        killtable,              ;;; ^K
        vedrefresh,             ;;; ^L
        ident veddocr,          ;;; CR
        vedchardown,            ;;; ^N
        undef,                  ;;; ^O
        vedcharup,              ;;; ^P
        undef,                  ;;; ^Q
        veddotdelete,           ;;; ^R
        undef,                  ;;; ^S
        "ved_sw",               ;;; ^T
        vedclearhead,           ;;; ^U
        vedscreendown,          ;;; ^V
        vedwordleftdelete,      ;;; ^W
        undef,                  ;;; ^X
        undef,                  ;;; ^Y
        undef,                  ;;; ^Z
        ident vedescapetable,   ;;; ESC
        repeat 4 times undef endrepeat,
                                ;;; ^\ ^] ^^ ^_

        ;;; printing characters
        repeat 95 times vedinsertvedchar endrepeat,

        ;;; delete
        vedchardelete           ;;; DEL
    %},
;

define vednewkeys();
    newnormaltable -> vednormaltable;
    newescapetable -> vedescapetable;
    undef -> vedquerytable;
enddefine;


;;; -- OLD KEY BINDINGS ---------------------------------------------------

lconstant                                ;;; "++" = new Oct 1990

    oldescapetable = {%
        vedscreenup,            ;;; ^A
        undef,                  ;;; ^B
        undef,                  ;;; ^C
        Switchmode_im,          ;;; ^D
        undef,                  ;;; ^E ++
        vedcharright,           ;;; ^F ++
        vedenter,               ;;; ^G ++
        "ved_xup",              ;;; ^H  (BS)
        vedtabright,            ;;; ^I  (TAB )
        "ved_xdn",              ;;; ^J
        killtable,              ;;; ^K ++
        vedrefresh,             ;;; ^L ++
        vedenter,               ;;; ^M ++
        undef,                  ;;; ^N ++
        undef,                  ;;; ^O ++
        "ved_mcp",              ;;; ^P ++
        undef,                  ;;; ^Q ++
        undef,                  ;;; ^R ++
        undef,                  ;;; ^S ++
        "ved_sw",               ;;; ^T ++
        "ved_yankw",            ;;; ^U ++
        undef,                  ;;; ^V ++
        "ved_yankw",            ;;; ^W ++
        undef,                  ;;; ^X ++
        undef,                  ;;; ^Y ++
        End_im,                 ;;; ^Z
        repeat 5 times undef endrepeat,
                                ;;; ESC ^\ ^] ^^ ^_
        vedcleartail,           ;;; ` `   space    32
        vedwordleftdelete,      ;;; !
        vedwordrightdelete,     ;;; "
        vedsetstatic,           ;;; #
        vedmarklo,              ;;; $
        vedmarkhi,              ;;; %
        "ved_m",                ;;; &
        vedpushkey,             ;;; '
        vedpopkey,              ;;; (
        vedlineabove,           ;;; )
        vedlinebelow,           ;;; *
        vedpushkey,             ;;; +
        "ved_cut",              ;;; , ++
        vedpopkey,              ;;; -
        "ved_splice",           ;;; . ++
        "ved_re_search",        ;;; /
        undef,                  ;;; 0
        vedcharuprightlots,     ;;; 1 = 49
        vedcharrightlots,       ;;; 2 = 50
        "vedfilecomplete",      ;;; 3 ++
        undef,                  ;;; 4 ++
        undef,                  ;;; 5 ++
        undef,                  ;;; 6 ++
        "ved_yankw",            ;;; 7 ++
        "ved_yankl",            ;;; 8 ++
        "ved_y",                ;;; 9 ++
        "ved_jcp",              ;;; : ++
        "ved_jjp",              ;;; ; ++
        "ved_lcw",              ;;; < ++
        "ved_jp",               ;;; = ++
        "ved_ucw",              ;;; > ++
        ident vedquerytable,    ;;; ?
        "ved_hk",               ;;; @
        vedscreenup,            ;;; A = 65
        vedscreendown,          ;;; B
        vedtextright,           ;;; C
        vedscreenleft,          ;;; D
        undef,undef,undef,      ;;; E - G
        vedendfile,             ;;; H
        undef,                  ;;; I
        vedchardownrightlots,   ;;; J
        vedchardownleftlots,    ;;; K
        vedcharupleftlots,      ;;; L
        vedcharleftlots,        ;;; M
        "vedprevioushelp",      ;;; N ++
        vedscreenmiddle,        ;;; O
        veddotdelete,           ;;; P
        vedclearhead,           ;;; Q
        vedlinedelete,          ;;; R
        repeat 8 times undef endrepeat,
                                ;;; S - Z
        vedtextleft,            ;;; [
        ved_re_backsearch,      ;;; \
        vedscreenright,         ;;; ]
        vedtextleft,            ;;; ^
        undef,                  ;;; _
        undef,                  ;;; `
        vedcharleftlots,        ;;; a
        vedendfile,             ;;; b
        "ved_lcp",              ;;; c
        "vedloadline",          ;;; d
        vedfileselect,          ;;; e
        vedchardownlots,        ;;; f
        ident vedredokey,       ;;; g ++
        "vedgetsysfile",        ;;; h
        vedcharuplots,          ;;; i
        "ved_jcp",              ;;; j ++
        "ved_t",                ;;; k ++
        vedwordleftdelete,      ;;; l
        "ved_m",                ;;; m ++
        "vednexthelp",          ;;; n
        undef,                  ;;; o
        vedexchangeposition,    ;;; p
        ident ved_q,            ;;; q
        vedwordrightdelete,     ;;; r
        vedcharrightlots,       ;;; s
        vedtopfile,             ;;; t
        "vedprevioushelp",      ;;; u
        vedrefresh,             ;;; v
        vedsetwindow,           ;;; w
        vedswapfiles,           ;;; x
        "ved_y",                ;;; y
        tab_mode,               ;;; z
        vedmarkfind,            ;;; { ++
        "ved_d",                ;;; | ++
        "vedendrange",          ;;; } ++
        undef,                  ;;; ~
        undef,                  ;;; <DEL>
    %},

    oldnormaltable = {%
         vedscreenleft,         ;;; ^A
         ident vedwordleft,     ;;; ^B
         keyboard_intr,         ;;; ^C
         "ved_lmr",             ;;; ^D load marked range
         vedcleartail,          ;;; ^E
         ident vedwordright,    ;;; ^F
         vedstatusswitch,       ;;; ^G
         vedcharleft,           ;;; ^H  Backspace
         vedinsertvedchar,      ;;; ^I  Tab
         vednextline,           ;;; ^J  Line feed
         vedscreenup,           ;;; ^K  vertical form feed
         vedscreendown,         ;;; ^L  form feed
         ident veddocr,         ;;; ^M  Carriage return
         vedchangecase,         ;;; ^N
         ident vedscreenbell,   ;;; ^O
         veddotdelete,          ;;; ^P
         ident vedscreenbell,   ;;; ^Q
         vedwordrightdelete,    ;;; ^R
         ident vedscreenbell,   ;;; ^S
         vedcharup,             ;;; ^T
         vedclearhead,          ;;; ^U
         vedchardown,           ;;; ^V
         vedwordleftdelete,     ;;; ^W
         ident vedscreenbell,   ;;; ^X
         ident vedscreenbell,   ;;; ^Y
         vedtextright,          ;;; ^Z
         ident vedescapetable,  ;;; ESC
         ident vedscreenbell,   ;;; ^\  could be used for something
         vedlinedelete,         ;;; ^]
         ident vedscreenbell,   ;;; ^^  WILL BE USED for inserting control characters
         ident vedredokey,      ;;; ^_  DO the command on the command line
            ;;; now the ordinary printing characters
         repeat 95 times vedinsertvedchar endrepeat,
         vedchardelete          ;;; DELETE
    %},
;

define vedoldkeys();
    oldnormaltable -> vednormaltable;
    oldescapetable -> vedescapetable;
    undef -> vedquerytable;
enddefine;


;;; -- DEFAULT KEY BINDINGS -----------------------------------------------

;;; Use the new key set by default
vars procedure (
    veddefaultkeys = vednewkeys,
);

;;; Do it now ...
veddefaultkeys();

endsection;     /* $-Sys$-Ved */



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 23 Jul 2003
        Declared vedsetwindow
--- John Gibson, Feb 16 1996
        \^C now runs keyboard_intr
--- Robert John Duncan, Jan 19 1994
        Added paragraph movements to vednewkeys (<ESC> { and <ESC> })
--- John Williams, Oct 14 1993
        Added mapping from <ESC> F to "vedfilecomplete".
--- Robert John Duncan, Aug 31 1993
        Made vednewkeys the default
--- John Gibson, Jan  5 1993
        Changed tables to use idents instead of words for some basic
        operations.
--- John Gibson, Dec 21 1992
        Weakref'ed IM things
--- John Gibson, Jun  4 1991
        Sectionised, replaced veddointerrupt with -do_int*errupt-
--- Aaron Sloman, Oct 31 1990
        Added vedfilecomplete and other new things to vedoldkeys.
        New things marked "++"
--- John Williams, Oct 24 1990
        -vednewkeys- maps \^[K to "vedhelpkey"
--- Robert John Duncan, Oct 11 1990
        Changed "default" to "new" and restored the old key set as the
        default.
--- Robert John Duncan, Jul 16 1990
        Reinstated <ESC> ^H = "ved_xup" and <ESC> ^J = "ved_xdn" for
        backward compatibility
--- Aaron Sloman, Jan 13 1990
        Added vedendrange for <ESC> G, to match documentation.
--- Aaron Sloman, Jan  3 1990
        Made <ESC> d = vedloadline for oldescapetable
--- Aaron Sloman, Jan  3 1990
        Replaced Pass_eof with ved_end_im, and Cr_input with
        ved_switchmode_im, both now exported.
--- John Gibson, Oct 13 1989
        Procedures -Pass_eof-, -Cr_input- into section Sys$-Ved$-Im
 */
