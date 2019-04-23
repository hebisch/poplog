/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/ved/src/vdsetup.p
 > Purpose:         Initialise VED permanent identifiers
 > Author:          John Gibson (see revisions)
 */

#_INCLUDE 'vddeclare.ph'
#_INCLUDE '../../src/dirs.ph'

constant
    vedspacestring          = '\s',
;

vars

    vedscreenlength         = 24,
    vedscreenwidth          = 80,

    veddumbvdu              = true,
    vednochardelete         = true,
    vednocharinsert         = true,
    vednolinedelete         = true,
    vednolineinsert         = true,
    vednokeypad             = true,
    vednomoveinsert         = false,
    vedscreenwrap           = false,

    vvedscreencharleft      = `\b`,
    vvedscreencharright     = nullstring,
    vvedscreencharup        = nullstring,
    vvedscreenchardown      = `\n`,
    vvedscreenscreenleft    = `\r`,
    vvedscreenscrollup      = `\n`,
    vvedscreenscrolldown    = nullstring,
    vvedscreenclear         = nullstring,
    vvedscreencleartail     = nullstring,
    vvedscreeninsertchar    = nullstring,
    vvedscreendeletechar    = nullstring,
    vvedscreeninsertline    = nullstring,
    vvedscreendeleteline    = nullstring,
    vvedscreeninsertmode    = nullstring,
    vvedscreenovermode      = nullstring,
    vvedscreenalpha         = nullstring,
    vvedscreengraphic       = nullstring,
    vvedscreencharnormal    = nullstring,
    vvedscreencharhighlight = nullstring,
    vvedscreencharbold      = nullstring,
    vvedscreencharunderline = nullstring,
    vvedscreencharaltfont   = nullstring,
    vvedscreencharblink     = nullstring,
    vvedscreenpoint         = nullstring,
    vvedscreenbell          = `\^G`,
    vvedscreensetpad        = nullstring,
    vvedscreenresetpad      = nullstring,
    vvedscreeninit          = nullstring,
    vvedscreenreset         = nullstring,
    vvedscreensendidseq     = '\eZ',

    vedscreencontrolmark    = `\Go`,        ;;; control character in text
    vedscreenmoremark       = `\G|`,        ;;; more text on line
    vedscreenrangemark      = `\[b2]\G|`,   ;;; marked ranges
    vedscreencursorlinemark = `\[b2]\G-`,   ;;; current line when cursor is on
                                            ;;; command line
    vedscreencursorlinerangemark
                            = `\[b2]\G+`,   ;;; current line in marked range

    vedscreenstatus_|-_mark = `\Glt`,   ;;; after line number in status line
    vedscreenstatus_-|_mark = `\Grt`,   ;;; before line number in status line
    vedscreenstatus_-_mark  = `\G-`,    ;;; horizontal line in status line
    vedscreenstatusinputmark= `\[f]I`,  ;;; when vedprocswaiting() is true
    vedscreenstatusnumattr  = `\[b]`,   ;;; attributes for line number chars

    ;;; cursor chars
    vedscreennormalcursor   = `O`,      ;;; normal input mode
    vedscreenstaticcursor   = `_`,      ;;; normal input with vedstatic true
    vedscreeninasciicursor  = `\[f]I`,  ;;; set by vedinascii

    vedscrollscreen         = true,     ;;; if false, then re-write screen

    vedwriteallfiles        = false,
    veddelspaces            = true,     ;;; delete leading spaces when merging
                                        ;;; lines
    vedcurrentfile          = false,    ;;; data structure for current file
    vedbufferlist           = [],       ;;; list of buffers

    vedescape               = `\^[`,
    vedquery                = `?`,

    vedvedname              = 'temp.p',
    vedlibdir               = POPVEDLIB,

    vedwlinewidth           = 0,
    vedwlineoffset          = 1,
    vedwcolumnoffset        = 1,

    ;;; window whose file has been quit but which hasn't yet been killed.
    wvedfreewindow          = false,

    vedlmr_errs_in_file     =  false,
    vedlmr_print_in_file    =  'output.p',

    ;;; Set true by XVed for variable-width-character mode
    vedvarwidthmode         = false,
;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 15 1997
        Added vedvarwidthmode
--- John Gibson, Mar 12 1996
        vvedpromptchar now a per-file local -- moved to vdfiles.p
--- John Gibson, May 26 1995
        Changed initialisation of vedscreenstatusnumattr to `\[b]`
--- John Gibson, Jan 13 1994
        Added vedscreennormalcursor etc
--- John Gibson, Jan 10 1994
        Added vedscreenstatusnumattr
--- John Gibson, Apr 20 1993
        Changed initialisation of vvedpromptchar to false
--- John Gibson, Dec 20 1992
        Moved all initialisations of per-file variables to vdfiles.p
--- John Gibson, Dec 11 1992
        Replaced G_ macros with char code syntax
--- John Gibson, Feb  6 1992
        Changed vedscreenmoremark etc to use standard graphics chars
--- John Gibson, Jan 23 1992
        New graphic chars.
--- John Gibson, Jan 17 1992
        Added initialisation for -vvedscreencharaltfont-
--- John Gibson, Dec 20 1991
        Added default assignments for new vvedscreenchar- vars
--- John Gibson, Aug 26 1991
        Exported the 3 vedw- variables
--- Rob Duncan, Feb  2 1990
        Moved declaration of -vedstartwindow- to "vdwindows.p"
--- John Williams, Jan 19 1990
        -vedstartwindow- initialised to 12 again!
--- John Williams, Jan 19 1990
        -vedstartwindow- now initialised to -false-
--- Aaron Sloman, Dec 27 1989
        Moved definition of -vednonbreakfiles- to vdfiles.p
--- Rob Duncan, Dec  1 1989
        Added '.ml' and '.sig' to -vednonbreakfiles-
--- Rob Duncan, Nov  7 1989
        Added new screen-control variables:
            vednochardelete
            vednolinedelete
            vednomoveinsert
            vedscreenwrap
            vvedscreencharleft
            vvedscreenchardown
            vvedscreenscreenleft
            vvedscreenscrollup
            vvedscreenscrolldown
            vvedscreeninsertchar
            vvedscreenbell
            vvedscreeninit
            vvedscreenreset
        Moved -vedterminalname-, -vedterminalselect- and -vedvt52select-
        to "vdinitseq.p";
        changed initialisations so as not to assume "vi200" by default;
        reordered declarations.
--- John Williams, Mar  1 1988
        Uses -lispfiletypes- instead of '.lsp' in -vednonbreakfiles-
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
 */
