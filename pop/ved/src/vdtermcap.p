/* --- Copyright University of Sussex 2004. All rights reserved. ----------
 > File:            C.unix/ved/src/vdtermcap.p
 > Purpose:         VED: termcap interface
 > Author:          Rob Duncan, Apr 12 1989 (see revisions)
 > Related Files:   C.unix/src/termcap.p
 */


#_INCLUDE 'vddeclare.ph'


#_IF DEF NCR
    /*  Termcap not available -- see comment in "termcap.p"
    */
define vedtermcapscreen(_);
enddefine;
#_TERMIN_IF true
#_ENDIF


vars
    vedterminalselect,
;

section $-Sys$-Ved =>
    vedtermcapscreen,
;

;;; Termcap capabilities used by VED (see termcap(5) or terminfo(4)):

lconstant
    AE  = 'ae',     ;;; str (P)     end alternate (graphic) character set
    AL  = 'al',     ;;; str (P*)    add new blank line
    AM  = 'am',     ;;; bool        terminal has auto-margins (screen wrap)
    AS  = 'as',     ;;; str (P)     start alternate (graphic) character set
    BC  = 'bc',     ;;; str         backspace if not ^H
    BL  = 'bl',     ;;; str (P)     terminal bell
    BS  = 'bs',     ;;; bool        terminal can backspace with ^H
    CE  = 'ce',     ;;; str (P)     clear to end of line
    CL  = 'cl',     ;;; str (P*)    clear screen
    CM  = 'cm',     ;;; str (NP)    cursor motion
    CO  = 'co',     ;;; num         number of columns
    CS  = 'cs',     ;;; str (NP)    change scrolling region (like vt100)
    DC  = 'dc',     ;;; str (P*)    delete character
    DM  = 'dm',     ;;; str         enter delete mode
    DL  = 'dl',     ;;; str (P*)    delete line
    DO  = 'do',     ;;; str         down one line
    ED  = 'ed',     ;;; str         end delete mode
    EI  = 'ei',     ;;; str         end insert mode
    IC  = 'ic',     ;;; str (P*)    insert character
    IM  = 'im',     ;;; str         enter insert mode
    KE  = 'ke',     ;;; str         end "keypad transmit" mode
    KS  = 'ks',     ;;; str         start "keypad transmit" mode
    LE  = 'le',     ;;; str (P)     cursor left (on newer systems)
    LI  = 'li',     ;;; num         number of lines
    MB  = 'mb',     ;;; str (P)     enter blink mode
    MD  = 'md',     ;;; str (P)     enter bold mode
    ME  = 'me',     ;;; str (P)     exit all modes
    MI  = 'mi',     ;;; bool        safe to move in insert mode
    MR  = 'mr',     ;;; str (P)     enter reverse video (highlight) mode
    ND  = 'nd',     ;;; str         non-destructive space (cursor right)
    NS  = 'ns',     ;;; bool        terminal won't scroll with ^J
    PC  = 'pc',     ;;; str         pad character if not null
    SE  = 'se',     ;;; str (P)     exit standout mode
    SF  = 'sf',     ;;; str (P)     scroll forwards (default ^J)
    SO  = 'so',     ;;; str (P)     enter standout mode
    SR  = 'sr',     ;;; str (P)     scroll backwards
    TE  = 'te',     ;;; str         terminal reset string
    TI  = 'ti',     ;;; str         terminal initialisation string
    UE  = 'ue',     ;;; str (P)     exit underscore mode
    UP  = 'up',     ;;; str         cursor up
    US  = 'us',     ;;; str (P)     enter underscore mode
    VB  = 'vb',     ;;; str         visible bell
;

;;; tc1, tc2, tc3:
;;;     abbreviations for calls to -termcap_compile-
;;;     use tc1 for simple strings;
;;;         tc2 for P*-type strings;
;;;         tc3 for NP-type strings

define lconstant tc1 =
    termcap_compile(% false, 1 %);
enddefine;

define lconstant tc2(s) -> s;
    lvars s;
    if isprocedure(termcap_compile(s, false, true) ->> s) then
        ;;; needs variable amount of padding: supply number of lines from
        ;;; current position to end of screen
        procedure(p);
            lvars p;
            fast_apply(vedscreenlength fi_- vedline fi_+ 1, p);
        endprocedure(% s %) -> s;
    endif;
enddefine;

define lconstant tc3 =
    termcap_compile(% 2, 1 %);
enddefine;

;;; vedtermcapscreenxy, vedtermcapsetscrollregion:
;;;     wrappers for termcap CM and CS procedures

define lconstant vedtermcapscreenxy(col, row, p) with_props vedscreenxy;
    lvars col, row, p;
    ;;; swap row & column
    fast_apply(row, col, p);
    ;;; set new cursor position
    col -> vedscreencolumn;
    row -> vedscreenline;
enddefine;

define lconstant vedtermcapsetscrollregion(/* top, bottom, p */)
with_props vedsetscrollregion with_nargs 3;
    fast_apply(/* top, bottom, p */);
    ;;; make cursor position undefined
    1000 ->> vedscreenline -> vedscreencolumn;
enddefine;

;;; vedtermcapscreen:
;;;     initialise VED screen-control variables from a named termcap entry

define vedtermcapscreen(name);
    lvars x, y, name;

    ;;; find an entry for the terminal name
    returnunless(termcap_getentry(name));

    ;;; set the terminal name
    consword($-Sys$-termcap_entry_name) -> vedterminalname;
    false -> vedterminalselect;

    ;;; establish VM context for any code planted
    sysCOMPILE(procedure;

        ;;; set screen size
        if termcap_getnum(LI) ->> x then x -> vedscreenlength endif;
        if termcap_getnum(CO) ->> x then x -> vedscreenwidth endif;
        termcap_getflag(AM) -> vedscreenwrap;

        ;;; basic cursor motion
        if termcap_getstring(UP) ->> x then tc1(x) -> vvedscreencharup endif;
        if termcap_getstring(DO) ->> x then tc1(x) -> vvedscreenchardown endif;
        if termcap_getstring(ND) ->> x then tc1(x) -> vvedscreencharright endif;
        if termcap_getstring(LE) ->> x then
            tc1(x) -> vvedscreencharleft;
        elseif not(termcap_getflag(BS)) and (termcap_getstring(BC) ->> x) then
            tc1(x) -> vvedscreencharleft;
        endif;
        if termcap_getstring(CM) ->> x then
            vedtermcapscreenxy(% tc3(x) %) -> vedscreenxy;
        endif;

        ;;; clear screen
        if termcap_getstring(CL) ->> x then tc1(x) -> vvedscreenclear endif;
        if termcap_getstring(CE) ->> x then tc1(x) -> vvedscreencleartail endif;

        ;;; scrolling
        if termcap_getstring(SF) ->> x then
            tc1(x) -> vvedscreenscrollup;
        elseif termcap_getflag(NS) then
            nullstring -> vvedscreenscrollup;
        endif;
        if termcap_getstring(SR) ->> x then tc1(x) -> vvedscreenscrolldown endif;
        if termcap_getstring(CS) ->> x then
            vedtermcapsetscrollregion(% tc3(x) %) -> vedsetscrollregion;
        endif;

        ;;; character insert
        if termcap_getstring(IM) ->> x then
            false -> vednocharinsert;
            tc1(x) -> vvedscreeninsertmode;
            if termcap_getstring(EI) ->> x then tc1(x) -> vvedscreenovermode endif;
            unless vvedscreeninsertmode = nullstring or termcap_getflag('MI') then
                true -> vednomoveinsert;
            endunless;
        endif;
        if termcap_getstring(IC) ->> x then
            false -> vednocharinsert;
            tc1(x) -> vvedscreeninsertchar;
        endif;

        ;;; character delete
        if termcap_getstring(DC) ->> x then
            false -> vednochardelete;
            tc1(x) -> vvedscreendeletechar;
            ;;; if there's a seperate delete mode ...
            if (termcap_getstring(DM) ->> x) and (termcap_getstring(ED) ->> y)
            then
                ;;; ... construct: start_delete <> delete_char <> end_delete
                tc1(x) <> vvedscreendeletechar <> tc1(y) -> vvedscreendeletechar;
            endif;
        endif;

        ;;; line insert (may need a procedure for variable padding)
        if termcap_getstring(AL) ->> x then
            false -> vednolineinsert;
            tc2(x) -> vvedscreeninsertline;
        endif;

        ;;; line delete (may need a procedure for variable padding)
        if termcap_getstring(DL) ->> x then
            false -> vednolinedelete;
            tc2(x) -> vvedscreendeleteline;
        endif;

        ;;; terminal bell (audible bell preferred)
        if (termcap_getstring(BL) ->> x) or (termcap_getstring(VB) ->> x) then
            tc1(x) -> vvedscreenbell;
        endif;

        ;;; graphic mode
        if termcap_getstring(AS) ->> x then
            tc1(x) -> vvedscreengraphic;
            if termcap_getstring(AE) ->> x then tc1(x) -> vvedscreenalpha endif;
        endif;

        ;;; character display attributes
        if termcap_getstring(ME) ->> x then
            ;;; using newer terminfo-type attributes
            tc1(x) -> vvedscreencharnormal;
            if termcap_getstring(MB) ->> x then
                tc1(x) -> vvedscreencharblink;
            endif;
            if termcap_getstring(MD) ->> x then
                tc1(x) -> vvedscreencharbold;
            endif;
            if termcap_getstring(MR) ->> x then
                tc1(x) -> vvedscreencharhighlight;
            endif;
        elseif termcap_getstring(SO) ->> x then
            ;;; older "standout" mode
            tc1(x) -> vvedscreencharaltfont;
            if termcap_getstring(SE) ->> x then
                tc1(x) -> vvedscreencharnormal;
            endif;
        endif;
        if termcap_getstring(US) ->> x then
            ;;; underscore mode
            tc1(x) -> vvedscreencharunderline;
            if termcap_getstring(UE) ->> x then
                tc1(x) -> x;
                unless issubstring(x, vvedscreencharnormal) then
                    x <> vvedscreencharnormal -> vvedscreencharnormal;
                endunless;
            endif;
        endif;

        ;;; initialisation
        if termcap_getstring(KS) ->> x then
            false -> vednokeypad;
            tc1(x) -> vvedscreensetpad;
            if termcap_getstring(KE) ->> x then tc1(x) -> vvedscreenresetpad endif;
        endif;
        if termcap_getstring(TI) ->> x then tc1(x) -> vvedscreeninit endif;
        if termcap_getstring(TE) ->> x then tc1(x) -> vvedscreenreset endif;

    endprocedure);
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 15 Mar 2004
        Disabled use of termcap on Linux systems
--- Robert Duncan, Aug 14 1997
        Disabled use of termcap on NCR Unix
--- Robert John Duncan, Mar 16 1992
        Changed to use "standout" mode for alternate font.
--- Robert Duncan, Feb  3 1992
        Added control for blinking characters
--- Robert John Duncan, Jan  7 1992
        Added controls for character display attributes: bold, highlight and
        underline.
--- Rob Duncan, Jun 21 1990
        Termcap procedure now called -vedtermcapscreen-.
--- Rob Duncan, Nov 15 1989
        Moved out all string compilation procedures to "termcap.p":
        -termcap_compile- now provides a more general interface.
        Moved the call to -sysCOMPILE- into -vedusetermcap-, surrounding
        all possible calls to VM procedures from -termcap_compile-.
        Put everything in section Sys$-Ved.
--- Rob Duncan, Nov 10 1989
        Added calls to -sysCOMPILE- in -compile_vedscreenxy/setscrollregion-
 */
