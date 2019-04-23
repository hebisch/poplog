/* --- Copyright University of Sussex 2012.  All rights reserved. ---------
 > File:        C.all/ved/src/vdscroll.p
 > Purpose:     SCROLLING AND ALIGNING SCREEN
 > Author:      Unknown (see revisions)
 */

#_INCLUDE 'vddeclare.ph'

constant
        procedure (vedscreenoutput, vedscreenovermode, vedcurrentchar,
        ved_winrel_column, Sys$-Ved$-Refresh_lower)
    ;

vars
        procedure (vedscreeninsertchar, vedscreendeletechar,
        vedscreeninsertline, vedscreendeleteline,
        vedscreenscrollregionup, vedscreenscrollregiondown,
        ),
        vedvarwidthmode
    ;

;;; -----------------------------------------------------------------------

section $-Sys$-Ved =>   vedvscr_offsets_of_column, vedvscr_average_width,
                        vedvscr_space_width, vedmaxscrollvert, vedmaxscrollhor,
                        vedalignscreen, vedcheck, vedscrollregion,
                        ved_terminal_can_scroll,
                        vedscrollvert, vedscrollhorz;

vars
    vedmaxscrollvert    = false,
    vedmaxscrollhor     = false,
    ;

;;; Added 8 Nov 2011 terminal_can_scroll is used in vdscreen.p

define active:1 ved_terminal_can_scroll();
    
    terminal_can_scroll

enddefine;

define updaterof active:1 ved_terminal_can_scroll();
    
    -> terminal_can_scroll

enddefine;


;;; --- VARIABLE-WIDTH PROCEDURES -----------------------------------------
;;; Redefined by XVed in variable-width mode

define vars vedvscr_offsets_of_column(string, col, alt_char);
    col fi_- 1, col
enddefine;

define vars vedvscr_average_width(cols);
    cols
enddefine;

define vars vedvscr_space_width(cols);
    cols
enddefine;


;;; -----------------------------------------------------------------------

define lconstant textrightlim(target_col) -> lim;
    lvars target_col, lim = vedcolumnoffset fi_+ vedwlinewidth;
    unless vedvarwidthmode then
        if (target_col or lim) fi_< vvedlinesize then
            ;;; not on the 'more' mark
            lim fi_- 1 -> lim
        endif
    endunless
enddefine;


    /*  Ensure cursor on screen.
        Move current line near middle if necessary
    */
define lconstant Align_screen();
    lvars windowrow, inwindow, col, n, limit, soffs, loffs, wlinewidth;
    vedsetlinesize();
    if vedediting then
        vedline fi_- vedlineoffset -> windowrow;
        false -> inwindow;
        if windowrow fi_> 0 and windowrow fi_< vedwindowlength then
            true -> inwindow;
            vedlineoffset
        elseif vedmaxscrollvert
        and abs(windowrow) fi_< (2 fi_* vedwindowlength fi_- 2)
        then
            ;;; behave as if it had scrolled to get cursor just on screen
            if windowrow fi_>= 0 then
                vedline fi_- vedwindowlength fi_+ 1
            else
                vedline fi_- 1
            endif
        elseif vedline fi_< vedwindowlength then
            0
        elseif vedline fi_> vvedbuffersize then
            vedline fi_- vedwindowlength  fi_+  1
        elseif vedline fi_> vvedbuffersize fi_- vedwindowlength fi_+ 1 then
            vvedbuffersize fi_- vedwindowlength  fi_+  1
        else
            ;;; put line in middle of window
            vedline fi_- (vedwindowlength >> 1)
        endif -> vedlineoffset;

        vedvscr_offsets_of_column(Buffer_line(vedline), vedcolumn, false)
                                                    -> (soffs, loffs);
        textrightlim(false) -> limit;
        limit fi_- vedcolumnoffset -> wlinewidth;
        if inwindow and vedcolumnoffset fi_<= soffs and loffs fi_<= limit then
            ;;; do nothing
        elseif loffs fi_<= wlinewidth then
            if vedonstatus or vedcolumn fi_>= vvedlinesize
            or wlinewidth fi_- loffs fi_> vedvscr_average_width(8) then
                0
            else
                vedcolumn -> col;
                vedwordright();
                col, vedcolumn fi_- 1 -> (vedcolumn, col);
                ;;; col is now at next word boundary
                lvars (soffs2, loffs2) =
                    vedvscr_offsets_of_column(Buffer_line(vedline), col, false);
                if loffs2 fi_<= wlinewidth then
                    0
                elseif loffs2 fi_- loffs fi_< (wlinewidth fi_>> 1) then
                    loffs2 fi_- wlinewidth
                else
                    loffs fi_- (wlinewidth fi_>> 1)
                endif
            endif -> vedcolumnoffset
        else
            (wlinewidth fi_* 3) fi_// 4 -> (, n);
            soffs fi_- n -> vedcolumnoffset
        endif
    endif;
    fi_max(0, vedlineoffset) -> vedlineoffset;
    fi_max(0, vedcolumnoffset) -> vedcolumnoffset
enddefine;      /* Align_screen */

define vedalignscreen();
    Align_screen();
    vedrefreshwindow(true);
enddefine;

;;; vedcheck:
;;;     ensure cursor on screen

define vedcheck();
    lvars wrline, coloffs, lastcol;

    define lconstant do_scrolling();
        lvars soffs, loffs, limit, maxscrollhor, maxscrollvert, n;
        dlocal vedscreencursoron = false;

        vedvscr_offsets_of_column(Buffer_line(vedline), vedcolumn, false)
                                                    -> (soffs, loffs);
        textrightlim(false) -> limit;

        ;;; vedmaxscrollhor and vedmaxscrollvert control the amount of
        ;;; scrolling
        10 -> maxscrollhor;
        if vedmaxscrollhor then
            vedmaxscrollhor -> maxscrollhor;
            if isprocedure(maxscrollhor) then
                fast_apply(maxscrollhor) -> maxscrollhor
            endif;
            unless isinteger(maxscrollhor) and maxscrollhor fi_>= 0 then
                ;;; should be a mishap, but that makes Ved unusable
                10 -> maxscrollhor
            endunless
        endif;
        vedvscr_average_width(maxscrollhor) -> maxscrollhor;

        ;;; scroll only if column not too far left or right
        if soffs fi_< vedcolumnoffset fi_- maxscrollhor
        or vednocharinsert and soffs fi_< vedcolumnoffset
        or loffs fi_> limit fi_+ maxscrollhor
        or vednochardelete and loffs fi_> limit
        then
            ;;; too far to scroll
            if ved_winrel_line(vedline) then
                if loffs fi_> limit then
                    if vvedlinesize fi_> vedcolumn then
                        Align_screen()
                    else
                        loffs fi_- limit fi_+ vedcolumnoffset
                                                -> vedcolumnoffset
                    endif
                else
                    soffs -> vedcolumnoffset
                endif
            else
                Align_screen()
            endif;
            vedrefreshwindow(true);
            return
        endif;

        vedwindowlength -> maxscrollvert;
        if vedmaxscrollvert then
            vedmaxscrollvert -> maxscrollvert;
            if isprocedure(maxscrollvert) then
                fast_apply(maxscrollvert) -> maxscrollvert;
            endif;
            unless isinteger(maxscrollvert) and maxscrollvert fi_>= 0 then
                ;;; should be a mishap, but that makes Ved unusable
                vedwindowlength -> maxscrollvert;
            endunless;
        endif;

        ;;; scroll only if line not too far up or down
        if vedmaxscrollvert = 0 and vedline fi_<= vedlineoffset then
            if vedline < 1 then 1 -> vedline; endif;
            vedline -1 -> vedlineoffset;
            vedalignscreen();
        elseif vedline fi_<= vedlineoffset fi_- maxscrollvert
        or vedline fi_> Bottom_of_window() fi_+ maxscrollvert
        then
            vedalignscreen();
            return;
        endif;

        ;;; now scroll left or right, up or down, as necessary
        if (loffs fi_- textrightlim(vedcolumn) ->> n) fi_> 0
        or (soffs fi_- vedcolumnoffset ->> n) fi_< 0
        then
            vedscrollhorz(n)
        endif;

        if (vedline fi_- Bottom_of_window() ->> n) fi_> 0 then
            vedscrollvert(n)
        elseif (vedline fi_- vedlineoffset ->> n) fi_<= 0 then
            vedscrollvert(n fi_- 1)
        endif;
    enddefine;

    fi_max(vedline, 1) -> vedline;
    fi_max(vedcolumn, 1) -> vedcolumn;

    if ved_winrel_line(vedline) ->> wrline then
        unless ved_winrel_column(wrline, vedcolumn, 2:000) then
            do_scrolling()
        endunless
    elseif ved_on_status then
        vedalignscreen();
        return
    else
        do_scrolling()
    endif;
    if vedrefreshneeded then Refresh_lower(true) endif
enddefine;

define Default_scrollregion(_top, _bottom, _nlines, _buffline)
                                            with_props vedscrollregion;
    lvars scrollup, scrollpdr, _top, _bottom, _nlines, _buffline;
    returnunless(vedediting);
    if _nlines fi_< 0 then
        if _bottom fi_+ _nlines fi_< 2 then
            _buffline fi_- _nlines fi_- _bottom fi_+ 1 -> _buffline;
            2 -> _top;
            false, false, _bottom fi_- 1
        else
            true, vedscreenscrollregionup, 0 fi_- _nlines
        endif;
    else
        false;
        if _top fi_+ _nlines fi_> vedwindowlength then
            false, vedwindowlength fi_+ 1 fi_- _top;
        else
            vedscreenscrollregiondown, _nlines
        endif;
    endif -> _nlines -> scrollpdr -> scrollup;
    until _nlines == 0 do
        if scrollpdr then scrollpdr(_top, _bottom) endif;
        unless vedrefreshneeded then
            vedrefreshline(scrollpdr, if scrollup then _bottom else _top endif,
                 Buffer_line(_buffline), vedmarked(_buffline));
        endunless;
        unless scrollup then
            _top fi_+ 1 -> _top;
            fi_min(_bottom fi_+ 1, vedwindowlength)  -> _bottom;
        endunless;
        _buffline fi_+ 1 -> _buffline;
        _nlines fi_- 1 -> _nlines;
    enduntil;
enddefine;
;;;
vars procedure vedscrollregion = Default_scrollregion;

define Default_scrollvert(nrows) with_props vedscrollvert;
    lvars nrows, line;
    if nrows fi_< 0 then
        ;;; scroll down
        fast_repeat -nrows times
            returnif(vedlineoffset == 0);
            vedscreenpulldown(vedwindowlength);
            unless vedrefreshneeded then
                vedrefreshline(true, 2, Buffer_line(vedlineoffset),vedmarked(vedlineoffset));
            endunless;
            vedlineoffset fi_- 1 -> vedlineoffset
        endrepeat
    else
        ;;; scroll up
        fast_repeat nrows times
            vedscreenpushup(vedwindowlength);
            vedlineoffset fi_+ vedwindowlength -> line;
            unless vedrefreshneeded then
                vedrefreshline(true, vedwindowlength, Buffer_line(line), vedmarked(line))
            endunless;
            vedlineoffset  fi_+  1 -> vedlineoffset;
        endrepeat
    endif
enddefine;
;;;
vars procedure vedscrollvert = Default_scrollvert;

define Default_scrollhorz(ncols) with_props vedscrollhorz;
    lvars   col, string, ncols, _row, _size, _lim, _wline,
            procedure subscrp = if vedonstatus then fast_subscrvedstring
                                else fast_subscrdstring
                                endif;
    dlocal vedscreencursoron = false;

    if ncols fi_< 0 then
        ;;; scroll right
        if vednocharinsert or vednomoveinsert then
            ;;; no insert mode, or else can't move while inserting
            fi_max(0, vedcolumnoffset fi_+ ncols) -> vedcolumnoffset;
            Set_refresh_needed(2);
            return;
        endif;
        fast_repeat -ncols times
            vedcolumnoffset -> col;     ;;; column of new character to be inserted
            returnif(col == 0);
            col fi_- 1 -> vedcolumnoffset;
            vedcolumnoffset fi_+ vedwlinewidth -> _lim;
            1 -> _row;
            until _row == vedwindowlength do
                Buffer_line(_row fi_+ vedlineoffset) -> string;
                vedusedsize(string) -> _size;
                if _size fi_>= col then
                    _row fi_+ vedwlineoffset -> _wline;
                    vedwindowpoint(_wline, vedwcolumnoffset fi_+ 1);
                    vedscreeninsertchar(subscrp(col,string));
                    if _size fi_> _lim then
                        vedwindowpoint(_wline, vedscreenwidth);
                        if vedscreenwrap then
                            ;;; insertion will have overflowed into last column
                            vedscreencleartail();
                        endif;
                        vedscreenoutput(vedscreenmoremark)
                    endif
                endif;
                _row fi_+ 1 -> _row;
            enduntil;
            vedscreenovermode();
        endrepeat
    else
        ;;; scroll left
        if vednochardelete then
            ;;; dumb vdu - refresh whole window
            vedcolumnoffset fi_+ ncols -> vedcolumnoffset;
            Set_refresh_needed(2);
            return
        endif;
        fast_repeat ncols times
            vedcolumnoffset fi_+ 1 -> vedcolumnoffset;
            vedcolumnoffset fi_+ vedwlinewidth -> _lim;
            1 -> _row;        ;;; row number in window
            until _row == vedwindowlength do
                Buffer_line(_row fi_+ vedlineoffset) -> string;
                vedusedsize(string) -> _size;
                if _size fi_>= vedcolumnoffset then
                    _row fi_+ vedwlineoffset -> _wline;
                    vedwindowpoint(_wline, vedwcolumnoffset fi_+ 1);
                    vedscreendeletechar();  ;;; pull line left one space
                    ;;; now insert characters at right edge
                    if _size fi_>= _lim then
                        ;;; refresh last two characters, for safety
                        ;;; indicate with vedscreenmoremark if there's more to right
                        vedwindowpoint(_wline, vedscreenwidth fi_- 1);
                        vedscreenoutput(subscrp(_lim fi_- 1, string));
                        vedscreenoutput(
                            if _size fi_> _lim then vedscreenmoremark
                            else subscrp(_lim, string)
                            endif)
                    endif;
                endif;
                _row fi_+ 1 -> _row;
            enduntil;
        endrepeat
    endif
enddefine;
;;;
vars procedure vedscrollhorz = Default_scrollhorz;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 8 Nov 2011 (25 Sep 2012)
    Added new active variable with updater
        ved_terminal_can_scroll();
    to control the previously hidden variable:
        terminal_can_scroll
    whose default value is now set false in vdinitseq.p
        
--- John Gibson, Aug 15 1997
        Changes for XVed variable-width mode
--- John Gibson, Dec 13 1996
        Changed Default_scrollhorz to use correct subscripting procedure.
--- John Gibson, Jun  3 1992
        Made -vedscrollregion- vars (so XVed can redefine it)
--- John Gibson, Jan 23 1992
        Replaced vedscreenm*ark with -vedscreenmoremark-
--- John Gibson, Oct 26 1991
        vedscrollup/down replaced with vedscrollvert(nrows) and
        vedscrollleft/right replaced with vedscrollhorz(ncols).
        Sectioned file.
--- John Gibson, Sep  6 1991
        Added turning off/on of cursor in do_scrolling
--- John Gibson, Jul  6 1991
        Added turning off/on of cursor in vedscrollright/left
--- Rob Duncan, May  2 1990
        Minor mods to -vedcheck-
--- Rob Duncan, Apr 24 1990
        Rewrote -vedcheck- to eliminate some oddities and minor bugs: should
        stop unnecessary refreshing when the cursor's in the last screen
        column, and stop the cursor ever ending up over the screen mark.
        Fixed -vedscrollleft- to check -vednochardelete- rather than
        -vednocharinsert-.
--- Rob Duncan, Nov  7 1989
        Added tests of -vednochardelete- in -vedcheck- and -vednomoveinsert-
        and -vedscreenwrap- in -vedscrollright-
--- John Gibson, Feb  9 1988
        Removed use of floating point in Align_screen
--- John Gibson, Dec  6 1987
        Lconstant'ed. Got rid of "exported" keyword (everything now exported
        unless explicitly none*xported).
--- Ben Rubinstein, Feb  6 1987 - added check for nocharinsert in -vedcheck-
--- A.Sloman (???) 23-10-86 - added vedmaxscrollvert vedmaxscrollhor
    Altered vedalignscreen not always to put cursor in middle
    Altered vedalignscreen and vedcheck to ensure horizontal context visible
*/
