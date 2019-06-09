/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpw/XpwScrollText.p
 > Purpose:         Methods in support of XpwScrollText widgetclass
 > Author:          Jon Meyer, Jan 1990 (see revisions)
 > Documentation:   REF * XpwScrollText, HELP * XpwScrollText
 > Related Files:   LIB * XpwScrollText, * XpwScrollText.ph
 */
compile_mode:pop11 +strict;

include XpwScrollText.ph;

section;
exload_batch;

uses XpwCore, xpwScrollTextWidget;

define lconstant bool(/*val*/) with_props false;
    unless isboolean(dup()) then
        mishap((), 1, 'BOOLEAN NEEDED');
    endunless;
enddefine;

define XpwTextScroll(w, col, row, cols, rows, dx, dy);
    lvars w, col, row, cols, rows, dx, dy;
    XpwCallMethod(XptCheckWidget(w),XpwMScroll,
            fi_check(col,0,false), fi_check(row,0,false),
            fi_check(cols,0,false), fi_check(rows,0,false),
            fi_check(dx,false,false), fi_check(dy,false,false), 8, false);
enddefine;

define XpwTextScrollScreenUp(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMScrollScreenUp, 2,false);
enddefine;

define XpwTextScrollScreenDown(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMScrollScreenDown, 2,false);
enddefine;

define XpwTextScrollScreenLeft(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMScrollScreenLeft, 2,false);
enddefine;

define XpwTextScrollScreenRight(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMScrollScreenRight, 2,false);
enddefine;

define XpwTextScrollLines(w, row, num_rows, dist);
    lvars w, row, num_rows, dist;
    XpwCallMethod(XptCheckWidget(w), XpwMScrollLines,
        fi_check(row,0,false), fi_check(num_rows,0,false),
        fi_check(dist,false,false), 5,false);
enddefine;

define XpwTextScrollTail(w, col, row, dist);
    lvars w, col, row, dist;
    XpwCallMethod(XptCheckWidget(w), XpwMScrollTail,
        fi_check(col,0,false), fi_check(row,0,false),
        fi_check(dist,false,false), 5,false);
enddefine;

define XpwTextScrollTails(w, col, row, num_rows, dist);
    lvars w, col, row, dist num_rows;
    XpwCallMethod(XptCheckWidget(w), XpwMScrollTail,
        fi_check(col,0,false), fi_check(row,0,false),
        fi_check(num_rows,0,false), fi_check(dist,false,false), 6,false);
enddefine;

define XpwTextScrollTailLeft(w, col, row);
    lvars w, col, row;
    XpwCallMethod(XptCheckWidget(w), XpwMScrollTailLeft,
        fi_check(col,0,false), fi_check(row,0,false), 4, false);
enddefine;

define XpwTextScrollTailRight(w, col, row);
    lvars w, col, row;
    XpwCallMethod(XptCheckWidget(w), XpwMScrollTailRight,
        fi_check(col,0,false), fi_check(row,0,false), 4, false);
enddefine;

define XpwTextInsertLineAtCursor(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMInsertLineAtCursor, 2, false);
enddefine;

define XpwTextInsertCharAtCursor(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMInsertCharAtCursor, 2, false);
enddefine;

define XpwTextDeleteLineAtCursor(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMDeleteLineAtCursor, 2, false);
enddefine;

define XpwTextDeleteCharsAtCursor(w, nchars);
    XpwCallMethod(XptCheckWidget(w), XpwMDeleteCharsAtCursor,
                    fi_check(nchars,0,false), 3, false);
enddefine;

define XpwTextDeleteCharAtCursor =
    XpwTextDeleteCharsAtCursor(%1%)
enddefine;

define XpwTextClear(w, col, row, cols, rows);
    lvars w, col, row, cols, rows;
    XpwCallMethod(XptCheckWidget(w), XpwMClear,
            fi_check(col,0,false), fi_check(row,0,false),
            fi_check(cols,0,false), fi_check(rows,0,false),
            6, false);
enddefine;

define XpwTextClearScreen(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMClearScreen, 2, false);
enddefine;

define XpwTextClearLine(w, start_line);
    lvars w, start_line;
    XpwCallMethod(XptCheckWidget(w),XpwMClearLine,
                            fi_check(start_line,0,false), 3,false);
enddefine;

define XpwTextClearLines(w, start_line, num_lines);
    lvars w, start_line, num_lines;
    XpwCallMethod(XptCheckWidget(w),XpwMClearLines,
            fi_check(start_line,0,false),
            fi_check(num_lines,0,false), 4, false);
enddefine;

define XpwTextClearTail(w, col,row);
    lvars w, col, row;
    XpwCallMethod(XptCheckWidget(w),XpwMClearTail,
            fi_check(col,0,false), fi_check(row,0,false), 4, false);
enddefine;

define XpwTextClearTails(w, col,row, num_lines);
    lvars w, col, row, num_lines;
    XpwCallMethod(XptCheckWidget(w),XpwMClearTails,
            fi_check(col,0,false), fi_check(row,0,false),
            fi_check(num_lines,0,false), 5, false);
enddefine;

define XpwTextClearChar(w, col, row);
    lvars w, col, row;
    XpwCallMethod(XptCheckWidget(w),XpwMClearChar,
            fi_check(col,0,false), fi_check(row,0,false), 4, false);
enddefine;

define XpwTextClearLineAtCursor(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w),XpwMClearLineAtCursor, 2, false);
enddefine;

define XpwTextClearTailAtCursor(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w),XpwMClearTailAtCursor, 2, false);
enddefine;

define XpwTextClearCharAtCursor(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w),XpwMClearCharAtCursor, 2, false);
enddefine;

define XpwTextInsert(w, col, row,string);
    lvars w, col, row, string;
    XpwCallMethod(XptCheckWidget(w),XpwMInsert,
            fi_check(col,0,false), fi_check(row,0,false),
            XptCheckString(string), -datalength(string), 6, false);
enddefine;

define XpwTextInsertAtCursor(w, string);
    lvars w, string;
    XpwCallMethod(XptCheckWidget(w),XpwMInsertAtCursor,
        XptCheckString(string), -datalength(string), 4, false);
enddefine;

define XpwTextWrite(w,col,row,start,nrows,offset1,len,strs,clear);
    lvars w,col,row,offset1,len,strs,clear,array_len,start,nrows;
    length(strs) -> array_len;
    ;;; probably doesn't work - need to build array of strings to pass C
    XpwCallMethod(XptCheckWidget(w), XpwMWrite,
            fi_check(col,0,false), fi_check(row,0,false),
            fi_check(start,0,array_len), fi_check(nrows,0,array_len),
            strs,
            fi_check(offset1,0,false), -fi_check(len,0,false),
            bool(clear),10,false);
enddefine;

define XpwTextWriteLine(w, col,row,string, clear_line);
    lvars w, col, row, string, clear_line;
    XpwCallMethod(XptCheckWidget(w), XpwMWriteLine,
            fi_check(col,0,false), fi_check(row,0,false),
            XptCheckString(string), -datalength(string),
            bool(clear_line), 7, false);
enddefine;

define XpwTextWriteLines(w, col,row, start,nrows,strings,clear_line);
    lvars w, col, row, strings, clear_line, array_len, start, nrows;
    length(strings) -> array_len;
    XpwCallMethod(XptCheckWidget(w), XpwMWriteLines,
            fi_check(col,0,false), fi_check(row,0,false),
            fi_check(start,0,array_len), fi_check(nrows,0,array_len),
            strings,-MAX_INT,
            bool(clear_line), 9, false);
enddefine;


define XpwTextWriteSubstr(w,col,row,offset1,len,str,clear);
    lvars w,col,row,offset1,len,str,clear;
    XpwCallMethod(XptCheckWidget(w), XpwMWriteSubstr,
            fi_check(col,0,false), fi_check(row,0,false),
            XptCheckString(str),
            fi_check(offset1,0,datalength(str)), fi_check(len,0,false),
            bool(clear), 8, false);
enddefine;

define XpwTextWriteAtCursor(w, string);
    lvars w, string;
    XpwCallMethod(XptCheckWidget(w),XpwMWriteAtCursor,
        XptCheckString(string), datalength(string), 4, false);
enddefine;

define XpwTextBell(w,volume);
    lvars w, volume;
    XpwCallMethod(XptCheckWidget(w), XpwMBell, fi_check(volume,0,100), 3, false);
enddefine;

define XpwTextCursorTo(w, col, row);
    lvars w, col, row;
    XpwCallMethod(XptCheckWidget(w), XpwMCursorTo,
        fi_check(col,0,false), fi_check(row,0,false),
        4, false);
enddefine;

define XpwTextCursorUp(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMCursorUp,2,false);
enddefine;

define XpwTextCursorDown(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMCursorDown,2,false);
enddefine;

define XpwTextCursorLeft(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMCursorLeft,2,false);
enddefine;

define XpwTextCursorRight(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMCursorRight,2,false);
enddefine;

define XpwSetCharAttributes(w, attr);
    lvars w, attr;
    XpwCallMethod(XptCheckWidget(w), XpwMSetCharAttributes,
                            fi_check(attr,0,false), 3, false);
enddefine;

define XpwGetCharAttributes(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMGetCharAttributes, 2, "XptInt")
enddefine;

define XpwSetTextCursor(w, cchar);
    lvars w, cchar;
    XpwCallMethod(XptCheckWidget(w), XpwMSetTextCursor,
                            fi_check(cchar,0,false), 3, false);
enddefine;

define XpwGetTextCursor(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMGetTextCursor, 2, "XptInt")
enddefine;

constant XpwScrollText = true;

endexload_batch;
endsection;     /* top level */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 13 1997
        Added XpwTextDeleteCharsAtCursor
--- John Gibson, Jan 14 1994
        Added XpwSetTextCursor and XpwGetTextCursor
--- John Gibson, Apr  3 1993
        Added uses for xpwScrollTextWidget and 'uses Xpw/XpwCore'
--- John Gibson, Feb  2 1992
        Added XpwSet/GetCharAttributes
--- John Gibson, Nov  5 1991
        Corrected some argument names
--- Adrian Howard, Nov  4 1991 : Fixed so it would compile under +strict.
--- Jonathan Meyer, Jul 30 1991
        Changed to use XptCheckWidget rather than xpw_check_live_widget
--- Andreas Schoter, Jul 15 1991
    Added constant XpwScrollText for compatibility with uses
--- Jonathan Meyer, Oct 23 1990
        Removed references to redundant xt_shadow/external_coerce.
        Made use of new XptWidgetSet.
--- Roger Evans, Oct 22 1990 changed to XpwScrollTextand installed
--- James Goodlet, Sep 27 1990 - added defintion of XpwScrollTextMethods for
        -uses-.
--- Andreas Schoter, Jul 23 1990
    Corrected typo of XpwMScrollTailLeft to XpwMScrollTailRight line 92
--- Andreas Schoter, July 16 1990
    Renamed to XpwScrollTextMethods.p and changed all variable names from Pop* to
    Xpw*
--- James Goodlet, Jul 11 1990 - fixed typo in -XpwTextWriteLine-.
--- James Goodlet, Jun  6 1990 - moved out from LIB * PopScrollText - see
        that file for more revision notes.  Also changed to use -fi_check-
        for integer parameters, and use -xt_shadow- more consistently.  All
        procedures made "global"
 */
