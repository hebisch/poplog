/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpw/xpwPixmapWidget.p
 > Purpose:         Xpw widgetclass
 > Author:          John Gibson, Apr  5 1993 (see revisions)
 > Documentation:   REF * XpwPixmap
 > Related Files:   Xpw/xpw*Widget.p
 */
compile_mode :pop11 +strict;

uses-now XptSpecifyResourceInfo;

section;
exload_batch;

uses XpwCallMethod;

XptLoadWidgetClass xpwPixmapWidget [^^XPW_EXLIBS]
    xpwPixmapWidget <- xpwPixmapWidgetClass
;

XptSpecifyResourceInfo( xpwPixmapWidget, [
    [ ;;; Widget resources
        [function Int]      ;;; {GCFunction 32}]
        [planeMask Int]
        [lineWidth Int]
        [lineStyle Int]     ;;; {GCLineStyle 32}]
        [capStyle Int]      ;;; {GCCapStyle 32}]
        [joinStyle Int]     ;;; {GCJoinStyle 32}]
        [fillStyle Int]
        [fillRule Int]
        [arcMode Int]
        [tile Pixmap]
        [stipple Bitmap]
        [tileStipXOrigin Int]
        [tileStipYOrigin Int]
        [subwindowMode Int] ;;; {GCSubwindowMode 32}]
        [clipXOrigin Int]
        [clipYOrigin Int]
        [clipMask Bitmap]
        [dashOffset Int]
        [dashes Int]
    ]
    ;;; Constraint resources
    ^false
]);

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 13 1995
        Changed Widget resources to use new built-in type sizes
 */
