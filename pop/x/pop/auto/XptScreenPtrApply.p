/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptScreenPtrApply.p
 > Purpose:         Class apply for ScreenPtr datatypes
 > Author:          Jonathan Meyer, Jun 27 1991 (see revisions)
 > Documentation:   REF *XPT_CLASSAPPLY
 > Related Files:   LIB *XptWidgetApply
 */
compile_mode :pop11 +strict;

section;

uses fast_xpt_screeninfo;
uses xpt_typecheck;

define global XptScreenPtrApply(name, desc);
    lvars name, desc, result;
    lconstant map = [
        black_pixel     ^fast_XBlackPixelOfScreen
        default_gc      ^fast_XDefaultGCOfScreen
        depth           ^fast_XPlanesOfScreen
        display         ^fast_XDisplayOfScreen
        height          ^fast_XHeightOfScreen
        mheight         ^fast_XHeightMMOfScreen
        mwidth          ^fast_XWidthMMOfScreen
        root            ^fast_XRootWindowOfScreen
        root_depth      ^fast_XPlanesOfScreen
        white_pixel     ^fast_XWhitePixelOfScreen
        width           ^fast_XWidthOfScreen
        num_cells       ^fast_XCellsOfScreen
        min_cmaps       ^fast_XMinCmapsOfScreen
        max_cmaps       ^fast_XMaxCmapsOfScreen
        save_unders     ^fast_XDoesSaveUnders
        does_backing    ^XptDoesBackingStore
        event_mask      ^fast_XEventMaskOfScreen
        default_cmap    ^fast_XDefaultColormapOfScreen
        number          ^fast_XScreenNumberOfScreen
    ];
    XptCheckScreenPtr(desc)->;
    if fast_lmember(name, map) ->> result then
        fast_apply(desc, fast_front(fast_back(result)))
    else
        mishap(name,1,'UNKNOWN ScreenPtr FIELD');
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul  5 1993
        Added "number" option.
--- Adrian Howard, Jul  4 1993
        Added "colormap" option.
--- Adrian Howard, Jun 30 1993
     #  Added other fields from LIB * FAST_XPT_SCREENINFO
     #  Now uses LIB * FAST_XPT_SCREENINFO
     #  Now uses XptCheckScreenPtr
 */
