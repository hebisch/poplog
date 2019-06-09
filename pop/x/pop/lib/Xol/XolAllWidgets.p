/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/XolAllWidgets.p
 > Purpose:         Load all Xol widgets
 > Author:          John Gibson, Apr 25 1993 (see revisions)
 > Documentation:   HELP * OPENLOOK
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses
    xolAbbrevMenuButtonWidget,
    xolArrowWidget,
    xolBaseWindowShellWidget,
    xolBulletinBoardWidget,
    xolButtonGadget,
    xolButtonWidget,
    xolCaptionWidget,
    xolCheckBoxWidget,
    xolControlAreaWidget,
    xolEventObj,
    xolExclusivesWidget,
    xolFlatCheckBoxWidget,
    xolFlatExclusivesWidget,
    xolFlatNonexclusivesWidget,
    xolFlatWidget,
    xolFooterPanelWidget,
    xolFormWidget,
    xolHelpWidget,
    xolListPaneWidget,
    xolMagWidget,
    xolManagerWidget,
    xolMenuButtonGadget,
    xolMenuButtonWidget,
    xolMenuShellWidget,
    xolNonexclusivesWidget,
    xolNoticeShellWidget,
    xolOblongButtonGadget,
    xolOblongButtonWidget,
    xolPopupWindowShellWidget,
    xolPushpinWidget,
    xolRectButtonWidget,
    xolScrollbarWidget,
    xolScrolledWindowWidget,
    xolScrollingListWidget,
    xolSliderWidget,
    xolStaticTextWidget,
    xolStubWidget,
    xolTextEditWidget,
    xolTextFieldWidget,
;

#_IF XOL_VERSION > 2000
    ;;; new in OLIT version 2.5
uses
    xolGaugeWidget,
;
#_ENDIF

#_IF XOL_VERSION >= 3000
    ;;; new in OLIT version 3.0
uses
    xolDrawAreaWidget,
    xolDropTargetWidget,
    xolRubberTileWidget,
;
#_ENDIF

#_IF XOL_VERSION <= 3000
    ;;; dropped from OLIT version 3.0.1
uses
    xolTextWidget,
    xolTextPaneWidget,
;
#_ENDIF

constant XolAllWidgets = true;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 11 1994
        Changed test for gauge widget to be > 2000
 */
