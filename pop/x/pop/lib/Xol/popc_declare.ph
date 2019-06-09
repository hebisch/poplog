/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/popc_declare.ph
 > Purpose:         Identifier declarations for POPC in this directory
 > Author:          John Gibson, Apr 16 1993
 */

uses-now popxlib;

library_declare_section '$usepop/pop/x/pop/lib/Xol/'

section;

weak global constant procedure (
        OlListItemPointer,
        XpolAddListItem,
        consXpolListItemPtr,
    );

weak global constant
        XolDynamic,
        XolFlat,
        XolManager,
        XolMenu,
        Xolbuffer,
        Xolgeneral,
        xolAbbrevMenuButtonWidget,
        xolArrowWidget,
        xolBaseWindowShellWidget,
        xolBulletinBoardWidget,
        xolButtonGadget,
        xolButtonWidget,
        xolCaptionWidget,
        xolCheckBoxWidget,
        xolControlAreaWidget,
        xolDrawAreaWidget,
        xolDropTargetWidget,
        xolEventObj,
        xolExclusivesWidget,
        xolFlatCheckBoxWidget,
        xolFlatExclusivesWidget,
        xolFlatNonexclusivesWidget,
        xolFlatWidget,
        xolFooterPanelWidget,
        xolFormWidget,
        xolGaugeWidget,
        xolHelpWidget,
        xolListPaneWidget,
        xolMagWidget,
        xolManagerWidget,
        xolMenuButtonGadget,
        xolMenuButtonWidget,
        xolMenuShellWidget,
        xolNonexclusivesWidget,
        xolNoticeShellWidget,
        xolOblongButtonGadget
        xolOblongButtonWidget,
        xolPopupWindowShellWidget,
        xolPushpinWidget,
        xolRectButtonWidget,
        xolRubberTileWidget,
        xolScrollbarWidget,
        xolScrolledWindowWidget,
        xolScrollingListWidget,
        xolSliderWidget,
        xolStaticTextWidget,
        xolStubWidget,
        xolTextEditWidget,
        xolTextFieldWidget,
        xolTextPaneWidget,
        xolTextWidget,
    ;

endsection;

end_library_declare_section;
