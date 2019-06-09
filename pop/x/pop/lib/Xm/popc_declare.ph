/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/popc_declare.ph
 > Purpose:         Identifier declarations for POPC in this directory
 > Author:          John Gibson, Apr 16 1993 (see revisions)
 */

uses-now popxlib;

library_declare_section '$usepop/pop/x/pop/lib/Xm/'

section;

weak global constant procedure (
        XmCreateOptionMenu,
        XmCreatePulldownMenu,
        XmCreateScrolledList,
        XmFontListEntryGetFont,
        XmFontListEntryGetTag
        XmFontListFreeFontContext,
        XmFontListGetNextFont,
        XmFontListInitFontContext,
        XmFontListNextEntry,
        XmInternAtom,
        XmIsList,
        XmIsTextField,
        XmListDeselectAllItems,
        XmListGetSelectedPos,
        XmListItemPos,
        XmListReplaceItemsPos,
        XmListSelectPos
        XmListSetBottomPos,
        XmListSetPos,
        XmOptionButtonGadget,
        XmOptionLabelGadget,
        XmProcessTraversal,
        XmStringWidth,
        XmTextFieldGetInsertionPosition,
        XmTextFieldSetInsertionPosition,
);

weak global constant
        XmAtomMgr,
        Xmgeneral,
        xmArrowButtonGadget,
        xmArrowButtonWidget,
        xmBulletinBoardWidget,
        xmCascadeButtonGadget,
        xmCascadeButtonWidget,
        xmCommandWidget,
        xmDesktopObject,
        xmDialogShellWidget,
        xmDisplayObject,
        xmDrawingAreaWidget,
        xmDrawnButtonWidget,
        xmExtObject,
        xmFileSelectionBoxWidget,
        xmFormWidget,
        xmFrameWidget,
        xmGadget,
        xmLabelGadget,
        xmLabelWidget,
        xmListWidget,
        xmMainWindowWidget,
        xmManagerWidget,
        xmMenuShellWidget,
        xmMessageBoxWidget,
        xmPanedWindowWidget,
        xmPrimitiveWidget,
        xmPushButtonGadget,
        xmPushButtonWidget,
        xmRowColumnWidget,
        xmScaleWidget,
        xmScreenObject,
        xmScrollBarWidget,
        xmScrolledWindowWidget,
        xmSelectionBoxWidget,
        xmSeparatorGadget,
        xmSeparatorWidget,
        xmShellExtObject,
        xmTextFieldWidget,
        xmTextWidget,
        xmToggleButtonGadget,
        xmToggleButtonWidget,
        xmVendorShellExtObject,
        xmWorldObject,
;

endsection;

end_library_declare_section;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  9 1995
        Added declaration for XmOptionLabelGadget
 */
