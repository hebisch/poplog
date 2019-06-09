/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/XmAllWidgets.p
 > Purpose:         Load all Xm widgets
 > Author:          John Gibson, Apr 25 1993
 > Documentation:   HELP * MOTIF
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses
    xmArrowButtonGadget,
    xmArrowButtonWidget,
    xmBulletinBoardWidget,
    xmCascadeButtonGadget,
    xmCascadeButtonWidget,
    xmCommandWidget,
    xmDialogShellWidget,
    xmDrawingAreaWidget,
    xmDrawnButtonWidget,
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
    xmScrollBarWidget,
    xmScrolledWindowWidget,
    xmSelectionBoxWidget,
    xmSeparatorGadget,
    xmSeparatorWidget,
    xmTextFieldWidget,
    xmTextWidget,
    xmToggleButtonGadget,
    xmToggleButtonWidget,
;

constant XmAllWidgets = true;

endexload_batch;
endsection;
