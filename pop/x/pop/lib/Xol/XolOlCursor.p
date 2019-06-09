/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/XolOlCursor.p
 > Purpose:         OpenLook Cursors
 > Author:          Jonathan Meyer, Sep  2 1990 (see revisions)
 > Documentation:   HELP *OpenLook
 */
compile_mode :pop11 +strict;

section;

XptPopLoadProcedures XolOlCursor [^^XOL_EXLIBS]

#_< lvars i; for i in

#_IF XOL_VERSION < 3000
[       ;;; PRE-OLIT 3.0 CURSORS
    GetOlMoveCursor
    GetOlDuplicateCursor
    GetOlBusyCursor
    GetOlPanCursor
    GetOlQuestionCursor
    GetOlTargetCursor
    GetOlStandardCursor
]

#_ELSE
[       ;;; OLIT 3.0 CURSORS
    OlGet50PercentGrey                        OlGet75PercentGrey
    GetOlBusyCursor                      GetOlDataDupeDragCursor
    GetOlDataDupeDropCursor            GetOlDataDupeInsertCursor
    GetOlDataDupeNoDropCursor            GetOlDataMoveDragCursor
    GetOlDataMoveDropCursor            GetOlDataMoveInsertCursor
    GetOlDataMoveNoDropCursor GetOlDocCursor GetOlDocStackCursor
    GetOlDropCursor   GetOlDupeDocCursor  GetOlDupeDocDragCursor
    GetOlDupeDocDropCursor              GetOlDupeDocNoDropCursor
    GetOlDupeStackCursor                GetOlDupeStackDragCursor
    GetOlDupeStackDropCursor          GetOlDupeStackNoDropCursor
    GetOlDuplicateCursor                       GetOlFolderCursor
    GetOlFolderStackCursor  GetOlMoveCursor   GetOlMoveDocCursor
    GetOlMoveDocDragCursor                GetOlMoveDocDropCursor
    GetOlMoveDocNoDropCursor                GetOlMoveStackCursor
    GetOlMoveStackDragCursor            GetOlMoveStackDropCursor
    GetOlMoveStackNoDropCursor GetOlNoDropCursor  GetOlPanCursor
    GetOlQuestionCursor   GetOlStandardCursor  GetOlTargetCursor
    GetOlTextDupeDragCursor              GetOlTextDupeDropCursor
    GetOlTextDupeInsertCursor          GetOlTextDupeNoDropCursor
    GetOlTextMoveDragCursor              GetOlTextMoveDropCursor
    GetOlTextMoveInsertCursor          GetOlTextMoveNoDropCursor
    OlGetDataDupeDragCursor              OlGetDataDupeDropCursor
    OlGetDataDupeInsertCursor          OlGetDataDupeNoDropCursor
    OlGetDataMoveDragCursor              OlGetDataMoveDropCursor
    OlGetDataMoveInsertCursor          OlGetDataMoveNoDropCursor
    OlGetDocCursor      OlGetDocStackCursor      OlGetDropCursor
    OlGetDupeDocCursor OlGetDupeDocDragCursor OlGetDupeDocDropCursor
    OlGetDupeDocNoDropCursor                OlGetDupeStackCursor
    OlGetDupeStackDragCursor            OlGetDupeStackDropCursor
    OlGetDupeStackNoDropCursor                 OlGetFolderCursor
    OlGetFolderStackCursor                    OlGetMoveDocCursor
    OlGetMoveDocDragCursor                OlGetMoveDocDropCursor
    OlGetMoveDocNoDropCursor                OlGetMoveStackCursor
    OlGetMoveStackDragCursor            OlGetMoveStackDropCursor
    OlGetMoveStackNoDropCursor                 OlGetNoDropCursor
    OlGetTextDupeDragCursor              OlGetTextDupeDropCursor
    OlGetTextDupeInsertCursor          OlGetTextDupeNoDropCursor
    OlGetTextMoveDragCursor              OlGetTextMoveDropCursor
    OlGetTextMoveInsertCursor     OlGetTextMoveNoDropCursor
]

#_ENDIF

do i, [(x) :ulong,].dl
endfor >_#
;

constant XolOlCursor = true;

endsection;
