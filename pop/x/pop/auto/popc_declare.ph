/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/popc_declare.ph
 > Purpose:         Identifier declarations for POPC in this directory
 > Author:          John Gibson, Mar 13 1993
 */

uses-now popxlib;

library_declare_section '$usepop/pop/x/pop/auto/'

section;

weak global constant macro (
        XtN,
    );

weak global constant procedure (
        XpmImportString,
        XptAncestors,
        XptArgList,
        XptBusyCursor,
        XptBusyCursorFeedback,
        XptCheckArgList,
        XptCheckDisplayPtr,
        XptCheckInt,
        XptCheckProcedure,
        XptCheckString,
        XptCheckUnsignedInt,
        XptCheckWidget,
        XptCheckWidgetClass,
        XptChildren,
        XptCoerceSaveString,
        XptCursorPlaneOn,
        XptDoesBackingStore,
        XptGarbageCursor,
        XptGeometrySpec,
        XptImportAny,
        XptImportCacheRef,
        XptImportKeySymTable,
        XptImportScreenPtr,
        XptImportWindow,
        XptImportXEventPtr,
        XptImportXrmDatabase,
        XptIsLiveType,
        XptIsType,
        XptIsValidCallback,
        XptLiveTypeCheck,
        XptNewCursorPlaneCursor,
        XptParseGeometry,
        XptPopImportProcedure,
        XptPopValue,
        XptResourceInfo,
        XptShellOfObject,
        XptSpecifyResourceInfo,
        XptSyncDisplay,
        XptTypeCheck,
        XptWMShellCoords,
        XptWidgetCoords,
        XptWidgetTree,
        XtDisplay,
        XtNLookup,
        fast_XptWidgetOfObject,
        fast_XtDisplayOfObject,
        fast_XtFree,
        fast_XtIsSubclass,
        fast_XtIsWidget,
        $-Xpt$-ConsAccess,
        $-Xpt$-ConstraintResources,
        $-Xpt$-Get_class_resources,
        $-Xpt$-Get_resource_struct,
        $-Xpt$-PopValueTypes,
        $-Xpt$-WidgetResources,
);

    /*  Next declaration necessary only for access/conversion procedures,
        and other procedures on which top-level closures are created.
    */
declare_updater
        XpmImportString,
        XptImportAny,
        XptImportCacheRef,
        XptImportKeySymTable,
        XptImportScreenPtr,
        XptImportWindow,
        XptImportXEventPtr,
        XptImportXrmDatabase,
;

weak global constant
        xtApplicationShellWidget,
        xtCoreWidget,
        $-Xpt$-RES_NOT_SET,
;

weak global vars procedure (
        XptBusyCursorChangeTrap,
);

endsection;

end_library_declare_section;

#_INCLUDE '../lib/popc_declare.ph'
