/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpw/popc_declare.ph
 > Purpose:         Identifier declarations for POPC in this directory
 > Author:          John Gibson, Mar 13 1993
 */

uses-now popxlib;

library_declare_section '$usepop/pop/x/pop/lib/Xpw/'

section;

weak global constant procedure (
        XpwCallMethod,
        XpwClearWindow,
        XpwDrawArc,
        XpwDrawLine,
        XpwDrawPoint,
        XpwDrawRectangle,
        XpwDrawRoundedRectangle,
        XpwDrawString,
        XpwSetColor,
    );

weak global constant
        XpwCore,
        XpwGraphic,
        XpwPixmap,
        XpwScrollText,
        xpwCompositeWidget,
        xpwCoreWidget,
        xpwGraphicWidget,
        xpwPixmapWidget,
        xpwScrollTextWidget,
        xpwTransparentWidget,
    ;

endsection;

end_library_declare_section;
