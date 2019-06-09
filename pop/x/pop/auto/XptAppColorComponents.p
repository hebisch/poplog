/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptAppColorComponents.p
 > Purpose:         Apply procedure to RGB components of pixels
 > Author:          Johhn Gibson, Jun 17 1999
 > Documentation:   REF * XT_LIBS
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_xtypes.ph;
include xpt_xcolor.ph;

XptLoadProcedures XptAppColorComponents
lvars
    XQueryColors(dpy,colormap,def_in_out,ncolors)
;

define XptAppColorComponents(n, w, app_p);
    lvars i, pixel, procedure app_p, XColor, XColors;
    l_typespec XColors :XColor[];
    returnif(n == 0);
    initexptr_mem(SIZEOFTYPE(:XColor) * n) -> XColors;
    for i from n by -1 to 1 do
        () -> pixel;
        if isstring(pixel) then XptVal w(%pixel% :XptPixel) -> pixel endif;
        pixel -> exacc (exacc[@,nc] XColors[i]).pixel
    endfor;
    exacc raw_XQueryColors( XtDisplay(w),
                            XptVal[fast] w(XtN colormap:XptColormap),
                            XColors, n);
    for i to n do
        exacc[@,nc] XColors[i] -> XColor;
        app_p(exacc XColor.red, exacc XColor.green, exacc XColor.blue)
    endfor;
    sys_grbg_fixed(XColors)
enddefine;


endexload_batch;
endsection;
