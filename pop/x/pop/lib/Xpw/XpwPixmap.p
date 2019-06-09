/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpw/XpwPixmap.p
 > Purpose:         Methods in support of XpwPixmap widgetclass
 > Author:          Jon Meyer, Jan 1990 (see revisions)
 > Documentation:   REF * XpwPixmap, HELP * XpwGraphic
 > Related Files:   LIB * XpwPixmap, * XpwPixmap.ph
 */
compile_mode:pop11 +strict;

include XpwPixmap.ph;

section;
exload_batch;

uses
    XpwCore,
    xpwPixmapWidget,
;

XptLoadProcedures XpwPixmap
lvars
        XGetPixel
        XPutPixel
        XAddPixel
        XDestroyImage
    ;


;;; Given a list, vector or shortvector, and an integer -n-: returns a
;;; shortvector and the number of groups of -n- integers in the shortvector
;;; attempts to free-list these vectors

lconstant procedure vectab = newproperty([], 16, [], "tmpval");

define lconstant shortpointvec(vec, n) -> vec -> nitems;
    lvars vec n nitems;
    lvars len = length(vec);
    unless isinteger(len / n ->> nitems) then
        mishap(vec, n, 2, 'Length of points list is not a correct multiple');
    endunless;
    unless isshortvec(vec) then
        explode(vec);
        if (vectab(len) ->> vec) == [] then
            consshortvec(len) -> vec;
        else
            sys_grbg_destpair(vec) -> vectab(len) -> vec;
            fill(vec) -> vec;
        endif;
    endunless;
enddefine;

define lconstant freepointvec(vec);
    lvars vec len = datalength(vec);
    conspair(vec, vectab(len)) -> vectab(len);
enddefine;

define XpwDrawPoint(w,x,y);
    lvars w,x,y;
    XpwCallMethod(XptCheckWidget(w),XpwMDrawPoint,
            fi_check(x,false,false),fi_check(y,false,false),4,false);
enddefine;

define XpwDrawPoints(w,pts, mode);
    lvars w,pts, mode, points, npoints;
    shortpointvec(pts, 2) -> points -> npoints;
    XpwCallMethod(XptCheckWidget(w),XpwMDrawPoints,
                points, npoints,
                fi_check(mode,0,1), 5,false);
    if points /== pts then freepointvec(points) endif;
enddefine;

define XpwDrawLine(w,x1,y1,x2,y2);
    lvars w,x1,y1,x2,y2;
    XpwCallMethod(XptCheckWidget(w),XpwMDrawLine,
            fi_check(x1,false,false),fi_check(y1,false,false),
            fi_check(x2,false,false),fi_check(y2,false,false),6,false);
enddefine;

define XpwDrawLines(w, points, mode);
    lvars w points mode;
    lvars npoints newpoints;
    shortpointvec(points, 2) -> newpoints -> npoints;
    XpwCallMethod(XptCheckWidget(w), XpwMDrawLines, newpoints, npoints,
        fi_check(mode,0,1), 5,false);
    if newpoints /== points then freepointvec(newpoints) endif;
enddefine;

define XpwDrawRectangle(wid,x1,y1,w,h);
    lvars wid,x1,y1,w,h;
    XpwCallMethod(XptCheckWidget(wid),XpwMDrawRectangle,
            fi_check(x1,false,false),fi_check(y1,false,false),
            fi_check(w,0,false),fi_check(h,0,false),6,false);
enddefine;

define XpwDrawRoundedRectangle(wid,x1,y1,w,h,s1,s2);
    lvars wid,x1,y1,w,h,s1,s2;
    XpwCallMethod(XptCheckWidget(wid),XpwMDrawRoundedRectangle,
            fi_check(x1,false,false),fi_check(y1,false,false),
            fi_check(w,0,false),fi_check(h,0,false),
            fi_check(s1,0,false),fi_check(s2,0,false),
            8,false);
enddefine;

define XpwDrawRectangles(w,recs);
    lvars w,recs;
    lvars nrectangles, rectangles;
    shortpointvec(recs,4) -> rectangles -> nrectangles;
    XpwCallMethod(XptCheckWidget(w),XpwMDrawRectangles,
            rectangles, nrectangles, 4,false);
    if rectangles /== recs then freepointvec(rectangles) endif;
enddefine;

define XpwDrawArc(w,x1,y1,dx,dy,a1,a2);
    lvars w,x1,y1,dx,dy,a1,a2;
    XpwCallMethod(XptCheckWidget(w),XpwMDrawArc,
                    fi_check(x1,false,false),fi_check(y1,false,false),
                    fi_check(dx,0,false),fi_check(dy,0,false),
                    fi_check(a1,false,false),fi_check(a2,false,false),
                    8,false);
enddefine;

define XpwDrawArcs(w, arcs);
    lvars w, arcs, newarcs, narcs;
    shortpointvec(arcs,6) -> newarcs -> narcs;
    XpwCallMethod(XptCheckWidget(w),XpwMDrawArcs, newarcs, narcs, 4, false);
    if arcs /== newarcs then freepointvec(newarcs); endif;
enddefine;

define XpwDrawSegments(w, segments);
    lvars w segments;
    lvars nsegments newsegments;
    shortpointvec(segments, 4) -> newsegments -> nsegments;
    XpwCallMethod(XptCheckWidget(w),
            XpwMDrawSegments, newsegments, nsegments, 4, false);
    if newsegments /== segments then freepointvec(newsegments) endif;
enddefine;

define lconstant DrawString(w, x, y, string, method);
    lvars s = sys_encode_string_fixed(string);
    XpwCallMethod(XptCheckWidget(w), method,
                    fi_check(x, false, false), fi_check(y, false, false),
                    s, datalength(s), 6, false);
    if s /== string then sys_grbg_fixed(s) endif
enddefine;

define XpwDrawString(/*w, x, y, string*/) with_nargs 4;
    DrawString(XpwMDrawString)
enddefine;

define XpwDrawImageString(/*w, x, y, string*/) with_nargs 4;
    DrawString(XpwMDrawImageString)
enddefine;

define XpwFillArc(wid, x1, y1, w, h, a1, a2);
    lvars wid x1 y1 w h a1 a2;
    XpwCallMethod(XptCheckWidget(wid), XpwMFillArc,
                    fi_check(x1,false,false), fi_check(y1,false,false),
                    fi_check(w,0,false), fi_check(h,0,false),
                    fi_check(a1,false,false), fi_check(a2,false,false),
                    8, false);
enddefine;

define XpwFillArcs(w, arcs);
    lvars w, arcs, newarcs, narcs;
    shortpointvec(arcs,6) -> newarcs -> narcs;
    XpwCallMethod(XptCheckWidget(w),XpwMFillArcs, newarcs, narcs, 4, false);
    if arcs /== newarcs then freepointvec(newarcs); endif;
enddefine;

define XpwFillPolygon(w, pts, shape, mode);
    lvars w, pts, shape, mode, points, npoints;
    shortpointvec(pts, 2) -> points -> npoints;
    XpwCallMethod(XptCheckWidget(w),XpwMFillPolygon, points, npoints,
            fi_check(shape,false,false),fi_check(mode,0,1), 6,false);
    if points /== pts then freepointvec(points) endif;
enddefine;

define XpwFillRectangle(wid, x1, y1, w, h);
    lvars wid x1 y1 w h;
    XpwCallMethod(XptCheckWidget(wid), XpwMFillRectangle,
            fi_check(x1,false,false), fi_check(y1,false,false),
            fi_check(w,0,false), fi_check(h,0,false), 6, false);
enddefine;

define XpwFillRoundedRectangle(wid,x1,y1,w,h,s1,s2);
    lvars wid,x1,y1,w,h, s1,s2;
    XpwCallMethod(XptCheckWidget(wid),XpwMFillRoundedRectangle,
            fi_check(x1,false,false),fi_check(y1,false,false),
            fi_check(w,0,false),fi_check(h,0,false),
            fi_check(s1,0,false),fi_check(s2,0,false), 8,false);
enddefine;

define XpwFillRectangles(w,recs);
    lvars w,recs, nrectangles, rectangles;
    shortpointvec(recs,4) -> rectangles -> nrectangles;
    XpwCallMethod(XptCheckWidget(w),XpwMFillRectangles,
            rectangles, nrectangles, 4,false);
    if rectangles /== recs then freepointvec(rectangles) endif;
enddefine;

define XpwFreeImage(image);
    lvars image;
    XptTypeCheck(image, "XImagePtr")->;
    if is_valid_external_ptr(image) then
        exacc (1) raw_XDestroyImage(image);
        0 -> exacc [fast] ^uint image;  ;;; clear my pointer
    endif;
    if XptDataProps(image) then
        ;;; Image is holding onto its data - just
        ;;; Free XImage structure and release hold on data
        false -> XptDataProps(image);
    endif;
enddefine;

define XpwDrawImage(w,dx,dy,x,y,image);
    lvars w,dx,dy,x,y,image, depth;
    unless image.isvectorclass then arrayvector(image)->image; endunless;
    field_spec_info(class_field_spec(datakey(image))) -> -> depth;
    XpwCallMethod(XptCheckWidget(w),XpwMDrawImage,
                    fi_check(dx,false,false),fi_check(dy,false,false),
                    fi_check(x,false,false),fi_check(y,false,false),
                    image,depth, 8, false);
enddefine;

define XpwPutImage(w,image,x,y,ex,ey,dx,dy);
    lvars w,x,y,image,ex,ey,dx,dy;
    XpwCallMethod(XptCheckWidget(w), XpwMPutImage, image,
        fi_check(x,false,false), fi_check(y,false,false),
        fi_check(ex,false,false), fi_check(ey,false,false),
        fi_check(dx,false,false), fi_check(dy,false,false), 9,false);
enddefine;

define XpwGetImage(w, x,y,dx,dy,msk,format) -> image;
    lvars w,x,y,dx,dy,msk,format, image;
    unless msk then 0 -> msk endunless;
    unless format then 0 -> format endunless;
    XpwCallMethod(XptCheckWidget(w), XpwMGetImage,
        fi_check(x,false,false), fi_check(y,false,false),
        fi_check(dx,false,false), fi_check(dy,false,false),
        fi_check(msk,false,false), fi_check(format,0,2), 8, "exptr") -> image;
    if image then
        "XImagePtr" -> external_ptr_props(image);
        XpwFreeImage -> sys_destroy_action(image);
    endif;
enddefine;

define XpwCreateImage(w,width, height, depth, data) -> image;
    lvars w, width, height, depth, data, image;
    unless data.isvectorclass then arrayvector(data)->data; endunless;
    unless is_fixed(data) then copy_fixed(data) -> data; endunless;
    unless depth then
        field_spec_info(class_field_spec(datakey(data))) -> -> depth;
    endunless;
    XpwCallMethod(XptCheckWidget(w),XpwMCreateImage,
                    width, height, depth, data, 6, "exptr") -> image;
    if image then
        ;;; keep hold of data so it isn't garbaged
        conspair("XImagePtr", data) ->  external_ptr_props(image);
        XpwFreeImage -> sys_destroy_action(image);
    endif;
enddefine;

define XpwImageAddPixelValue(image, value);
    lvars image, value;
    exacc (2) raw_XAddPixel(XptLiveTypeCheck(image, "XImagePtr"),
            XptCheckInt(value));
enddefine;

define XpwImagePixelValue(image, x, y);
    lvars image, x, y;
    exacc (3):ulong raw_XGetPixel(XptLiveTypeCheck(image, "XImagePtr"),
            XptCheckUnsignedInt(x),
            XptCheckUnsignedInt(y));
enddefine;

define updaterof XpwImagePixelValue(pixel, image, x, y);
    lvars image, x, y, pixel;
    exacc (4) raw_XPutPixel(XptLiveTypeCheck(image, "XImagePtr"),
            XptCheckUnsignedInt(x),
            XptCheckUnsignedInt(y),
            XptCheckUnsignedInt(pixel));
enddefine;

define XpwPixelValue(image, x, y);
    lvars image, x, y;
    if XptDataType(image) == "XImagePtr" then
        XpwImagePixelValue(image, x, y);
    else
        ;;; given a widget - get a 1 pixel image
        XpwGetImage(image, x, y, 1, 1, 0,0) -> image;
        XpwImagePixelValue(image, 0,0);
        XpwFreeImage(image);
    endif;
enddefine;

define updaterof XpwPixelValue(pixel, image, x, y);
    lvars image, x, y, pixel;
    if image.XptDataType == "XImagePtr" then
        pixel -> XpwImagePixelValue(image, x, y);
    else
        ;;; given a widget - use XpwDrawPoint to set pixel value
        XptCheckWidget(image)->;
        procedure;
            dlocal 2 %XptVal[fast] image(XtN foreground, XtN function)%
                                            = (pixel, GXcopy);
            XpwDrawPoint(image, x, y);
        endprocedure();
    endif;
enddefine;

define XpwCopyFrom(w, src, x,y,dx,dy,ex,ey);
    lvars w, src, x,y,dx,dy,ex,ey;
    XpwCallMethod(XptCheckWidget(w), XpwMCopyFrom, XptCheckWidget(src),
        fi_check(x,false,false), fi_check(y,false,false),
        fi_check(dx,false,false), fi_check(dy,false,false),
        fi_check(ex,false,false), fi_check(ey,false,false), 9,false);
enddefine;

define XpwCopyTo(w, dst, x,y,dx,dy,ex,ey);
    lvars w, dst, x,y,dx,dy,ex,ey;
    XpwCallMethod(XptCheckWidget(w), XpwMCopyTo, XptCheckWidget(dst),
        fi_check(x,false,false), fi_check(y,false,false),
        fi_check(dx,false,false), fi_check(dy,false,false),
        fi_check(ex,false,false), fi_check(ey,false,false), 9,false);
enddefine;

define XpwClearWindow(w);
    lvars w;
    XpwCallMethod(XptCheckWidget(w), XpwMClearWindow, 2, false);
enddefine;

define XpwClearArea(w,x,y,dx,dy);
    lvars w,x,y,dx,dy;
    XpwCallMethod(XptCheckWidget(w), XpwMClearArea,
                fi_check(x, false, false), fi_check(y, false, false),
                fi_check(dx, 0, false), fi_check(dy, 0, false),  6, false);
enddefine;


constant XpwPixmap = true;

endexload_batch;
endsection;     /* top level */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1997
        Changed XpwDraw(Image)String to call sys_encode_string_fixed on
        string arg.
--- John Gibson, Apr  5 1993
        Uses Xpw/xpwPixmapWidget
--- John Gibson, Sep 11 1992
        Changed to use XptVal
--- Adrian Howard, Nov  4 1991 : Now compiles under +strict compile mode
--- Jonathan Meyer, Sep 25 1991
        Fixed XpwFreeImage so that it can be called multiple times.
        Fixed typespec for updaterof XpwImagePixelValue
--- Jonathan Meyer, Jul 30 1991
        Now uses new XpwCallMethod interface, and XptCheckWidget
        instead of xpw_check_live_widget. Added XpwCreateImage,
        XpwFreeImage, XpwImagePixelValue, XpwPixelValue
--- Jonathan Meyer, Jul 29 1991
        Updated GC subpart resource specifications
--- Jonathan Meyer, Jul  2 1991
        Added RoundedRectangle
--- Jonathan Meyer, Mar 13 1991
        Changed reps of resources in XptSpecifyResourceInfo so that
        they don't get specified as typed args (the X toolkit cannot
        cope with typed subpart resources)
--- Jonathan Meyer, Mar 13 1991
        Added XptSpecifyResourceInfo/PopValueTypes to define subpart
        resources.
--- John Williams, Jan 24 1991
        Replaced -class_spec- with -class_field_spec-
--- Jonathan Meyer, Jan 17 1991
        Fixed XpwDrawImage to use field_spec_info to determine size of
        elements in a vector.
--- Jonathan Meyer, Oct 23 1990
        Added line to load Core widget from Poplog widget set.
        Removed reference to X11/StringDefs.
--- Roger Evans, Oct 22 1990 changed to XpwPixmap and reinstalled
--- James Goodlet, Sep 27 1990 - added definition of XpwPixmapMethods
        for -uses-.
--- Jonathan Meyer, Aug 31 1990
    Updated to work with new xpop version: removed xt_shadow, tested for
    XpwCore methods being loaded, removed xt_ from xt_check_live_widget.
--- Jonathan Meyer, Aug  8 1990 - fixed simple bug in XpwFillArcs
--- Andreas Schoter, July 16 1990
    Renamed to XpwPixmapMethods.p and changed all variable names from Pop* to
    Xpw*
--- James Goodlet, Jul  9 1990 - simple bug in -XpwFillPolygon- fixed.
--- James Goodlet, Jun  6 1990 - moved out from LIB * PopPixmap - see that
        file for more revision notes.
 */
