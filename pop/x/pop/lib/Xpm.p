/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpm.p
 > Purpose:         X PixMap tools (for V3.2g)
 > Author:          Julian Clinton, Feb 9 1996. (see revisions)
 > Documentation:   REF * Xpm
 > Related Files:
 */

compile_mode :pop11 +strict;

section;

exload_batch;

include sysdefs.ph;
include xpt_xpmtypes.ph;

lvars XPM_EXLIBS = [%

#_IF DEF UNIX
    '-lXpm',

#_ELSEIF DEF VMS
  #_IF DEF ALPHA
    'pop$libxpm:/share',        ;;; "pop$libxpm" is defined as a logical name
  #_ELSE
    'popexternlib:libxpm.olb/library',
  #_ENDIF

#_ELSE_ERROR
#_ENDIF

    ident XLINK_EXLIBS
%];

XptPopLoadProcedures ''
    [^^XPM_EXLIBS]

    ;;; Pixmap routines
    XpmReadFileToPixmap(disp,d,f,p,s,a)     :int,
    XpmWriteFileFromPixmap(disp,f,p,s,a)    :int,
    XpmCreatePixmapFromData(disp,d,data,p,s,a) :int,
    XpmCreateDataFromPixmap(disp,d,p,s,a)   :int,

    ;;; XImage routines
    XpmReadFileToImage(disp,f,i,s,a)        :int,
    XpmWriteFileFromImage(disp,f,i,s,a)     :int,
    XpmCreateImageFromData(disp,data,i,s,a) :int,
    XpmCreateDataFromImage(disp,data,i,s,a) :int,

    ;;; XPM buffer routines
    XpmCreateImageFromBuffer(disp,b,i,s,a)      :int,
    XpmCreatePixmapFromBuffer(disp,d,b,p,s,a)   :int,
    XpmCreateBufferFromImage(disp,b,i,s,a)      :int,
    XpmCreateBufferFromPixmap(disp,b,p,s,a)     :int,
    XpmReadFileToBuffer(f,b)        :int,
    XpmWriteFileFromBuffer(f,b)     :int,

    ;;; Dynamic allocation
    XpmFreeAttributes(a)    :int,
    XpmAttributesSize()     :int,
    XpmFreeExtensions(e,n)  :int,
    ;;; XpmFree(p)              :int,   ;;; only defined as a macro

;

endexload_batch;

constant Xpm = true;

endsection;
