/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptGetFontProperty.p
 > Purpose:         Fetching standard font properties
 > Author:          Jonathan Meyer, Aug  1 1991 (see revisions)
 > Documentation:   REF *XT_LIBS
 > Related Files:
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include x_atoms.ph;
uses xpt_atomcache;

XptLoadProcedures 'xptfontproperty'
lvars
    XGetFontProperty(font_struct,atom,value) :int,
;


define XptGetFontProperty(font, name);
    lvars font, name, tmp, deref = false, dpy = XptDefaultDisplay;
    lconstant value = EXPTRINITSTR(:ulong);

    if isexternal_ptr_class(name) then
        ;;; optional display
        ((), font, name) -> (font, name, dpy)
    endif;
    if name.isboolean then
        ((), font, name) -> (font, name, deref)
    endif;

    if name.isstring or name.isword then
        /* Turn name or string into word */
        XptInternAtom(dpy, name, true) -> tmp;
        returnunless(tmp)(false);
        tmp -> name;
    else
        XptCheckUnsignedInt(name)->;
        /* Should we check for standard property atoms?
        unless name == XA_FONT or
                (name fi_>= XA_MIN_SPACE and name fi_<= XA_CAP_HEIGHT) then
            mishap(name,1, 'FONT PROPERTY ATOM OUT OF RANGE');
        endunless;
        */
    endif;

    unless isinteger(font) or isexternal_ptr_class(font) then
        mishap(font,1,'FONTSTRUCT NEEDED');
    endunless;

    if (exacc raw_XGetFontProperty(font, name, value)) /== 0 then
        exacc [fast] :ulong value -> tmp;
        if deref then
            /* Return value is an atom - get its name */
            XptGetAtomName(dpy, tmp);
        else
            tmp
        endif;
    else
        false
    endif;
enddefine;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec  5 1995
        Allowed it to take an optional display as last argument.
--- John Gibson, Mar 22 1993
        Got rid of run-time loading of xpt_atomcache.
--- Adrian Howard, Jul 17 1992
        Now returns -false- if the font property atom name is unknown
--- Adrian Howard, Aug 12 1991 : xatoms.ph --> x_atoms.ph
 */
