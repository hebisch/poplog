/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_widgettypes.ph
 > Purpose:         Typespec of Core and Composite widgets
 > Author:          John Gibson, Nov  3 1991
 > Documentation:   REF *XPT_WIDGETTYPES
 */

#_TERMIN_IF DEF XPT_WIDGETTYPES_INCLUDED

section;

include xpt_generaltypes.ph; ;;; For XptWidget, XptWidgetClass, XptBoolean,
                             ;;;     XptPointer, XptPosition, XptDimension,
                             ;;;     XptCardinal, XptString, XptWidgetList
include xpt_xtypes.ph;       ;;; For XptPixel, XptPixmap, XptColormap, and
                             ;;;     XptXrmQuark, XptScreenPtr, XptWindow


;;; definition from X11/IntrinsicP.h
l_typespec XptTMRec {
    :XptTranslations,       /* PRIVATE                           */
    :exptr.:XptProcedure,   /* procedure bindings for actions    */
    :exptr,                 /* translation manager state pointer */
    :ulong                  /* last event time                   */
};


;;; Definition for Core widget fields
i_typespec XptCorePart {
    self                :XptWidget,       /* pointer to widget itself     */
    widget_class        :XptWidgetClass,  /* pointer to Widget's class    */
    parent              :XptWidget,       /* parent widget                */
                        :XptXrmQuark,     /* PRIVATE                      */
    being_destroyed     :XptBoolean,      /* marked for destroy?          */
    destroy_callbacks   :exptr,           /* called when widget destroyed */
                                          /* (should be :XptCallbackList) */
    constraints         :XptPointer,      /* constraint record            */
    x                   :XptPosition,     /* window x position            */
    y                   :XptPosition,     /* window y position            */
    width               :XptDimension,    /* window width                 */
    height              :XptDimension,    /* window height                */
    border_width        :XptDimension,    /* window border width          */
    managed             :XptBoolean,      /* widget geometry managed?     */
    sensitive           :XptBoolean,      /* widget user event sensitive? */
    ancestor_sensitive  :XptBoolean,      /* are all ancestors sensitive? */
                        :exptr,           /* PRIVATE                      */
                        :XptTMRec,        /* PRIVATE                      */
    accelerators        :XptTranslations, /* accelerator translations     */
    border_pixel        :XptPixel,        /* window border pixel          */
    border_pixmap       :XptPixmap,       /* window border pixmap or NULL */
    popup_list          :exptr,           /* list of popups               */
                                          /* (should be :XptWidgetList)   */
    num_popups          :XptCardinal,     /* how many popups              */
    name                :XptString,       /* widget resource name         */
    screen              :XptScreenPtr,    /* window's screen              */
    colormap            :XptColormap,     /* colormap                     */
    window              :XptWindow,       /* window ID                    */
    depth               :XptCardinal,     /* number of planes in window   */
    background_pixel    :XptPixel,        /* window background pixel      */
    background_pixmap   :XptPixmap,       /* background pixmap or NULL    */
    visible             :XptBoolean,      /* mapped and not occluded?     */
    mapped_when_managed :XptBoolean,      /* map window if it's managed?  */
};


;;; Typespec for core widgets
i_typespec XptCoreWidget {
    core    :XptCorePart
};


;;; Typespec for composite widget fields
i_typespec XptCompositePart {
    children        :exptr,         /* array of ALL widget children      */
                                    /* (should be :XptWidgetList)        */
    num_children    :XptCardinal,   /* total number of widget children   */
    num_slots       :XptCardinal,   /* number of slots in children array */
    insert_position :XptProcedure   /* compute position of new child     */
};


;;; Typespec for composite widgets
i_typespec XptCompositeWidget {
    core        :XptCorePart,
    composite   :XptCompositePart
};


iconstant XPT_WIDGETTYPES_INCLUDED = true;

endsection;
