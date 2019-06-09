/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ved/src/xveddeclare.p
 > Purpose:         Declares new variables
 > Author:          Jonathan Meyer, Apr  1 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-xved =>
    vvedclipboard, vvedlastclipboard,
    vedserverxvedkeys,
    vvedmousedata,
    xved_value,
    xved_dispatch_event,
    xvedeventhandled,
    xvedignorepixelmotion,
    xvedinterruptchar,
;

include sysdefs.ph;

uses
    fast_xt_widgetclass,
    fast_xt_composite,
    fast_xt_util,
    xt_trans,
    xt_resource,
    xt_widget,
    xt_widgetinfo,
    xt_grab,
    xt_event,
    xt_action,
    xt_display,
    xt_callback,
    xt_composite,
;


/************************************************************************
 * Xved Variable Declaration
 ************************************************************************/


;;; New global vars
vars
    vvedmousedata,
    vvedlastclipboard,
    active vvedclipboard,
    procedure (
        vedserverxvedkeys = identfn,
    ),
;


;;; (-Nonexported-) variables vars
vars
    xveddisplay = false,
    xvedappcontext = false,
    xveddummyshell = false,         ;;; misc non-realized shell
    xvedselectionon = false,        ;;; whether a buffer has a highlight on
    xvedselectiononstatus = false,  ;;; whether highlight on status line
    xvedisincreate = false,         ;;; true during  window creation
    xvedisasynchronous = false,     ;;; true if ved is being run async.
    xvedhasinputfocus = false,      ;;; a window if XVed currently has focus
    xvedblockinput = false,         ;;; if true, raise procedures bell/exit
    xvednextkeyseqvec = false,      ;;; created in xvedresources.p
    xvednexteventvec = false,       ;;; and used in xvedwindows.p
    xvedstartmetaseq = '\^[\^XM',
    xvedinterruptchar = `\^C`,
    xvedeventhandled,
    xvedignorepixelmotion = false,

    xvedwarpmouse = false,
    xvedsetfocus = true,
    xvedpopfocusonly = false,
    xvedkeypadon = true,
    xvedautoplace = true,
    xvedautocut = true,
    xvedselectionautocut = false,
    xveduseownappcontext = true,
    xvedvscrollstep = 1,
    xvedhscrollstep = 1,
    xvedshowfilename = true,
    xvedvanilla = true,
    xveddialogslist = [], ;;; LIST OF ALL DIALOG BOXES CREATED BY XVED (*NOT* THE PUI)
    xvedidents = [],

    procedure (
        xved_after_editor_entry = identfn,
        ;;; called each time before ved starts up

        xved_before_editor_exit  = identfn,
        ;;; called when ved leaves, to remove any cached references
        ;;; to things, so that they can become garbaged.
        ;;; also resets any state variables.

        xved_rawin_read_trap = syshibernate,
        ;;; xved-definable, called from within device

        xved_dispatch_event,
        ;;; user-definable, default in xvedhandlers.p

    )
;

;;; Things that may not be loaded
weak constant
        $-popxlink_motif,
        $-popxlink_openlook,
        $-xvedmenubar,
        $-xvedscrollbar,
;

weak constant procedure (

        xved_check_scrollbar,
        ;;; defined in xvedscrollbar.p

        xved_check_buffer_menu,
        ;;; defined in xvedmenubar.p
    );


#_IF DEF IRIS or DEF HPUX
false -> xveduseownappcontext;  ;;; see bugreport isl-fr.4477
#_ENDIF

constant procedure (
    xvedeventtable  = newproperty([], 16, false, "perm"),
    xvedkeyseqtable = newproperty([], 16, false, "perm"),
);

constant

    ;;; input device
    xvedrawdevindata = writeable initv(3),

    xvedselectioncoords = writeable consvector(1,1,1,1,4),

    ;;; forward declarations
    procedure (
        xved_value,
        xved_is_next_event,
        xved_process_event,
        xved_select_window,
        xved_default_window_value,
        xved_make_create_args,

        ;;; set widthInc and heightInc to make window size measure fontsize
        xved_size_hints_set,

        vedselection_adjust,
        xved_button_release,
    ),
;

;;; ved window record - used in xvedwindows.p
defclass xvedwin [external_ptr] {
    xvedwin_datatype,               ;;; XptDataType field = "Widget"
>-> xvedwin_widget_ptr: exptr,      ;;; pointer passed to Xt routines
    xvedwin_widget,                 ;;; XpwScrollText widget descriptor
    xvedwin_parent,                 ;;; wrapper around ved window
    xvedwin_shell,                  ;;; top level window containing ved window
    xvedwin_iconwindow,             ;;; icon of shell window
    xvedwin_configure,              ;;; procedure to configure window
    xvedwin_menubar,                ;;; data for menubars
    xvedwin_scrollbar,              ;;; data for scrollbars
    xvedwin_hscrollbar,             ;;; data for horiz scrollbars
    xvedwin_props,                  ;;; extra props - definable
    xvedwin_globals,                ;;; global vars saved and set
    xvedwin_eventvec,               ;;; table for events
    xvedwin_keyseqvec,              ;;; table for keyseq translations
    xvedwin_isnew,                  ;;; true first time window is created
    xvedwin_positionstack,          ;;; saved positions (used by selections).
    xvedwin_screendatavec           ;;; screen output data vec
};

procedure(win);
    lvars win;
    dlocal pop_pr_quotes = false;
    if is_valid_external_ptr(win) then
        sys_syspr('<Ved Window>')
    else
        sys_syspr('<(NULL)Ved Window>')
    endif;
endprocedure -> class_print(xvedwin_key);

XptDescriptor_apply -> class_apply(xvedwin_key);



endsection;


/* --- Revision History ---------------------------------------------------
--- Integral Solutions Ltd, Aug 31 1995 (Julian Clinton)
        Made HP not use own appcontext.
--- John Gibson, Feb 11 1994
        Added xvedwin_hscrollbar
--- John Gibson, Jan 21 1994
        Removed xvedwin_isch*anged field -- not needed
--- John Gibson, Dec 14 1993
        Added xvedpopfocusonly
--- Julian Clinton, Dec 10 1993
        Changed initial value of xvedwarpmouse to false.
--- Simon Nichols, Oct 15 1992
        Assigned <false> to -xveduseownappcontext- on SG Iris systems (see
        bugreport isl-fr.4477). This required sysdefs.ph to be included.
--- Adrian Howard, Aug 20 1992
        o pop_pr_quotes -false- in class print of XVed windows.
--- John Gibson, Jul 24 1992
        Added xvedvanilla
--- John Gibson, Mar 28 1992
        Moved call of raw_XpwSetWMProtocolActions to xvedsetup.
        Removed xved_a*fter_x_setup.
--- John Gibson, Dec 28 1991
        Added -xvedwin_screen*charmode- field
--- Jonathan Meyer, Nov 19 1991
        Made class_print use is_valid_external_ptr rather than `exacc ^int
--- John Gibson, Nov 16 1991
        Moved in exload for raw_Xpw things and perm procedures from
        xved_xpw.ph.
        Added new fields -xvedwin_screenl*inecol- and -xvedwin_screeno*utstate-
        to xvedwin record.
--- John Gibson, Oct 28 1991
        Added xvedignorepixelmotion
--- Jonathan Meyer, Sep 11 1991 Added xved_check_buffer_menu
--- John Gibson, Aug 10 1991
        Made working vectors writeable
--- Jonathan Meyer, Aug  2 1991 Added XV_NO_ARGS
--- Jonathan Meyer, Jul 31 1991 Added exload of xvedwidgets
--- Jonathan Meyer, Jul 27 1991
        Renamed xved*after_editor_exit xved_before_editor_exit
--- Jonathan Meyer, Jul 27 1991
        Added xveduseownappcontext
--- Adrian Howard, Jul 25 1991 : Removed -xveduserlevel-, added
        -xvedpw*mbuttonbindings-
--- John Gibson, Jul 12 1991
        Put in xvedinterruptchar
--- Jonathan Meyer, Jul  5 1991
        made xved_check_scrollbar a vars
--- Jonathan Meyer, Jul  5 1991
        Removed xved*hasinputwaiting
--- Jonathan Meyer, Jul  1 1991
        Made XptDescriptor_apply the class apply of the xvedwin key
--- Jonathan Meyer, Jun  4 1991
        Removed xvedwin_ismapped
--- Jonathan Meyer, Jun  3 1991
        Added xvedautoplace
--- Jonathan Meyer, Jun  3 1991
        Added xvednexteventvec and xvednextkeyseqvec
--- Jonathan Meyer, Jun  2 1991 Added xvedkeypadon
--- Jonathan Meyer, Apr  7 1991 Moved in defclass statements.
 */
