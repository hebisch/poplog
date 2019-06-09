/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/ved/src/popc_declare.ph
 > Purpose:         Identifier declarations for POPC in this directory
 > Author:          John Gibson, May 30 1993
 */
compile_mode :pop11 +strict;

uses-now popxved;

section;

weak constant procedure (
        vedselection_adjust,
        vedselection_compile,
        vedselection_copy,
        vedselection_cut,
        vedselection_help,
        vedselection_paste,
        vedselection_select,
        vedselection_set_primary,
        xved_create_icon_window,
        xved_get_icon_filename,
        xved_server_vendor_string,
        xved_value,
        xvedsetup,
    );

weak constant
        xveddialogs,
        xvedmenubar,
        xvedscrollbar,
        xvedtitle,
        xvedversion,
    ;

weak vars active (
        vvedclipboard,
    );

weak vars procedure (
        vedserverxvedkeys,
        xved_dispatch_event,
    );

weak vars
        vvedlastclipboard,
        vvedmousedata,
        xved,
        xvedeventhandled,
        xvedignorepixelmotion,
;

endsection;

section $-xved;

weak constant active (
        xvedclicktime,
    );

weak constant procedure (
        consxvedwin,
        isxvedwin,
        xved_active_change_callback,
        xved_add_button_handler,
        xved_arg_list,
        xved_button_callback,
        xved_button_release,
        xved_check_buffer_menu,
        xved_check_scrollbar,
        xved_default_window_value,
        xved_event_handler,
        xved_focus_change_callback,
        xved_get_app_resources,
        xved_get_icon_pixmap,
        xved_get_subpart,
        xved_icon_name,
        xved_icon_window_pixmap,
        xved_init_screendatavec,
        xved_init_wm_actions,
        xved_is_next_event,
        xved_is_raised_window,
        xved_key_callback,
        xved_keysym_seq,
        xved_make_create_args,
        xved_make_window_position,
        xved_max_linewidth,
        xved_popup_menu,
        xved_process_event,
        xved_raise_ascii,
        xved_raise_event,
        xved_raise_window_event,
        xved_scroll_to_columnoffset,
        xved_scroll_to_lineoffset,
        xved_select_window,
        xved_set_args,
        xved_set_color_args,
        xved_set_varwidthmode,
        xved_size_hints_set,
        xved_va_arg_list,
        xved_window_has_mouse,
        xved_window_label,
        xved_x_query_pointer,
        xved_x_sync,
        xved_x_warp_pointer,
        xvedeventtable,
        xvedkeyseqtable,
        xvedwin_configure,
        xvedwin_eventvec,
        xvedwin_hscrollbar,
        xvedwin_ischanged,
        xvedwin_isnew,
        xvedwin_keyseqvec,
        xvedwin_menubar,
        xvedwin_parent,
        xvedwin_screendatavec,
        xvedwin_scrollbar,
        xvedwin_shell,
    );

weak constant
        xvedrawdevindata,
        xvedselectioncoords,
    ;

weak vars procedure (
        xved_rawin_read_trap,
        xved_after_editor_entry,
        xved_before_editor_exit,
    );

weak vars
        xvedappcontext,
        xvedautocut,
        xvedautoplace,
        xvedblockinput,
        xvedcurrinputwin,
        xveddialogbackground,
        xveddialogforeground,
        xveddialogslist,
        xveddisplay,
        xveddummyshell,
        xvedhasinputfocus,
        xvedhscrollstep,
        xvedidents,
        xvedinterruptchar,
        xvedisasynchronous,
        xvedisincreate,
        xvedkeypadon,
        xvedmaxwindows,
        xvednexteventvec,
        xvednextkeyseqvec,
        xvedpopfocusonly,
        xvedscreenhighlight,
        xvedselectionautocut,
        xvedselectionclasses,
        xvedselectionon,
        xvedselectiononstatus,
        xvedsetfocus,
        xvedshowfilename,
        xvedstartmetaseq,
        xveduseownappcontext,
        xvedvanilla,
        xvedvscrollstep,
        xvedwarpmouse,
;

declare_incremental property (
        xvedeventtable = newproperty([], 16, false, "perm"),
    );

declare_incremental list (
        xvedidents,
        xvedselectionclasses,
    );

declare_incremental procedure (
        xved_after_editor_entry,
        xved_before_editor_exit,
    );

endsection;

#_INCLUDE '../lib/popc_declare.ph'
