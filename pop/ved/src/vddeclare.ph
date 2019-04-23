/* --- Copyright University of Sussex 2011. All rights reserved. ----------
 > File:            C.all/ved/src/vddeclare.ph
 > Purpose:			Declare variables used by VED source files
 > Author:          John Gibson et al (see revisions)
 */

;;;--------------- VED IDENTIFIER DECLARATIONS ------------------------------

#_INCLUDE '../../src/syspop.ph'
#_INCLUDE '../../lib/include/ved_declare.ph'
#_INCLUDE '../../lib/include/vedfile_struct.ph'
#_INCLUDE '../../lib/include/vedscreendefs.ph'

lconstant macro (
	USEWINDOWS	= [($-Sys$-Ved$-Using_windows)],
	XWINDOWS	= [($-Sys$-Ved$-Using_windows == "x")],
	);

	;;; constant procedures
weak constant
	procedure (
		consveddevice,
		is_vedfile_local,
		ved_save_file_globals,
		ved_winrel_line,
		vedalignscreen,
		vedappfiles,
		vedatitemend,
		vedatitemstart,
		vedbufferextend,
		vedchangecase,
		vedchardelete,
		vedchardown,
		vedchardownleft,
		vedchardownleftlots,
		vedchardownlots,
		vedchardownright,
		vedchardownrightlots,
		vedcharinsert,
		vedcharleft,
		vedcharleftlots,
		vedcharmiddle,
		vedcharright,
		vedcharrightlots,
		vedcharup,
		vedcharupleft,
		vedcharupleftlots,
		vedcharuplots,
		vedcharupright,
		vedcharuprightlots,
		vedcheck,
		vedclearinput,
		vedclearhead,
		vedcleartail,
		vedcompilefiles,
		veddecodetabs,
		veddocommand,
		veddointerrupt,
		veddotdelete,
		vededit,
		vedencodetabs,
		vedendfile,
		vedenter,
		vedexchangeposition,
		vedexit,
		vedfileisonscreen,
		vedhelpdefaults,
		vedinputwaiting,
		vedinsertvedchar,
		vedjumpto,
		vedlineabove,
		vedlinebelow,
		vedlinedelete,
		vedmarked,
		vedmarkhi,
		vedmarklo,
		vedmarkpop,
		vedmarkpush,
		vednextline,
		vedoutascii,
		vedpositionpop,
		vedpositionpush,
		vedprocesschar,
		vedprocswaiting,
		vedrefresh,
		vedrefreshwindow,
		vedrefreshline,
		vedrefreshtail,
		vedscreendown,
		vedscreenleft,
		vedscreenmiddle,
		vedscreenoutput,
		vedscreenright,
		vedscreenup,
		vedsetbottomscreen,
		vedsetcursor,
		vedsetlinesize,
		vedsetstatic,
		vedsetstatus,
		vedsettopscreen,
		vedsetwindow,
		vedstatusswitch,
		vedtabright,
		vedtextleft,
		vedtextright,
		vedthisline,
		vedtopfile,
		vedtrimline,
		vedusedsize,
		vedwindowpoint,
		vedwordleftdelete,
		vedwordrightdelete,
		vedwritefiles,
	),

	active (
		ved_current_file,
		ved_on_status,
		ved_terminal_can_scroll,
	),

	;


	;;; global variables and constants

weak constant
		vedspacestring
		;

weak vars active (
		vedstartwindow,
		vedscreencursoron,
		)
	;

weak vars
		ved_char_in_stream,
		vedalwaysask,
		vedautowrite,
		vedbreak,
		vedbuffer,
		vedchanged,
		vedcolumn,
		vedcolumnoffset,
		vedcompileable,
		vedcurrentfile,
		veddelspaces,
		veddirectory,
		vedescape,
		vedfinished,
		vedgraphicmode,
		vedhelpdirectory,
		vedindentstep,
		vedinputfocus,
		vedfileprops,
		vedleftmargin,
		vedline,
		vedlinemax,
		vedlineoffset,
		vedlowerfile,
		vedneedscompiling,
		vednochardelete,
		vednocharinsert,
		vednokeypad,
		vednolinedelete,
		vednolineinsert,
		vednomoveinsert,
		vednotabs,
		vedonstatus,
		vedpathname,
		vedpositionstack,
		vedprintingdone,
		vedquery,
		vedreadintabs,
		vedrefdirectory,
		vedrefreshneeded,
		vedscreencolumn,

		vedscreenmoremark,
		vedscreencontrolmark,

		vedscreencursorlinemark,
		vedscreenrangemark,
		vedscreencursorlinerangemark,

		vedscreenstatus_-|_mark,
		vedscreenstatus_|-_mark,
		vedscreenstatus_-_mark,

		vedscreenlength,
		vedscreenline,
		vedscreenoffset,
		vedscreenwidth,
		vedscreenwrap,
		vedscrollscreen,
		vedstatic,
		vedstatusheader,
		vedstatusline,
		vedteachdirectory,
		vedterminalname,
		vedupperfile,
		vedwarpcontext,
		vedwindowlength,
		vedwlinewidth,
		vedwlineoffset,
		vedwcolumnoffset,
		vedwriteable,
		vedwriteallfiles,
		vvedbuffersize,
		vveddump,
		vvedlinedump,
		vvedlinesize,
		vvedmarkhi,
		vvedmarklo,
		vvedmarkprops,
		vvedpromptchar,
		vvedscreenalpha,
		vvedscreenbell,
		vvedscreenchardown,
		vvedscreencharleft,
		vvedscreencharright,
		vvedscreencharnormal, vvedscreencharhighlight,
		vvedscreencharbold, vvedscreencharunderline,
		vvedscreencharaltfont, vvedscreencharblink,
		vvedscreencharup,
		vvedscreenclear,
		vvedscreencleartail,
		vvedscreendeletechar,
		vvedscreendeleteline,
		vvedscreengraphic,
		vvedscreeninsertchar,
		vvedscreeninsertline,
		vvedscreeninsertmode,
		vvedscreenovermode,
		vvedscreenpoint,
		vvedscreenscreenleft,
		vvedscreenscrolldown,
		vvedscreenscrollup,
		vvedscreencursorupscroll,
		vvedscreenresetpad,
		vvedscreensetpad,
		vvedscreeninit,
		vvedscreenreset,
		vvedworddump,

		wvedalwaysraise,
		wvedfreewindow,
		wvedwindow,
	;


	;;; non-constant procedures
weak vars procedure (
		prolog_compile,
		lisp_compile,

		vedchartype,
		veddocr,
		vederror,
		vedinitfile,
		vedinitialise,
		vedinterrupt,
		vedpopready,
		vedputmessage,
		vedrestorescreen,
		vedscreenreset,
		vedscreenbell,
		vedscreenpulldown,
		vedscreenpullup,
		vedscreenpushdown,
		vedscreenpushup,
		vedscreencleartail,
		vedscreenxy,
		vedscreenclearhead,
		vedscreencharleft,
		vedscreencharright,
		vedscreencharup,
		vedscreenchardown,
		vedscreenclear,
		vedscreengraphtrans,
		vedscrollhorz,
		vedscrollvert,
		vedscrollregion,
		vedsetonscreen,
		vedsetscrollregion,
		vedveddefaults,
		vedwordleft,
		vedwordright,
		vedscr_char_out,
		ved_da,
		ved_lmr,
		ved_m,
		ved_tidy,
		ved_w,
		ved_w1,
		ved_x1,
		ved_xdn,
		ved_xup,

		wved_file_of_window,
		wved_is_open_window,
		wved_is_live_window,
		wved_open_window,
		wved_set_input_focus,
		wved_should_warp_mouse,
		wved_window_of_file,
		)
	;

section $-Sys$-Ved;

weak constant procedure (
		Check_vedfile,
		Isvedfile,
		Open_file_window,
		Set_changed,
		Set_vedcommand,
		Setonscreen_next,
		Skipwhite,
		Trimwhite,
		Clear_temp_mark,
		Get_left_margin,
		Set_refresh_needed,
		Set_wait_cursor,
		Tab_size_at,
		Check_not_in_tab
		Buffer_line,
		Shift_buffer_lines,
		Shift_string,
		Normalised_line,
		Bottom_of_window,
		Window_line,
		Errmess_sprintf,
	);

weak vars (
		Using_windows,
		marked_range_stack,
		terminal_can_scroll,
	);

endsection;


lconstant macro (
	STATUS_HEADER_LEN = 9,
	MAXSCREENCOL	= 1e4,		;;; N.B. must fit in 16 bits for XVed

	VDDEV_LOADED	= [testdef consveddevice],
	VDDEV_WEAK		= [weakref[consveddevice]],
	);


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov  8 2011
		ved_terminal_can_scroll, active variable, to make it possible for
		user to turn it off. Defined in vdscroll.p
--- John Gibson, Nov  9 1995
		Removed pw*m stuff
--- Jonathan Meyer, Sep 25 1993
		Removed old search vars: ved*foundcol ved*foundline vved*foundstring
		vved*instring ved*srchsize vved*srchstring.
--- John Gibson, Dec 11 1992
		Got rid of G_ macros
--- John Gibson, Dec 11 1992
		Includes syspop.ph instead of src/declare.ph
--- John Gibson, Oct  9 1992
		Now includes lib/include/ved_declare.ph, which contains declarations
		for exported things used weakly in libraries. Removed corresponding
		declarations from this file.
--- John Gibson, Feb  6 1992
		Added graphics character macros
--- John Gibson, Jul  9 1991
		Moved file subscript macros to lib/include/vedfile_struct.ph
--- Jonathan Meyer, Jul  8 1991
		Added wved_ved_quit_file
--- John Gibson, Jun 21 1991
		Added VF_ON_STATUS to file structure
--- Jonathan Meyer, Jun 18 1991
		Added wved_ved_set_window
--- John Gibson, Jun  8 1991
		wved_ rationalisation
--- Jonathan Meyer, Jun  5 1991
		Made wved_file_of_window a vars
--- John Gibson, Apr 11 1991
		Added some things.
--- Aaron Sloman, Nov 27 1990
		made vedinputfocus exported
--- John Gibson, Nov  2 1990
		Added some procedures in Sys$-Ved
--- John Gibson, Oct 31 1990
		Added -vedinputfocus-
--- Aaron Sloman, Sep 25 1990
		added wved_is_open_window, and made other changes for Xved
		Reorganised slightly
--- John Gibson, Aug 27 1990
		Changed n*ote to weak
--- Aaron Sloman, Jul  6 1990
		Added -vedexit- defined (soon) in vdprocess.p
--- Aaron Sloman, Jul  3 1990
		Made -ved*editor- vars procedure
--- Rob Duncan, Feb  2 1990
		Made -vedstartwindow- an active variable
--- Aaron Sloman, Dec 28 1989
		Added vedinitialise, for use in vdfiles.p and vdfileio.p
--- Rob Duncan, Nov  7 1989
		Added various new screen-control variables
--- John Gibson, Apr 30 1989
		Made declarations "weak"
 */
