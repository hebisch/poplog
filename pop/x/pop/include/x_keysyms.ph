/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/x_keysyms.ph
 > Purpose:         Keysym number codes for X Keyboards.
 > Author:          Jonathan Meyer, Dec 12 1990 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF X_KEYSYMS_INCLUDED

;;; Defines most of the basic keysym macros for X, and also defines
;;; inline macros for determining if keys are in certain categories.

section;

iconstant macro (

    XNoSymbol = 0,
    XK_VoidSymbol          = 16:FFFFFF,

/*
    * TTY Functions, cleverly chosen to map to ascii, for convenience of
    * programming, but could have been arbitrary (at the cost of lookup
    * tables in client code.
    */

    XK_BackSpace            = 16:FF08,  /* back space, back char */
    XK_Tab                  = 16:FF09,
    XK_Linefeed             = 16:FF0A,  /* Linefeed, LF */
    XK_Clear                = 16:FF0B,
    XK_Return               = 16:FF0D,  /* Return, enter */
    XK_Pause                = 16:FF13,  /* Pause, hold, scroll lock */
    XK_Escape               = 16:FF1B,
    XK_Delete               = 16:FFFF,  /* Delete, rubout */



/* International & multi-key character composition */

    XK_Multi_key            = 16:FF20,  /* Multi-key character compose */
    XK_Kanji                = 16:FF21,  /* Kanji, Kanji convert */

/* Cursor control & motion */

    XK_Home                 = 16:FF50,
    XK_Left                 = 16:FF51,  /* Move left, left arrow */
    XK_Up                   = 16:FF52,  /* Move up, up arrow */
    XK_Right                = 16:FF53,  /* Move right, right arrow */
    XK_Down                 = 16:FF54,  /* Move down, down arrow */
    XK_Prior                = 16:FF55,  /* Prior, previous */
    XK_Next                 = 16:FF56, /* Next */
    XK_End                  = 16:FF57,  /* EOL */
    XK_Begin                = 16:FF58,  /* BOL */


/* Misc Functions */

    XK_Select               = 16:FF60,  /* Select, mark */
    XK_Print                = 16:FF61,
    XK_Execute              = 16:FF62,  /* Execute, run, do */
    XK_Insert               = 16:FF63,  /* Insert, insert here */
    XK_Undo                 = 16:FF65,  /* Undo, oops */
    XK_Redo                 = 16:FF66,  /* redo, again */
    XK_Menu                 = 16:FF67,
    XK_Find                 = 16:FF68,  /* Find, search */
    XK_Cancel               = 16:FF69,  /* Cancel, stop, abort, exit */
    XK_Help                 = 16:FF6A,  /* Help, ? */
    XK_Break                = 16:FF6B,
    XK_Mode_switch          = 16:FF7E,  /* Character set switch */
    XK_script_switch        = 16:FF7E,  /* Alias for mode_switch */
    XK_Num_Lock             = 16:FF7F,

/* Keypad Functions, keypad numbers cleverly chosen to map to ascii */

    XK_KP_Space             = 16:FF80,  /* space */
    XK_KP_Tab               = 16:FF89,
    XK_KP_Enter             = 16:FF8D,  /* enter */
    XK_KP_F1                = 16:FF91,  /* PF1, KP_A, ... */
    XK_KP_F2                = 16:FF92,
    XK_KP_F3                = 16:FF93,
    XK_KP_F4                = 16:FF94,
    XK_KP_Equal             = 16:FFBD,  /* equals */
    XK_KP_Multiply          = 16:FFAA,
    XK_KP_Add               = 16:FFAB,
    XK_KP_Separator         = 16:FFAC,  /* separator, often comma */
    XK_KP_Subtract          = 16:FFAD,
    XK_KP_Decimal           = 16:FFAE,
    XK_KP_Divide            = 16:FFAF,

    XK_KP_0                 = 16:FFB0,
    XK_KP_1                 = 16:FFB1,
    XK_KP_2                 = 16:FFB2,
    XK_KP_3                 = 16:FFB3,
    XK_KP_4                 = 16:FFB4,
    XK_KP_5                 = 16:FFB5,
    XK_KP_6                 = 16:FFB6,
    XK_KP_7                 = 16:FFB7,
    XK_KP_8                 = 16:FFB8,
    XK_KP_9                 = 16:FFB9,



/*
    * Auxilliary Functions; note the duplicate definitions for left and right
    * function keys;  Sun keyboards and a few other manufactures have such
    * function key groups on the left and/or right sides of the keyboard.
    * We've not found a keyboard with more than 35 function keys total.
    */

    XK_F1                   = 16:FFBE,
    XK_F2                   = 16:FFBF,
    XK_F3                   = 16:FFC0,
    XK_F4                   = 16:FFC1,
    XK_F5                   = 16:FFC2,
    XK_F6                   = 16:FFC3,
    XK_F7                   = 16:FFC4,
    XK_F8                   = 16:FFC5,
    XK_F9                   = 16:FFC6,
    XK_F10                  = 16:FFC7,
    XK_F11                  = 16:FFC8,
    XK_L1                   = 16:FFC8,
    XK_F12                  = 16:FFC9,
    XK_L2                   = 16:FFC9,
    XK_F13                  = 16:FFCA,
    XK_L3                   = 16:FFCA,
    XK_F14                  = 16:FFCB,
    XK_L4                   = 16:FFCB,
    XK_F15                  = 16:FFCC,
    XK_L5                   = 16:FFCC,
    XK_F16                  = 16:FFCD,
    XK_L6                   = 16:FFCD,
    XK_F17                  = 16:FFCE,
    XK_L7                   = 16:FFCE,
    XK_F18                  = 16:FFCF,
    XK_L8                   = 16:FFCF,
    XK_F19                  = 16:FFD0,
    XK_L9                   = 16:FFD0,
    XK_F20                  = 16:FFD1,
    XK_L10                  = 16:FFD1,
    XK_F21                  = 16:FFD2,
    XK_R1                   = 16:FFD2,
    XK_F22                  = 16:FFD3,
    XK_R2                   = 16:FFD3,
    XK_F23                  = 16:FFD4,
    XK_R3                   = 16:FFD4,
    XK_F24                  = 16:FFD5,
    XK_R4                   = 16:FFD5,
    XK_F25                  = 16:FFD6,
    XK_R5                   = 16:FFD6,
    XK_F26                  = 16:FFD7,
    XK_R6                   = 16:FFD7,
    XK_F27                  = 16:FFD8,
    XK_R7                   = 16:FFD8,
    XK_F28                  = 16:FFD9,
    XK_R8                   = 16:FFD9,
    XK_F29                  = 16:FFDA,
    XK_R9                   = 16:FFDA,
    XK_F30                  = 16:FFDB,
    XK_R10                  = 16:FFDB,
    XK_F31                  = 16:FFDC,
    XK_R11                  = 16:FFDC,
    XK_F32                  = 16:FFDD,
    XK_R12                  = 16:FFDD,
    XK_R13                  = 16:FFDE,
    XK_F33                  = 16:FFDE,
    XK_F34                  = 16:FFDF,
    XK_R14                  = 16:FFDF,
    XK_F35                  = 16:FFE0,
    XK_R15                  = 16:FFE0,

/* Modifiers */

    XK_Shift_L              = 16:FFE1,  /* Left shift */
    XK_Shift_R              = 16:FFE2,  /* Right shift */
    XK_Control_L            = 16:FFE3,  /* Left control */
    XK_Control_R            = 16:FFE4,  /* Right control */
    XK_Caps_Lock            = 16:FFE5,  /* Caps lock */
    XK_Shift_Lock           = 16:FFE6,  /* Shift lock */

    XK_Meta_L               = 16:FFE7,  /* Left meta */
    XK_Meta_R               = 16:FFE8,  /* Right meta */
    XK_Alt_L                = 16:FFE9,  /* Left alt */
    XK_Alt_R                = 16:FFEA,  /* Right alt */
    XK_Super_L              = 16:FFEB,  /* Left super */
    XK_Super_R              = 16:FFEC,  /* Right super */
    XK_Hyper_L              = 16:FFED,  /* Left hyper */
    XK_Hyper_R              = 16:FFEE,  /* Right hyper */

    /*
     *  Latin 1
     *  Byte 3 = 0
     */
    XK_space               = 16:020,
    XK_exclam              = 16:021,
    XK_quotedbl            = 16:022,
    XK_numbersign          = 16:023,
    XK_dollar              = 16:024,
    XK_percent             = 16:025,
    XK_ampersand           = 16:026,
    XK_quoteright          = 16:027,
    XK_parenleft           = 16:028,
    XK_parenright          = 16:029,
    XK_asterisk            = 16:02A,
    XK_plus                = 16:02B,
    XK_comma               = 16:02C,
    XK_minus               = 16:02D,
    XK_period              = 16:02E,
    XK_slash               = 16:02F,
    XK_0                   = 16:030,
    XK_1                   = 16:031,
    XK_2                   = 16:032,
    XK_3                   = 16:033,
    XK_4                   = 16:034,
    XK_5                   = 16:035,
    XK_6                   = 16:036,
    XK_7                   = 16:037,
    XK_8                   = 16:038,
    XK_9                   = 16:039,
    XK_colon               = 16:03A,
    XK_semicolon           = 16:03B,
    XK_less                = 16:03C,
    XK_equal               = 16:03D,
    XK_greater             = 16:03E,
    XK_question            = 16:03F,
    XK_at                  = 16:040,
    XK_A                   = 16:041,
    XK_B                   = 16:042,
    XK_C                   = 16:043,
    XK_D                   = 16:044,
    XK_E                   = 16:045,
    XK_F                   = 16:046,
    XK_G                   = 16:047,
    XK_H                   = 16:048,
    XK_I                   = 16:049,
    XK_J                   = 16:04A,
    XK_K                   = 16:04B,
    XK_L                   = 16:04C,
    XK_M                   = 16:04D,
    XK_N                   = 16:04E,
    XK_O                   = 16:04F,
    XK_P                   = 16:050,
    XK_Q                   = 16:051,
    XK_R                   = 16:052,
    XK_S                   = 16:053,
    XK_T                   = 16:054,
    XK_U                   = 16:055,
    XK_V                   = 16:056,
    XK_W                   = 16:057,
    XK_X                   = 16:058,
    XK_Y                   = 16:059,
    XK_Z                   = 16:05A,
    XK_bracketleft         = 16:05B,
    XK_backslash           = 16:05C,
    XK_bracketright        = 16:05D,
    XK_asciicircum         = 16:05E,
    XK_underscore          = 16:05F,
    XK_quoteleft           = 16:060,
    XK_a                   = 16:061,
    XK_b                   = 16:062,
    XK_c                   = 16:063,
    XK_d                   = 16:064,
    XK_e                   = 16:065,
    XK_f                   = 16:066,
    XK_g                   = 16:067,
    XK_h                   = 16:068,
    XK_i                   = 16:069,
    XK_j                   = 16:06A,
    XK_k                   = 16:06B,
    XK_l                   = 16:06C,
    XK_m                   = 16:06D,
    XK_n                   = 16:06E,
    XK_o                   = 16:06F,
    XK_p                   = 16:070,
    XK_q                   = 16:071,
    XK_r                   = 16:072,
    XK_s                   = 16:073,
    XK_t                   = 16:074,
    XK_u                   = 16:075,
    XK_v                   = 16:076,
    XK_w                   = 16:077,
    XK_x                   = 16:078,
    XK_y                   = 16:079,
    XK_z                   = 16:07A,
    XK_braceleft           = 16:07B,
    XK_bar                 = 16:07C,
    XK_braceright          = 16:07D,
    XK_asciitilde          = 16:07E,

    XK_nobreakspace        = 16:0A0,
    XK_exclamdown          = 16:0A1,
    XK_cent                = 16:0A2,
    XK_sterling            = 16:0A3,
    XK_currency            = 16:0A4,
    XK_yen                 = 16:0A5,
    XK_brokenbar           = 16:0A6,
    XK_section             = 16:0A7,
    XK_diaeresis           = 16:0A8,
    XK_copyright           = 16:0A9,
    XK_ordfeminine         = 16:0AA,
    XK_guillemotleft       = 16:0AB,    /* left angle quotation mark */
    XK_notsign             = 16:0AC,
    XK_hyphen              = 16:0AD,
    XK_registered          = 16:0AE,
    XK_macron              = 16:0AF,
    XK_degree              = 16:0B0,
    XK_plusminus           = 16:0B1,
    XK_twosuperior         = 16:0B2,
    XK_threesuperior       = 16:0B3,
    XK_acute               = 16:0B4,
    XK_mu                  = 16:0B5,
    XK_paragraph           = 16:0B6,
    XK_periodcentered      = 16:0B7,
    XK_cedilla             = 16:0B8,
    XK_onesuperior         = 16:0B9,
    XK_masculine           = 16:0BA,
    XK_guillemotright      = 16:0BB,    /* right angle quotation mark */
    XK_onequarter          = 16:0BC,
    XK_onehalf             = 16:0BD,
    XK_threequarters       = 16:0BE,
    XK_questiondown        = 16:0BF,
    XK_Agrave              = 16:0C0,
    XK_Aacute              = 16:0C1,
    XK_Acircumflex         = 16:0C2,
    XK_Atilde              = 16:0C3,
    XK_Adiaeresis          = 16:0C4,
    XK_Aring               = 16:0C5,
    XK_AE                  = 16:0C6,
    XK_Ccedilla            = 16:0C7,
    XK_Egrave              = 16:0C8,
    XK_Eacute              = 16:0C9,
    XK_Ecircumflex         = 16:0CA,
    XK_Ediaeresis          = 16:0CB,
    XK_Igrave              = 16:0CC,
    XK_Iacute              = 16:0CD,
    XK_Icircumflex         = 16:0CE,
    XK_Idiaeresis          = 16:0CF,
    XK_Eth                 = 16:0D0,
    XK_Ntilde              = 16:0D1,
    XK_Ograve              = 16:0D2,
    XK_Oacute              = 16:0D3,
    XK_Ocircumflex         = 16:0D4,
    XK_Otilde              = 16:0D5,
    XK_Odiaeresis          = 16:0D6,
    XK_multiply            = 16:0D7,
    XK_Ooblique            = 16:0D8,
    XK_Ugrave              = 16:0D9,
    XK_Uacute              = 16:0DA,
    XK_Ucircumflex         = 16:0DB,
    XK_Udiaeresis          = 16:0DC,
    XK_Yacute              = 16:0DD,
    XK_Thorn               = 16:0DE,
    XK_ssharp              = 16:0DF,
    XK_agrave              = 16:0E0,
    XK_aacute              = 16:0E1,
    XK_acircumflex         = 16:0E2,
    XK_atilde              = 16:0E3,
    XK_adiaeresis          = 16:0E4,
    XK_aring               = 16:0E5,
    XK_ae                  = 16:0E6,
    XK_ccedilla            = 16:0E7,
    XK_egrave              = 16:0E8,
    XK_eacute              = 16:0E9,
    XK_ecircumflex         = 16:0EA,
    XK_ediaeresis          = 16:0EB,
    XK_igrave              = 16:0EC,
    XK_iacute              = 16:0ED,
    XK_icircumflex         = 16:0EE,
    XK_idiaeresis          = 16:0EF,
    XK_eth                 = 16:0F0,
    XK_ntilde              = 16:0F1,
    XK_ograve              = 16:0F2,
    XK_oacute              = 16:0F3,
    XK_ocircumflex         = 16:0F4,
    XK_otilde              = 16:0F5,
    XK_odiaeresis          = 16:0F6,
    XK_division            = 16:0F7,
    XK_oslash              = 16:0F8,
    XK_ugrave              = 16:0F9,
    XK_uacute              = 16:0FA,
    XK_ucircumflex         = 16:0FB,
    XK_udiaeresis          = 16:0FC,
    XK_yacute              = 16:0FD,
    XK_thorn               = 16:0FE,
    XK_ydiaeresis          = 16:0FF,

);

define :inline iconstant IsKeypadKey(keysym);
    (keysym >= XK_KP_Space and keysym <= XK_KP_Equal)
enddefine;

define :inline iconstant IsCursorKey(keysym);
    (keysym >= XK_Home and keysym <= XK_Select)
enddefine;

define :inline iconstant IsPFKey(keysym);
    (keysym >= XK_KP_F1 and keysym <= XK_KP_F4)
enddefine;

define :inline iconstant IsFunctionKey(keysym);
    (keysym >= XK_F1 and keysym <= XK_F35)
enddefine;

define :inline iconstant IsMiscFunctionKey(keysym);
    (keysym >= XK_Select and keysym <= XK_KP_Space)
enddefine;

define :inline iconstant IsModifierKey(keysym);
    (keysym >= XK_Shift_L and keysym <= XK_Hyper_R)
enddefine;

iconstant X_KEYSYMS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Nov 22 1991 : Added XNoSymbol (NoSymbol in Xlib)
--- Adrian Howard, Aug 12 1991 : Renamed from xkeysyms.ph to x_keysyms.ph
--- Jonathan Meyer, Aug  2 1991 Now uses :inline iconstant
--- Jonathan Meyer, Aug  1 1991 Renamed xkeysyms.ph
 */
