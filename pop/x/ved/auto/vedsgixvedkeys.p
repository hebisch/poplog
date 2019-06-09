/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/ved/auto/vedsgixvedkeys.p
 > Purpose:         VED: key bindings for SGI console
 > Author:          Robert Duncan, Dec  2 1991
 > Documentation:   HELP *SGIXVEDKEYS
 > Related Files:   C.x/x/ved/lib/vedncdxvedkeys.p
 */
compile_mode :pop11 +strict;

section;

uses vedncdxvedkeys;

define vedsgixvedkeys();
    vedncdxvedkeys();
    vedset keys
        charup              =   (KP_Up)
        chardown            =   (KP_Down)
        charleft            =   (KP_Left)
        charright           =   (KP_Right)
        charuplots          =   esc (KP_Up)
        chardownlots        =   esc (KP_Down)
        charleftlots        =   esc (KP_Left)
        charrightlots       =   esc (KP_Right)
        charupleft          =   (KP_Home)
        charupright         =   (KP_Prior)
        chardownleft        =   (KP_End)
        chardownright       =   (KP_Next)
        charupleftlots      =   esc (KP_Home)
        charuprightlots     =   esc (KP_Prior)
        chardownleftlots    =   esc (KP_End)
        chardownrightlots   =   esc (KP_Next)
        wordleft            =   (KP_Insert)
        wordright           =   (KP_Delete)
    endvedset;
    'sgixved' -> vedkeymapname;
    true -> $-xved$-xvedkeypadon;
enddefine;
;;;
uses-by_name (vedsgixvedkeys);

#_IF pop_runtime
    vedsgixvedkeys -> vedserverxvedkeys;
#_ENDIF

endsection;
