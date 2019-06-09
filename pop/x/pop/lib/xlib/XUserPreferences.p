/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XUserPreferences.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;

global constant macro (

 AutoRepeatModeOff       = 0,
 AutoRepeatModeOn        = 1,
 AutoRepeatModeDefault   = 2,

 LedModeOff      = 0,
 LedModeOn       = 1,

/* masks for ChangeKeyboardControl */

 KBKeyClickPercent   = (1 << 0),
 KBBellPercent       = (1 << 1),
 KBBellPitch         = (1 << 2),
 KBBellDuration      = (1 << 3),
 KBLed               = (1 << 4),
 KBLedMode           = (1 << 5),
 KBKey               = (1 << 6),
 KBAutoRepeatMode    = (1 << 7),

);


external declare XUserPreferences in c;
    (external_import_procedure XptImportProcedure)


    /* Data structure for XChangeKeyboardControl */

    typedef struct {
            int key_click_percent;
            int bell_percent;
            int bell_pitch;
            int bell_duration;
            int led;
            int led_mode;
            int key;
            int auto_repeat_mode;   ;;; On, Off, Default */
    } XKeyboardControl;


    /* Data structure for XGetKeyboardControl */

    typedef struct {
            int key_click_percent;
        int bell_percent;
        unsigned int bell_pitch, bell_duration;
        unsigned long led_mask;
        int global_auto_repeat;
        char auto_repeats[32];
    } XKeyboardState;


void XAutoRepeatOff(display)
Display *display;
{}

void XAutoRepeatOn(display)
Display *display;
{}

void XBell(display, percent)
Display *display;
int percent;
{}

char *XGetDefault(display, program, option)
Display *display;
char *program;
char *option;
{}

;;; duplicated from Pointers
void XGetPointerControl(display, accel_numerator, accel_denominator, threshold)
Display *display;
int *accel_numerator, *accel_denominator;       ;;; RETURN
int *threshold;                                 ;;; RETURN
{}

void XGetKeyboardControl(display, values)
Display *display;
XKeyboardState *values;                     ;;; RETURN
{}

void XChangeKeyboardControl(display, value_mask, values)
Display *display;
unsigned long value_mask;
XKeyboardControl *values;
{}



endexternal;


xlib_external_require XUserPreferences;


global vars XUserPreferences = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
