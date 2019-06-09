/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/XolTutorial.p
 > Purpose:         OpenLook tutorial
 > Author:          Tom Khabaza, Dec  5 1990 (see revisions)
 > Documentation:   TEACH *OPENLOOK, TEACH *MOTIF
 > Related Files:   LIB XmTutorial.p, LIB *XolSamplerDemo.p,
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
 * Simple Poplog X demonstration using Open Look widgets.
 *
 * This program creates a simple control panel for setting the
 * integer values of a set of variable named the global list
 * -variable_list-. The control panel contains two widgets: a
 * button and a slider.  The button label shows a variable name;
 * clicking on the button causes cycling through the list of
 * variables, changing the button label appropriately.  The slider
 * controls the value of the current variable, from 0 to 50.
 */


;;; The application:
;;;     List of initialised global variables

vars variable_list = [a b c d e];
vars a=0, b=0, c=0, d=0, e=0;


;;;     Current variable and place in variable list

vars    current_variable,
        current_variable_list = variable_list;


;;;     Cycling through the variables

define new_current_variable;
    if current_variable_list = [] then
        variable_list -> current_variable_list;
    endif;
    hd(current_variable_list) -> current_variable;
    tl(current_variable_list) -> current_variable_list;
enddefine;



exload_batch

;;; Load X-related libraries
uses popxlib;
uses xt_widget;
uses xt_callback;
uses xt_event;

include xpt_coretypes;

;;; Access widgetclasses
uses
    xolBaseWindowShellWidget,
    xolControlAreaWidget,
    xolOblongButtonWidget,
    xolSliderWidget;

endexload_batch;

include XolConstants;

;;; Initialise Poplog X Toolkit & Open Look widgetset, and connect to server

XptDefaultSetup();


;;; Create base window

vars basewin =  XtAppCreateShell('panel', 'demo', xolBaseWindowShellWidget,
                    XptDefaultDisplay,
                    [{allowShellResize ^false}]);


;;; Create widgets

    vars
        control =   XtCreateManagedWidget('mycontrol', xolControlAreaWidget,
                        basewin, []),
        button =    XtCreateManagedWidget('mybutton', xolOblongButtonWidget,
                        control, []),
        slider =    XtCreateManagedWidget('myslider', xolSliderWidget,
                        control,
                        [{width 100} {orientation ^OL_HORIZONTAL}
                            {sliderMax 50} ]);


;;; Updating resources:
;;;     The button label - current variable name & value

define set_button_label;
    current_variable sys_>< ' ' sys_>< valof(current_variable)
        -> XptValue(button, XtN label, TYPESPEC(:XptString));
    XptAppTryEvents(XptDefaultAppContext); ;;; flush changes to display
enddefine;


;;;     The slider value - current variable value

define set_slider_value;
    valof(current_variable) -> XptValue(slider, XtN sliderValue);
enddefine;


;;; Callback routines:
;;;     For the button

define switch_variable_callback(widget, clientdata, calldata);
    lvars widget, clientdata, calldata;
    new_current_variable();
    set_button_label();
    set_slider_value();
enddefine;

XtAddCallback(button, 'select', switch_variable_callback,
    'Data for button select callback');


;;;     For the slider

define set_variable_callback(widget, clientdata, calldata);
    lvars widget, clientdata, calldata;
    exacc :int calldata -> valof(current_variable);
    set_button_label();
enddefine;

XtAddCallback(slider, 'sliderMoved', set_variable_callback,
    'Data for slider sliderMoved callback');


;;; Realise the widgets

XtRealizeWidget(basewin);


;;; Initialise the application and set the button label

new_current_variable();
set_button_label();


;;; Updating the widget appearance

define update_slider(x);
    lvars x;
    x -> valof(current_variable);
    set_button_label();
    set_slider_value();
enddefine;

update_slider(45);


/*

    ;;; The widgets can be destroyed by making them become garbage...
    false ->> basewin ->> control ->> slider -> button;

    ;;; ... or explicitly destroying them
    XtDestroyWidget(basewin);


*/


/* --- Revision History ---------------------------------------------------
--- Integral Solutions Ltd, Nov 22 1993
        Modified to use xol* constants.
--- Adrian Howard, Mar 19 1992
        o Added -update_slider-
        o Added commented section on destroying the widgets (just so we have
        all the code from TEACH *OPENLOOK)
--- Integral Solutions Ltd, Oct 22 1991 (Julian Clinton)
    Changed >< to sys_><.
--- Jonathan Meyer, Sep 12 1991
        Changed sysxsetup to XptDefaultSetup
--- Jonathan Meyer, Feb 15 1991 Added reference to TEACH *MOTIF
--- Jonathan Meyer, Feb  8 1991
    Added TYPESPEC and used XtN macro to make fixed address strings.
--- Jonathan Meyer, Jan 17 1991
    Added XptAppTryEvents in set_label, and also changed some word quotes
    to string quotes.
 */
