/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/XmTutorial.p
 > Purpose:         Motif tutorial code
 > Author:          Jonathan Meyer, Feb 15 1991 (see revisions)
 > Documentation:   TEACH *MOTIF, TEACH *OPENLOOK
 > Related Files:   LIB *XolTutorial.p
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
 * Simple Poplog X demonstration using Motif widgets.
 *
 * This program creates a simple control panel for setting the
 * integer values of a set of variable named the global list
 * -variable_list-. The control panel contains two widgets: a
 * button and a scale.  The button label shows a variable name;
 * clicking on the button causes cycling through the list of
 * variables, changing the button label appropriately.  The scale
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




exload_batch;

;;; Load X-related libraries
uses popxlib;
uses xt_widget;
uses xt_callback;
uses xt_event;

include xpt_coretypes;

;;; Access widgetclasses
uses
    xtApplicationShellWidget,
    xmRowColumnWidget,
    xmPushButtonWidget,
    xmScaleWidget;

endexload_batch;

include XmConstants;

;;; Initialise Poplog X Toolkit and connect to server

XptDefaultSetup();


;;; Create top level window

vars topwin =  XtAppCreateShell('panel', 'Demo', xtApplicationShellWidget,
                        XptDefaultDisplay,
                        [{allowShellResize ^true}]);


;;; Create widgets

vars
        rowcol =   XtCreateManagedWidget('mycontrol', xmRowColumnWidget,
                        topwin, [{orientation ^XmHORIZONTAL}]),

        button =   XtCreateManagedWidget('mybutton', xmPushButtonWidget,
                        rowcol, [{recomputeSize ^false}]),

        scale =    XtCreateManagedWidget('myscale', xmScaleWidget,
                        rowcol, [
                            {width 100}
                            {orientation ^XmHORIZONTAL}
                            {maximum 50}]);


;;; Updating resources:
;;;     The button label - current variable name & value

define set_button_label;
    lvars string;

    XmStringCreateLtoR(
        current_variable sys_>< ' ' sys_>< valof(current_variable),
        XmSTRING_DEFAULT_CHARSET) -> string;

    string -> XptValue(button, XmN labelString, TYPESPEC(:XmString));
enddefine;


;;;     The scale value - current variable value

define set_scale_value;
    valof(current_variable) -> XptValue(scale, XmN value);
enddefine;


;;; Callback routines:
;;;     For the button

define switch_variable_callback(widget, client_data, call_data);
    lvars widget, client_data, call_data;
    new_current_variable();
    set_button_label();
    set_scale_value();
enddefine;

XtAddCallback(button, XmN activateCallback,
        switch_variable_callback,
        'Data for button activate callback');

;;;     For the slider

define set_variable_callback(widget, client_data, call_data);
    lvars widget, client_data, call_data;
    l_typespec call_data: XmScaleCallbackStruct;

    exacc call_data.value -> valof(current_variable);

    set_button_label();
enddefine;

XtAddCallback(scale, XmN valueChangedCallback,
        set_variable_callback,
        'Data for scale valueChanged callback');

XtAddCallback(scale, XmN dragCallback,
        set_variable_callback,
        'Data for scale drag callback');


;;; Initialise the application and set the button label

new_current_variable();
set_button_label();


;;; Display the widgets

XtRealizeWidget(topwin);


;;; Give the scale a label

vars label =
    XmStringCreateLtoR('value', XmSTRING_DEFAULT_CHARSET);
label -> XptValue(scale, XmN titleString, TYPESPEC(:XmString));


;;; Show the current value of the scale/10

true -> XptValue(scale, XmN showValue, TYPESPEC(:XptBoolean));
1 -> XptValue(scale, XtN decimalPoints);


;;; Updating the scale

define update_scale(x);
    lvars x;
    x -> valof(current_variable);
    set_button_label();
    set_scale_value();
enddefine;

update_scale(45);


/*

    ;;; The widgets can be destroyed by making them become garbage...
    false ->> topwin ->> rowcol ->> scale -> button;

    ;;; ... or explicitly destroying them
    XtDestroyWidget(topwin);


*/


/* --- Revision History ---------------------------------------------------
--- Integral Solutions Ltd (Julian Clinton), Nov 22 1993
        Modified to use xm* constants.
--- Adrian Howard, Mar 19 1992
        o Added -update_scale-
        o Added -set_variable_callback- to valueChangedCallback
--- Integral Solutions Ltd, Oct 22 1991 (Julian Clinton)
    Changed >< to sys_><.
--- Jonathan Meyer, Sep 12 1991
        Changed sysxsetup to XptDefaultSetup
 */
