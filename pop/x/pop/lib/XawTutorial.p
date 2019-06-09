/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/lib/XawTutorial.p
 > Purpose:         Athena tutorial code
 > Author:          Andreas Schoter, Jul  1 1991 (see revisions)
 > Documentation:   TEACH *ATHENA
 > Related Files:   LIB *XmTutorial, *XolTutorial
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
 * Simple Poplog X demonstration using Athena widgets.
 *
 * This program creates a simple control panel for setting the
 * integer values of a set of variables named in the global list
 * -variable_list-. The control panel contains three widgets: a
 * scale and two buttons.  One button label shows a variable name;
 * clicking on that button causes cycling through the list of
 * variables, changing the button label appropriately.  The second
 * button terminates the demonstration.  The scale controls the
 * value of the current variable, from 0 to 50.
 */


;;; The application:
;;;     List of initialised global variables

vars    variable_list = [a b c d e],
        a=0, b=0, c=0, d=0, e=0;


;;;     Current variable and place in variable list

vars    current_variable = hd(variable_list),
        current_variable_list = tl(variable_list);


;;;     Cycling through the variables

define get_next_variable();
    if current_variable_list == [] then
        variable_list -> current_variable_list;
    endif;
    hd(current_variable_list) -> current_variable;
    tl(current_variable_list) -> current_variable_list;
enddefine;


;;; Load X-related libraries

uses popxlib;
uses xt_widget;
uses xt_callback;
uses xt_widgetinfo;


;;; Access widgetclasses

exload_batch;

    ;;; load Widget Set
    constant AWS = XptWidgetSet("Athena");

    ;;; load required Widget Classes
    constant
        AppShell = XptWidgetSet("Toolkit")("ApplicationShellWidget"),
        Box = AWS("BoxWidget"),
        ScrollBar = AWS("ScrollbarWidget"),
        Command = AWS("CommandWidget");

endexload_batch;


;;; Initialise Poplog X Toolkit and connect to server

XptDefaultSetup();


;;; Create base window

vars shell = XtAppCreateShell('demo', 'Demo', AppShell,
                XptDefaultDisplay, []);


;;; Create widgets

vars box = XtCreateManagedWidget('box', Box, shell,
            [{orientation ^XtOrientHorizontal}]);

vars varButton = XtCreateManagedWidget('varButton', Command, box,
                [{label 'a 0 '} {resize ^false} {justify ^XtJustifyLeft}]);

vars scrollbar = XtCreateManagedWidget('scrollbar', ScrollBar, box,
                [{length 200} {minimumThumb 20}
                 {orientation ^XtOrientHorizontal}]);

vars quitButton = XtCreateManagedWidget('quitButton', Command, box,
                [{label 'QUIT'}]);


;;; Updating resources:
;;;     The thumb position - current variable value

define update_thumb();
    (valof(current_variable) / 50.0)
        -> XptValue(scrollbar,XtN topOfThumb, "float");
enddefine;


;;;     The button label - current variable name & value

define update_label();
    current_variable sys_>< ' ' sys_>< valof(current_variable)
        -> XptValue(varButton, XtN label, TYPESPEC(:XptString));
enddefine;


;;; Callback routines:
;;;     For the variable button

define switch_callback(widget, client_data, call_data);
    lvars widget, client_data, call_data;
        get_next_variable();
        update_label();
        update_thumb();
enddefine;

XtAddCallback(varButton, XtN callback, switch_callback, false);


;;;     For the scrollbar

define scroll_callback(widget, client_data, call_data);
    lvars widget, client_data, call_data;
    l_typespec call_data :float;
    intof((exacc call_data) * 50) -> valof(current_variable);
    update_label();
enddefine;

XtAddCallback(scrollbar, XtN jumpProc, scroll_callback, false);


;;;     For the quit button

define exit_callback(widget, client_data, call_data);
    lvars widget, client_data, call_data;
    XtDestroyWidget(shell);
enddefine;

XtAddCallback(quitButton, XtN callback, exit_callback, false);


;;; Realising the widgets

XtRealizeWidget(shell);


;;; Updating the widget appearance

define update_var(x);
    lvars x;
    x -> valof(current_variable);
    update_label();
    update_thumb();
enddefine;

update_var(45);


/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Mar 20 1992
        Rewrote to make it match the TEACH file and be closer to the
        applications built in TEACH *OPENLOOK and TEACH *MOTIF
--- Integral Solutions Ltd, Oct 22 1991 (Julian Clinton)
    Changed >< to sys_><.
--- Jonathan Meyer, Sep 12 1991
        Changed sysxsetup to XptDefaultSetup
--- Jonathan Meyer, Jul  2 1991 Changed comment at top of file (used to
        say "Motif" rather than Athena).
        Stopped null-terminating strings - less efficient, but easier
        to read for a tutorial
 */
