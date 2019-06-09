/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/XmDemos.p
 > Purpose:         Load and run the Motif demos
 > Author:          Jonathan Meyer, Jan 17 1991
 > Documentation:   TEACH *XmDemos
 > Related Files:   LIB *XmDemoUtils
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

uses popxdemo;

lib XmDemoUtils
lib XmDialogsDemo
lib XmTextDemo
lib XmControlsDemo

XmDemo();

endsection;
