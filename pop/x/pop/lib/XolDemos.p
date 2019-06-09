/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/XolDemos.p
 > Purpose:         Load and run the Xol demos
 > Author:          Jonathan Meyer, Jan 17 1991
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

uses popxdemo;

lib XolDemoUtils
lib XolSamplerDemo
lib XolTextDemo
lib XolControlsDemo

XolDemo();

endsection;
