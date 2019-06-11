/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 > File:           C.all/lib/ved/keypad.p
 > Purpose:        for terminals without keypads
 > Author:         Aaron Sloman, July 1984 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
This may be useful for users of VDUs which do not have an 'alternate keypad'
mode, such as the Visual 200 and VT100 provide. This file enables short or
longish cursor moves in EIGHT directions to be achieved by using <ESC>
followed by a number key - one of 1 2 3 4 6 7 8 9.
The sequence <ESC> 5 switches modes, between short and long moves.

We associate 8 directions of movement with <ESC>
followed by one of the keypad numbers 1 2 3 4 6 7 8 9,
and make <ESC> 5 switch between small moves and largeish moves.

*/

uses vedset

section;

global vars ved_large_moves = false;

/* Make <ESC> 5 switch scale of move */

define lconstant vedesc5key();
    not(ved_large_moves) -> ved_large_moves;
    vedputmessage(if ved_large_moves then 'LARGE' else 'SMALL'endif);
enddefine;


define ved_largeorsmall(l,s);
    lvars l, s;
    if ved_large_moves then l() else s() endif
enddefine;

;;; Now alter the character table for '<ESC> char' sequences.


vedsetkey('\^[1', ved_largeorsmall(%vedchardownleftlots, vedchardownleft%));
vedsetkey('\^[2', ved_largeorsmall(%vedchardownlots, vedchardown%));
vedsetkey('\^[3', ved_largeorsmall(%vedchardownrightlots, vedchardownright%));
vedsetkey('\^[4', ved_largeorsmall(%vedcharleftlots, vedcharleft%));

vedsetkey('\^[6', ved_largeorsmall(%vedcharrightlots, vedcharright%));
vedsetkey('\^[7', ved_largeorsmall(%vedcharupleftlots, vedcharupleft%));
vedsetkey('\^[8', ved_largeorsmall(%vedcharuplots , vedcharup%));
vedsetkey('\^[9', ved_largeorsmall(%vedcharuprightlots , vedcharupright%));


vedset keys
    screenup        = esc ,
    screendown      = esc .
    topfile         = esc t
    endfile         = esc b
    esc5key         = esc 5
    enterkey        = esc e
    redokey         = esc cr
    statusswitch    = esc 0
    refresh         = ctrl L
endvedset

endsection;


/*  --- Revision History ---------------------------------------------------
--- Jason Handby, Jul 12 1988 - changed to use vedset notation
--- Ben Rubinstein, Oct 12 1986 - vedenter, vedredo indirected through ..key
*/
