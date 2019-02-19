/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/subscr_stack.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *STACK
 */

;;; -------------------- SUBSCR_STACK AND UPDATER -----------------------------

#_INCLUDE 'declare.ph'

;;; -------------------------------------------------------------------------

section $-Sys => subscr_stack;

define :inline lconstant CHECK_SUBSCR();
    unless isinteger(_num)
    and (--@@(w)[_int(_num)] -> _offs; _stklength() _gr _offs) then
        mishap(_num, 1, 'BAD SUBSCRIPT FOR INDEXED STACK ACCESS');
        return
    endunless
enddefine;

define subscr_stack(_num);
    lvars _offs, _num;
    CHECK_SUBSCR();
    _user_sp()!(w){_offs}
enddefine;
;;;
define updaterof subscr_stack(item, _num);
    lvars item, _offs, _num;
    CHECK_SUBSCR();
    ;;; can't say  item -> _user_sp()!(w){_offs}  !!
    _user_sp()@(w){_offs} -> _offs;
    item -> _offs!(w)
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  5 1996
        Speeded up subscr_stack. Got rid of St*ack_apply.
--- Roger Evans, Jul  8 1990 made St*ack_apply safer!
--- Roger Evans, Jul  5 1990 added St*ack_apply (for Xpop use)
--- John Gibson, Mar 11 1988
        Renamed subscr_stack.p (previously subscrstk.p)
 */
