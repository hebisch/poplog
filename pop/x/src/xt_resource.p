/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/src/xt_resource.p
 > Purpose:         X Toolkit - resources and actions
 > Author:          Roger Evans, Jul  5 1988 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xt_declare.ph'
#_INCLUDE '../../src/external.ph'


section $-Sys$-Xt  =>
                        fast_XtGetResourceList,
                        fast_XtGetConstraintResourceList,
                        fast_XtGetApplicationResources,
                        fast_XtVaGetApplicationResources,
                        fast_XtGetValues,
                        fast_XtVaGetValues,
                        fast_XtGetSubvalues,
                        fast_XtVaGetSubvalues,
                        fast_XtSetValues,
                        fast_XtVaSetValues,
                        fast_XtSetSubvalues,
                        fast_XtVaSetSubvalues,
                        sys_XptGetValues,
;


define fast_XtGetResourceList with_nargs 3;
    X_apply(_3, _extern XtGetResourceList) -> ;
enddefine;

define fast_XtGetConstraintResourceList with_nargs 3;
    X_apply(_3, _extern XtGetConstraintResourceList) -> ;
enddefine;

define fast_XtGetApplicationResources with_nargs 6;
    X_apply(_6, _extern XtGetApplicationResources) -> ;
enddefine;

define fast_XtVaGetApplicationResources;
    X_apply(_int(XTC_VARARGS(4)), _extern XtVaGetApplicationResources) -> ;
enddefine;

define fast_XtGetValues with_nargs 3;
    X_apply(_3, _extern XtGetValues) -> ;
enddefine;

define fast_XtVaGetValues;
    X_apply(_int(XTC_VARARGS(1)), _extern XtVaGetValues) -> ;
enddefine;

define fast_XtGetSubvalues with_nargs 5;
    X_apply(_5, _extern XtGetSubvalues) -> ;
enddefine;

define fast_XtVaGetSubvalues;
    X_apply(_int(XTC_VARARGS(3)), _extern XtVaGetSubvalues) -> ;
enddefine;

define fast_XtSetValues with_nargs 3;
#_IF _XtVersion == 11004
    /* call this first to set display at front of _XtPerDisplay list */
    X_apply(subscr_stack(3), _1, _extern XtWidgetToApplicationContext)->;
#_ENDIF
    X_apply(_3, _extern XtSetValues) -> ;
enddefine;

define fast_XtVaSetValues;
#_IF _XtVersion == 11004
    /* call this first to set display at front of _XtPerDisplay list */
    X_apply(subscr_stack(dup() fi_+ 2), _1, _extern XtWidgetToApplicationContext)->;
#_ENDIF
    X_apply(_int(XTC_VARARGS(1)), _extern XtVaSetValues) -> ;
enddefine;

define fast_XtSetSubvalues with_nargs 5;
    X_apply(_5, _extern XtSetSubvalues) -> ;
enddefine;

define fast_XtVaSetSubvalues;
    X_apply(_int(XTC_VARARGS(3)), _extern XtVaSetSubvalues) -> ;
enddefine;


    /*  Used by syntax construct XptVal. Args are

            widget, name1, offs2, name2, ... offsN, nameN, (size<<10)||N

        or just

            widget, name1

        for a single exptr-sized value,

        Returns an exptr_mem with the values filled in at the appropriate
        offsets.
    */
define sys_XptGetValues(_arg) -> em;
    lvars em, _usp, _lim, _lastoffs, _ptr, _nargs, _arg, _size;

    if isinteger(_arg) then
        _int(_arg) -> _arg;
        _shift(_arg, _-10), _arg _bimask _16:3FF
    else
        ;;; _arg is a single name string
        _arg;           ;;; put it back
        ##(b)[_1|XtPointer], _1
    endif -> (_size, _nargs);

    initexptr_mem(_pint(_size)) -> em;
    em!XP_PTR -> _ptr;
    0;                                      ;;; position for last arg ptr
    _shift(_nargs, _1) -> _nargs;           ;;; number of args on stack
    _0 -> _lastoffs;
    _user_sp() -> _lim;
    _lim@(w)[_nargs] -> _usp;               ;;; points to widget pointer
    _usp!(w)!XP_PTR -> _usp!(w);            ;;; deref it
    while _usp >@(w) _lim do
        _usp@(w)[_-2] -> _usp;              ;;; points to offset
        _int(_usp!(w)), _ptr@(b)[_lastoffs] -> (_lastoffs, _usp!(w))
    endwhile;

    dlocal _external_flags = _:PEF_DO_USER_MALLOC, _in_X_call = _1;

    ;;; use _call_sys so we can pass the raw pointers
    _call_sys(_NULL, _nargs _add _2, _extern XtVaGetValues) ->
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 15 1993
        Made sys_XptGetValues locally set PEF_DO_USER_MALLOC in
        _external_flags, to make XtVaGetValues malloc from normal user
        memory instead of internal system block
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- John Gibson, Sep 10 1992
        Rewrote sys_XptGetValues for different args
--- John Gibson, Sep  6 1992
        o Added sys_XptGetValues
        o Uses XTC_VARARGS instead of X*TC_varargs (arg conversion done by
          latter is no longer necessary)
--- Jonathan Meyer, Aug 19 1991
        Added calls to _extern XtWidgetToApplicationContext in SetValues
        and VaSetValues - this is a workaround for the bug in XtDestroyGC,
        which the widget might call during its set_values action.
--- Roger Evans, Oct 20 1990 added X*TC_varargs
--- Roger Evans, Jul  4 1990 changed to use X_apply
 */
