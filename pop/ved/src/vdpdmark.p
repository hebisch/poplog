/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/ved/src/vdpdmark.p
 > Purpose:         Mark current procedure, and related utilities.
 > Author:          Aaron Sloman (see revisions)
 */

;;; --------------------- MARKING PROCEDURES ----------------------------------

#_INCLUDE 'vddeclare.ph'

constant
        procedure (vedmarkfind, vedrepeater, vedlinestart)
    ;

vars
        vedvarskeywords
    ;


define vars ved_mbp;
    ;;; Mark from beginning of procedure
    ;;; Allow 'function' for compatibility with POP2
    lvars start = vedline;
    dlocal vedline, vedcolumn, vvedlinesize;
    until vedlinestart("define") or vedlinestart("function") do
        if vedline == 1 then
            vederror('\{b}ndb: no define before line '
                                        <> (start sys_>< nullstring))
        endif;
        vedline fi_- 1 -> vedline
    enduntil;
    vedmarklo()
enddefine;

define vars ved_mep;
    ;;; Mark to end of procedure, matching opening and closing
    ;;; brackets of modules. Can't work if started in middle of nested
    ;;; procedure. Doesn't cope with [% "]" %]
    lvars item, procedure items = incharitem(vedrepeater), _invars = false,
        _inquote = 0, _instruct = 0, _pdcount = 0 ;
    dlocal vedline, vedcolumn, vvedlinesize, poplastchar;
    until (items() ->> item) == termin do

        if _inquote fi_> 0 then
            _inquote fi_- 1 -> _inquote;
        elseif _invars then
            item /== ";" -> _invars
        elseif lmember(item, vedvarskeywords) then
            true -> _invars
        elseif item == """ then
            2 -> _inquote
        elseif item == "[" or item == "{" then
            _instruct fi_+ 1 -> _instruct
        elseif item == "]" or item == "}" then
            _instruct fi_- 1 -> _instruct
        elseif _instruct fi_> 0 then
            ;;; in list or vector - ignore item
        elseif lmember(item, [nonsyntax nonmac nonop nonactive]) then
            1 -> _inquote
        elseif lmember(item, [define procedure function lambda]) then
            _pdcount fi_+ 1 -> _pdcount;
            true -> _invars
        elseif lmember(item, [enddefine endprocedure end]) then
            _pdcount fi_- 1 -> _pdcount;
            if _pdcount fi_<= 0 then
                vedmarkhi();
                return
            endif
        endif
    enduntil;
    vederror('\{b}no matching enddefine')
enddefine;

define vars ved_mcp;
    dlocal vedline, vedcolumn, vvedlinesize;
    ved_mbp();
    vedmarkfind();
    ved_mep()
enddefine;



/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- Aaron Sloman, Jul  6 1988
        Used more sensible method of detecting end of procedure.
        Altered to cope with "nonsyntax define()" etc.
--- John Gibson, Aug 16 1987
        Tidied up
 */
