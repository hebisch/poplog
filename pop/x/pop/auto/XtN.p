/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XtN.p
 > Purpose:         Macro for hashed table of fixed address nt strings
 > Author:          Jonathan Meyer, Sep  6 1990 (see revisions)
 > Documentation:   HELP *XtN
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

#_IF not(DEF chatty_XtN)
global vars chatty_XtN = false;     ;;; not chatty by default
#_ENDIF

lconstant strings = newproperty([],200,false,"perm");

define XtNLookup(name,type);
    lvars type name string;
    if strings(name) ->> string then
        string;
    else
        unless name.isword or name.isstring then
            mishap(name, 1,'WORD or STRING NEEDED');
        endunless;
        if isstring(name) then
            /***
                Nothing for it but to dig out the word and try again.
            ***/
            if last(name) == 0 then allbutlast(1, name) else name endif
                -> name;
            fast_chain(consword(name), type, XtNLookup);
        endif;
        if chatty_XtN then
            pr(';;; DECLARING Xt');pr(type);pr(' STRING CONSTANT ');npr(name);
        endif;
        cons_fixed(
            #| name.explode, unless dup() == 0 then 0 endunless |#,
            string_key
        ) ->> strings(name);
    endif;
enddefine;

define macro XtN;
    XtNLookup(readitem(), "N");
enddefine;

define XtNDeclare(list);
    dlocal chatty_XtN = false;
    lvars item list;
    unless list.islist then
        mishap(list,1,'LIST NEEDED');
    endunless;
    for item in list do
        unless item.isword then
            mishap(item,1,'Invalid XtN-string declaration - WORD NEEDED');
        endunless;
        XtNLookup(item,"N")->;
    endfor;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Aug 15 1991
        Added a check so that duplicate 0's are not added to the end of the
        string
--- Jonathan Meyer, Feb 15 1991 Added type check for XtNLookup
--- Jonathan Meyer, Nov 14 1990
        Disallowed "-" character after XtN
--- Jonathan Meyer, Oct 31 1990
        Made XtN use readitem (so it doesn't expand macros).
*/
