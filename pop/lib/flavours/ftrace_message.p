/* --- Copyright University of Sussex 1991.  All rights reserved. ---------
 > File:           C.all/lib/flavours/ftrace_message.p
 > Purpose:        tracing package for tracing flavour methods
 > Author:         Mark Rubinstein, Apr 18 1986 (see revisions)
 > Documentation:  HELP * FLAVOUR_LIBRARY, TEACH * FLAVOURS
 > Related Files:  LIB * FTRACEALL_MESSAGE, * FLAVOURS
 */

section;
section temporary => fsystrace;

vars ftrindent = 0;                     ;;; current trace indentation

define lvars ftrace_pr(props, inoutword, args);
lvars props inoutword args;                   
vars tracing = false;
    repeat ftrindent - 1 times cucharout(`!`) endrepeat;
    printf('%p%p<-%p ', [% inoutword, self, props %]); appdata(args,spr);
    cucharout(`\n`);
enddefine;

define global fsystrace(pdr, props);
vars ftrindent;
lvars pdr props num args;
    ftrindent fi_+ 1 -> ftrindent;
    ;;; pdnargs can give funny values for closures of variadic pdrs
    max(0, pdnargs(pdr)) -> num;
    if stacklength() fi_< num then
        mishap(stacklength(),'TOO FEW ARGUMENTS FOR ' sys_>< pdprops(pdr));
    endif;
    if tracing then
        consvector(num) -> args;
        stacklength() -> num;
        ftrace_pr(props, ">", args);
        explode(args); nil -> args;
    endif;
    pdr();
    if tracing then 
        stacklength() fi_- num -> num;
        if num fi_< 0 then {} else consvector(num) endif -> args;
        ftrace_pr(props, "<", args);
        explode(args)
    endif
enddefine;

section_cancel(current_section, true);
endsection;                             ;;; temporary

flavour metaflavour a metaflavour novanilla;
    defmethod ftrace(methname);
    lvars methname pdr upd = false;
        true -> tracing; 
        if islist(methname) then
            for pdr in methname do self <- ftrace(pdr) endfor;
            return;
        endif;
        unless self <- methodfor(methname) ->> pdr do
            mishap(methname, 1, 'METHOD TO TRACE NOT FOUND')
        endunless;
        unless pdpart(pdr) == fsystrace do
            if updater(pdr) then
                fsystrace(% updater(pdr), methname %) -> upd;
                methname -> pdprops(upd);
            endif;
            fsystrace(% pdr, methname %) -> pdr;
            methname -> pdprops(pdr);
            if upd then upd -> updater(pdr) endif;
            pdr -> self<-methodfor(methname);
        endunless;
    enddefmethod;
    defmethod ftraceall;
        self <- ftrace(self<-methods);
    enddefmethod;
    defmethod unftrace(methname);
    lvars methname pdr upd = false;
        if islist(methname) then
            false -> tracing;
            for pdr in methname do self <- unftrace(pdr) endfor;
            return;
        endif;
        unless self <- methodfor(methname) ->> pdr do
            mishap(methname, 1, 'METHOD TO UNTRACE NOT FOUND')
        endunless;
        if pdpart(pdr) == fsystrace then
            if updater(pdr) then frozval(1, updater(pdr)) -> upd; endif;
            frozval(1, pdr) -> pdr;
            if upd then upd -> updater(pdr) endif;
            pdr -> (self <- methodfor(methname));
        endif;
    enddefmethod;
    defmethod unftraceall;
        self <- unftrace(self<-methods);
    enddefmethod;
endflavour;

endsection;

/* --- Revision History ---------------------------------------------------
--- Andreas Schoter, Jun 27 1991 - updated document and library cross
    references
--- James Goodlet, Jan 22 1990 - corrected grammar in mishap message.
 */
