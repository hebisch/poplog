/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/tags.p
 > Purpose:         Common Lisp TAGBODY construct
 > Author:          John Williams, Feb 25 1986 (see revisions)
 > Documentation:   CLtL, p130-133
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;


define lconstant Istag(form);
    issymbol(form) or isintegral(form)
enddefine;


define lconstant Taglabel(form);
    if issymbol(form) then
        symbol_name(form)
    else
        form sys_>< nullstring
    endif
enddefine;


define lconstant Find_tag_pair(tag);
    lvars tags;
    Tags -> tags;
    while ispair(tags) do
        if sys_=(fast_destpair(tags) -> tags, tag) then
            return(fast_front(tags))
        endif;
        fast_back(tags) -> tags
    endwhile;
    if tags == [] then
        program_error('Cannot GO to ~S', [^tag])
    else
        program_error('Not inside a tagbody', [])
    endif
enddefine;


define Tagbody(forms, nresults);
    lvars stkinfo, form;
    dlocal Tags = Tags or [];
    lispSET_STKLEN(0, false) -> stkinfo;
    ;;; first find all tags, and create associated labels
    for form in forms do
        if Istag(form) then
            acons(form, conspair(Taglabel(form), stkinfo), Tags) -> Tags
        elseunless ispair(form) do
            warn('Strange form in tagbody', [^form]) ->
        endif
    endfor;
    ;;; now compile the forms
    fast_for form in forms do
        if Istag(form) then
            sysLABEL(fast_front(Find_tag_pair(form)))
        else
            compile_form(form, 0)
        endif
    endfast_for;
    lispPUSHNQ(nil, nresults)
enddefine;


define Go(tag, nresults);
    unless islistlength(tag, 1)
    and Istag(fast_front(tag) ->> tag) do
        mishap(0, 'GO expects one argument (a tag)')
    endunless;
    fast_destpair(Find_tag_pair(tag)) -> nresults -> tag;
    lispRESET_STKLEN_IF_NEEDED(nresults);
    sysGOTO(tag);
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
 */
