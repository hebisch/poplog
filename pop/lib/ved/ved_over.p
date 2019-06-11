/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_over.p
 >  Purpose:        Overlay one block of text upon another
 >  Author:         Chris Slymon (from David Roberts), June 1983 (see revisions)
 >  Documentation:  * VED_OVER
 >  Related Files:
 */

/*
 * Overlays one block of text (the 'arrow text') upon another (the 'target
 * text') i.e. each non-space character of the arrow text replaces the
 * character in the equivalent position in the target text;
 *
 * The arrow text is indicated by a marked range, the current line is the
 * first line of the target text. The arrow text cannot overlay itself.
 *
 * The original target text is stored in VVEDDUMP.
 */

compile_mode: pop11 +strict;
section;

define vars ved_over();
    dlocal vedbreak=false, vedstatic=true, vvedmarkhi, vvedmarklo;
    lvars oldchanged, targetline, targethi, arrowstring, arrowline,
        arrowlength;

    vedpositionpush();
    vedchanged -> oldchanged;
    false -> vedchanged;
    (vedline ->> targetline) + vvedmarkhi - vvedmarklo -> targethi;

    if vedmarked(targetline) or vedmarked(targethi) then
        vederror('Cannot overlay text with itself');
    endif;

    vvedmarklo -> arrowline;
    targetline -> vvedmarklo;
    targethi -> vvedmarkhi;
    ved_copy();
    0 -> vvedmarkhi;
    targetline -> vedline;

    until vedline > targethi do
        vedcheck();
        vedbuffer(arrowline) -> arrowstring;
        unless (vedusedsize(arrowstring) ->> arrowlength) = 0 then
            vedsetlinesize();
            1 -> vedcolumn;
            if vvedlinesize = 0 then
                vedinsertstring(arrowstring);
            else
                until vedcolumn > arrowlength do
                    unless fast_subscrs(vedcolumn,arrowstring) == `\s` then
                        vedcharinsert(fast_subscrvedstring(vedcolumn,arrowstring));
                    else
                        vedcharright();
                    endunless;
                enduntil;
            endif;
        endunless;
        arrowline + 1 -> arrowline;
        vedchardown();
    enduntil;

    vedpositionpop();
    if oldchanged then oldchanged + 1 else 1 endif -> vedchanged;

enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Sep  7 1995
        Changed to work with 'vedstrings' (i.e. having associated char data)
--- Adrian Howard, Jun 10 1993
        Tidied, made +strict, now works with dstrings
--- Mark Rubinstein, Oct  4 1985 - sectionised and lvarsed.
 */
