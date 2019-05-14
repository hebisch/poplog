/*  --- Copyright University of Sussex 2005.  All rights reserved. ---------
 >  File:           C.all/lib/auto/instance.p
 >  Purpose:        takes and pattern with ? and ?? and instantiates ALL vars.
 >  Author:         Unknown (Steven Hardy?), ??? (see revisions)
 >  Documentation:  HELP * INSTANCE
 >  Related Files:  LIB * INSTANCES
 */
compile_mode :pop11 +strict;

;;; NOTE: this procedure clashes with uses of 'instance' in Objectclass,
;;; and is therefore replaced by pattern_instance

;;; INSTANCE takes a pattern with ? or ?? and instantiates ALL the variables
;;; in a copy of the pattern. .

section;

define vars instance(Pattern);
lvars XX undefaction Pattern;
    if Pattern.isprocedure then
        Pattern -> undefaction;
        -> Pattern
    else
        false -> undefaction;
    endif;

    [%  until null(Pattern) do
            fast_destpair(Pattern) -> Pattern -> XX;
            if XX = "?" then
                destpair(Pattern) -> Pattern -> XX;
                if undefaction
                and (identprops(XX) == undef or isundef(valof(XX))) then
                    undefaction("?", XX)
                else
                    valof(XX);
                endif;
chop:
                if ispair(Pattern) and fast_front(Pattern) == ":" then
                    back(fast_back(Pattern)) -> Pattern
                endif
            elseif XX == "??" then
                destpair(Pattern) -> Pattern -> XX;
                if undefaction
                and (identprops(XX) == undef or not(XX.valof.ispair)) then
                    undefaction("??", XX)
                else
                    dl(valof(XX));
                endif;
                goto chop
            elseif atom(XX) then
                XX
            elseif undefaction then
                instance(XX, undefaction)
            else
                instance(XX)
            endif
        enduntil %]
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan 13 2005
        Now deprecated, because of clash with 'instance' in objectclass.
        Use pattern_instance instead.
--- Mark Rubinstein, Apr 15 1986 - made to check if the value of the variable
    is an undef (instead of just the identprops == undef) if the undefaction
    is to be run.
--- Mark Rubinstein, Feb 1985 - Fixed - so that it can take a second optional
    argument see HELP * INSTANCE.
 */
