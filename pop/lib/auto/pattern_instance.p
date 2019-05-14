/* --- Copyright University of Birmingham 2005. All rights reserved. ------
 > File:            $poplocal/local/auto/pattern_instance.p
 > Purpose:         Version of instance to avoid clashes with objectclass
 > Author:          Aaron Sloman, Jan 18 1997 (see revisions)
 > Documentation:  HELP * PATTERN_INSTANCE
 > Related Files:
 */

/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/lib/auto/instance.p
 >  Purpose:        takes and pattern with ? and ?? and instantiates ALL vars.
 >  Author:         Unknown (Steven Hardy?), ??? (see revisions)
 >  Related Files:  LIB * INSTANCES
 */
compile_mode :pop11 +strict;

;;; INSTANCE takes a pattern with ? or ?? and instantiates ALL the variables
;;; in a copy of the pattern. Used in FOREVERY, etc.

section;

define vars procedure pattern_instance(Pattern);
    lvars XX, undefaction, Pattern;
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
                pattern_instance(XX, undefaction)
            else
                pattern_instance(XX)
            endif
        enduntil %]
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan  1 2005
        Added HELP PATTERN_INSTANCE
--- Aaron Sloman, Jan 18 1997
    renamed as pattern_instance, to prevent clashes, e.g. with 'instance'
    in objectclass.
--- Mark Rubinstein, Apr 15 1986 - made to check if the value of the variable
    is an undef (instead of just the identprops == undef) if the undefaction
    is to be run.
--- Mark Rubinstein, Feb 1985 - Fixed - so that it can take a second optional
    argument see HELP * INSTANCE.
 */
