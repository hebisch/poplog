/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/with_index.p
 > Purpose:
 > Author:          Ian Rogers, Mar 26 1990 (see revisions)
 > Documentation:
 > Related Files:   C.all/lib/auto/define_with_index_hook.p
 */
compile_mode :pop11 +strict;

/*

    for <vars> with_index <var> <vectorclass_for_form> <exprs> do
        <body>
    endfor;

*/

section;

define lconstant get_for_subsyntax(name) -> forp;
    lvars name, forp;
    unless is_sub_syntax_word(name, "for") ->> forp then
        mishap(name, 1, 'EXPECTING A for FORM')
    endunless
enddefine;

define :for_extension global with_index(vlist, isfast);
    lvars   vlist, isfast, ivar = itemread(), ftype = itemread(),
            forp = get_for_subsyntax(ftype);
    if updater(forp) ->> forp then
        forp(vlist, isfast, ivar)
    else
        mishap(ftype, 1, 'with_index NOT APPROPRIATE FOR THIS for FORM');
    endif;
enddefine;

define :define_form global with_index_hook;
    lvars ftype = readitem(), p;
    get_for_subsyntax(ftype) -> ;       ;;; just to check it
    pop11_comp_procedure([enddefine], false, false) -> p;
    ;;; put the index hook procedure in the updater of the for form
    ;;; (must do it with compiled code so it works correctly in POPC)
    [ #_< ^p -> updater(back(nonsyntax ^ftype)) >_# ^^proglist] -> proglist
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 17 1992
        Rewritten to store the index procedure in the updater of the
        for form procedure rather than using a property (means (a) the
        index hook is scoped properly, and (b) everything works with POPC).
--- Adrian Howard, Aug  2 1991 : Changed to use -newproperty- rather than
        -newassoc-, saves an autoload.
 */
