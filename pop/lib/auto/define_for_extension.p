/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/auto/define_for_extension.p
 > Purpose:         For defining extensions to the iteration mechanism
 > Author:          Ian Rogers, Apr 14 1989 (see revisions)
 > Documentation:   HELP * FOR_FORM
 > Related Files:
 */

/***
E.g.

define :for_extension in_property(varlist, isfast);
enddefine;

***/

section;

define :define_form global for_extension;
    [:sub_syntax for] <> proglist -> proglist;
    nonsyntax define()
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  6 1989
        Now uses define:sub_syntax ...
 */
