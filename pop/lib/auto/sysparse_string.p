/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/sysparse_string.p
 >  Purpose:        parse a string into a lst of strings and numbers
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  REF *STRINGS
 */
compile_mode :pop11 +strict;

section;

define global sysparse_string(string);
    lvars string, try_strnumber = true;
    if isboolean(string) then
        (), string -> (string, try_strnumber)
    endif;
    [% sys_parse_string(string, if try_strnumber then
                                    procedure(substr);
                                        lvars substr;
                                        strnumber(substr) or substr
                                    endprocedure
                                endif) %]
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Oct 21 1992
        Rewritten to use sys_parse_string
--- John Gibson, Feb 15 1992
        Gave an optional boolean arg to say don't try strnumber.
--- Aaron Sloman, Oct 22 1985 -
        Made substantially faster, needs documenting.
--- Mark Rubinstein, Sep 27 1985
        lvarsed and sectioned.
 */
