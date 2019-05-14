/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/auto/match_wordswith.p
 > Purpose:         Make a list of words in dictionary satisfying a condition
 > Author:          Jonathan Meyer, Oct 10 1992
 > Documentation:   HELP * WORDSWITH
 > Related Files:   LIB * WORDSWITH, LIB * VED_WORDSWITH SRC * REGEXP_COMPILE.P
 */

/*
match_wordswith(STRING, P) -> LIST

    Returns a sorted list of all words W such that P(S,W).

match_wordswith(STRING) -> LIST
match_wordswith(STRING, WILDCARDS) -> LIST

    Returns a list of all the words that match STRING, which is a
    regular expression - see REF *REGEXP. The optional WILDCARDS is either:

        -false-
            wildcard patterns should be ignored.

        -true-
            @ style wildcards are used (this is the default).

        "ed"
            ed style wildcards are used.

    See *VEDWILDCARDS.
*/

section;
compile_mode :pop11 +strict;

define global match_wordswith(string) -> list;
    lvars string, error, list, flags = 0, escc = `@`, wildcards = true;
    dlvars procedure (test, regexp);

    define lconstant Regexp_test(string, word);
        lvars string, word;
        fast_apply(1, word, false, false, regexp) ->
    enddefine;

    if string.isprocedure then
        string -> (string, test);
    else
        if string.isboolean or string == "ed" then
            string -> (string, wildcards);
        endif;

        if wildcards == "ed" then
            `\\` -> escc;
            flags || 2:1e5 -> flags;
        elseif wildcards then
            `@` -> escc;
        else
            false -> escc;
        endif;

        ;;; compile the expression.
        if regexp_compile(string, flags, false, escc) -> regexp ->> error then
            mishap(string, 1,
                'CANNOT COMPILE REGULAR EXPRESSION (' sys_>< error sys_>< ')');
        endif;
        Regexp_test -> test;
    endif;

    ;;; make a list of words matching the string, and sort it
    sort([%appdic(
            procedure(word);
                lvars word;
                if test(string,word) then
                    word
                endif
            endprocedure)%]) -> list;
enddefine;

endsection;
