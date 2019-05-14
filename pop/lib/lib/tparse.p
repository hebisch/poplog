/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 >  File:           C.all/lib/lib/tparse.p
 >  Purpose:        Demonstration showing the use of processes in a parser.
 >  Author:         John Gibson and A.Sloman 1983 (see revisions)
 >  Documentation:  HELP * TPARSE
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
To run this do something like
    showparses("s", [the man ate the cat and the mouse]);

This prints out all the ways an S can be found as an initial segment
of the list of words.

    listparses("s", [the man put the cat on the book in the tree]) ==>

This makes a list of all ways of parsing the complete list as an "s".

    listallparses("s", [the man put the cat on the book in the tree]) ==>
This makes a list of all ways of parsing an initial segment of the
list as an "s".

    appparses("s", list, proc);
for every initial segment of list which is an "s", this applies proc to
the parse-tree and the list of unused words

*/

section;

;;; globals
lvars myproc, max_recursion_level,
    procedure(grammar, lexicon, parse, inside);


define lconstant procedure set_assoc(list) -> prop;
    newproperty([], 20, false, true) -> prop;
    applist(list,
        procedure(list); lvars list;
            back(list) -> prop(fast_front(list))
        endprocedure)
enddefine;


define global setup(gram, lex);
    if islist(gram) then set_assoc(gram) -> grammar
    else
        mishap('LIST NEEDED FOR GRAMMAR', [^gram])
    endif;
    if islist(lex) then set_assoc(lex) -> lexicon
    else
        mishap('LIST NEEDED FOR GRAMMAR', [^lex])
    endif;
enddefine;


define lconstant procedure tryrule(rule,wordlist,tree);
    ;;; run in a process, which when it suspends leaves itself,
    ;;; remaining words and parse tree so far, on stack
    lvars symbol, getparse, subtree;
    if listlength(rule) fi_<= listlength(wordlist) then
        destpair(rule) -> rule -> symbol;
        parse(symbol, wordlist) -> subtree;
        while  subtree do
                -> wordlist -> getparse;
            if rule == nil then
                suspend(myproc, wordlist, rev(subtree::tree), 3);
            else
                tryrule(rule, wordlist, subtree::tree);
            endif;
            if getparse then getparse(0) else false endif -> subtree;
        endwhile;
    endif
enddefine;


define lvars procedure parse(symbol,wordlist);
    ;;; returns a continuation or false, the list of unused words, the parse tree
    ;;; or else returns false
    lvars rule, rulelist, set, levels;
    dlocal myproc;
    if wordlist == nil then false
    elseif (grammar(symbol) ->> rulelist) then
        inside(symbol) -> levels;
        if levels fi_<= max_recursion_level then
            levels fi_+ 1 -> inside(symbol);
            consproc_to(0, parse) -> myproc;
            fast_for rule in rulelist do
               tryrule(rule, wordlist, [^symbol])
            endfast_for;
            levels -> inside(symbol);
        endif;
        false;
    elseif (lexicon(symbol) ->> set) and member(front(wordlist),set) then
        false, fast_back(wordlist), [%symbol,fast_front(wordlist)%];
    elseif symbol == fast_front(wordlist) then
        false, fast_back(wordlist), symbol;
    else false
    endif;
enddefine;


define global procedure appparses(symbol,wordlist,func);
    ;;; apply func to the parse tree and the list of unused words, for every
    ;;; way of parsing part of wordlist as the symbol
    lvars tree, proc, procedure func;
    dlocal max_recursion_level, inside,grammar,lexicon;

    listlength(wordlist) + 1 -> max_recursion_level;
    newproperty(nil,20,0,true) -> inside;
    if parse(symbol,wordlist)->>tree then
            ->wordlist -> proc;
        func(tree,wordlist);
        repeat
            if runproc(0,proc) ->>tree then
                    ->wordlist -> proc;
                func(tree,wordlist)
            else quitloop()
            endif
        endrepeat;
    endif
enddefine;


define global procedure showparses(symbol,wordlist);
    appparses(symbol,wordlist,
        procedure(tree,wordlist); tree ==> [unused ^wordlist] ==> endprocedure
        );
    [no more parses] =>
enddefine;


define global procedure listparses(symbol,wordlist);
    [%
      appparses(symbol,wordlist,
        procedure(tree,wordlist); if null(wordlist) then tree endif endprocedure
        )
    %]
enddefine;


define global procedure listallparses(symbol,wordlist);
    [%
      appparses(symbol,wordlist,
        procedure(tree,wordlist); [^tree ^wordlist] endprocedure
        )
    %]
enddefine;


set_assoc([
    [s  [np vp]   ]
    [vp [v np] [v np prep np]    ]
    [np [pn] [det noun] [np and np] [np prep np]]
]) -> grammar;


set_assoc([
    [noun   cat mouse man girl boy book tree]
    [pn     fred aaron steve john sharon]
    [v      liked killed thanked bought ate put]
    [prep   on over in at under]
    [det    each every the a some]
]) -> lexicon;


define global macro ---;
    dlocal vedediting;
    false -> vedediting;
    appparses("s", readline(),
        procedure(tree,wordlist);
            lvars tree,wordlist;
            if null(wordlist) then tree ==> endif
        endprocedure)
enddefine;


global vars tparse = true;  ;;; for uses


endsection;



/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 29 1996
        Increased max_recursion_level as suggested in BR aarons.128
--- John Gibson, Sep 15 1989
        Replaced old -c*onsprocto- with new -consproc_to-.
--- Aaron Sloman, Jan 27 1987
    Tidied up a bit. Put in lvars
*/
