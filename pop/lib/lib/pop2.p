/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:           C.all/lib/lib/pop2.p
 > Purpose:        library of macros for running POP-2 code
 > Author:         Mark Rubinstein, Oct 29 1985 (see revisions)
 > Documentation:  HELP * POP2
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING


section;

/* Macros & Syntax Words */

;;; comment:            comment <text> ;
;;; function:           function <name> <parameters> <body> end
;;; lambda:             lambda <parameters> <body> end
;;; loopif:             loopif <test> do <actions> enddo
;;; enddo:              do <actions> enddo
;;; exit:               if <test> then <actions> exit
;;; forall              forall <var> <lo> <step> <hi> [<do>] <actions> close
;;; switch:             <expression> SWITCH <sequence of labels>
;;; goon:               goon

syssynonym("function",      "define");
syssynonym("lambda",        "procedure");
syssynonym("loopif",        "while");

global constant syntax (close, end);

global vars macro (
    comment,
    enddo       =   "close",
    exit        =   [; return close],
    forall,
    goon,
    switch      =   [; go_on to],
    );


define global macro comment;
    lvars item;
    dlocal pop_longstrings = true;
    until (readitem() ->> item) == ";" or item == termin do
        /* nothing */
    enduntil
enddefine;


define macro forall Var Lo Step Hi;
    lvars Hi Lo Step Var;
    "for", Var, "from", Lo, "by", Step, "to", Hi;
    unless nextitem() == "do" then "do" endunless;
enddefine;


define macro goon;
    lvars x, n = 1, m = 0;
    if cucharin /== charin then return(termin) endif;
    while caller(n) ->> x do
        if x == pop11_comp_stream then m + 1 -> m endif;
        n + 1 -> n;
    endwhile;
    if m > 1 then termin endif;
enddefine;


"function"  :: vedopeners -> vedopeners;
"lambda"    :: vedopeners -> vedopeners;
"loopif"    :: vedopeners -> vedopeners;
"close"     :: vedclosers -> vedclosers;
"enddo"     :: vedclosers -> vedclosers;
"exit"      :: vedclosers -> vedclosers;
"end"       :: vedclosers -> vedclosers;

/* Procedures */

uses pdcomp;
syssynonym("fncomp",        "pdcomp");
syssynonym("fnpart",        "pdpart");
syssynonym("fnprops",       "pdprops");

syssynonym("init",          "initv");
syssynonym("macresults",    "dl");

global vars procedure (
    equal           =   nonop =,
    meaning         =   newproperty([], 101, undef, true),
    print,
    recordfns,
    );


define print(item) -> item;
    lvars item;
    pr(item)
enddefine;


define recordfns(word, spec);
    lvars key spec word;
    if isinteger(spec) then
        [% repeat spec times "full" endrepeat %] -> spec
    endif;
    conskey(word, spec) -> key;
    class_cons(key);
    class_dest(key);
    explode(key);           ;;; stacks -class_access- procedures
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 11 1996
        Added syntax declaration for "close" and "end".
--- John Gibson, Aug 13 1989
        Replaced sys compile with pop11_comp_stream
--- Aaron Sloman, Apr 15 1989
        Put "end" on vedclosers
--- John Williams, Mar 31 1989
        Tidied; now adds syntax words to -vedopeners- and -vedclosers-
--- Aaron Sloman, Jul  9 1986 fixed definition of "enddo" - should not have
    used syssynonym
--- John Williams, Jun  9 1986 - moved the 'goon' library in here
--- Mark Rubinstein, Apr 18 1986 - added the -meaning- library.
*/
