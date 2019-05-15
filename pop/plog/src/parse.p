/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:        C.all/plog/src/parse.p
 > Purpose:     Prolog: parser for DEC10-like syntax
 > Author:      Robert Duncan & Simon Nichols, February 1987 (see revisions)
 */


section prolog;

constant
    procedure ( items_read, prolog_initbuf, prolog_itemread, prolog_nextitem,
        prolog_readstring, prolog_switch_itemiser, prolog_try_nextitem,
        replace_in_buf, Read_cmdname, Read_cmdarg, prefix_prec, prefix_rprec,
        postfix_prec, postfix_lprec, infix_prec, infix_lprec, infix_rprec, );

weak constant
    procedure ( $-vedsetpop, );

vars
    prolog_expand_macros,
    prolog_lastitem,
;

weak vars
    procedure ( prolog_read_command, prolog_read_cmdname,
        prolog_read_cmdarg, ),
    $-vedinvedprocess,
;

;;; =======================================================================


/* Syntax Errors */

vars
    prolog_abort_on_syntax_errors = DEF prolog_debugging,
;

lvars
    oldstacklength,
;

constant procedure Readterm;    ;;; forward

define vars syntax_error(msg);
    lvars   msg;
    dlocal  pr = syspr, cucharout = cucharerr, pop_pr_quotes = false;

    define lconstant print_location(more, items);
        lvars more, items;

        define dlocal pr(item);
            lvars   item;
            dlocal  pop_pr_quotes = true;
            if item == termin then "end_of_file" -> item endif;
            syspr(item);
        enddefine;

        printf(prolog_lastitem, ';;; FOUND\s\s: %p\n');
        unless items == [] then
            printf(';;; READING: ');
            if more then printf('... ') endif;
            until (Destpair(items) ->> items) == [] do
                spr(/* item */);
            enduntil;
            printf(/* item, */ '<<HERE>> %p\n');
        endunless;
    enddefine;

    define lconstant skipto(t);
        lvars   item = prolog_lastitem, t;
        dlocal  popnewline = true, prolog_expand_macros = false;
        until item == t or item == termin do
            prolog_getitem() -> item;
        enduntil;
    enddefine;

    if isprocedure(prolog_abort_on_syntax_errors) then
        prolog_abort_on_syntax_errors();
    endif;

    printf(msg, '\n;;; PROLOG SYNTAX ERROR - %p\n');
    print_location(items_read());
    if popfilename then
        printf(poplinenum, popfilename,
            ';;; FILE\s\s\s: %p\s\s\s\s\sLINE NUMBER: %p\n');
    endif;

    if prolog_abort_on_syntax_errors
    or VEDLOADED and VEDWEAK vedinvedprocess
        and not(iscaller(VEDWEAK vedsetpop))
    then
        ;;; abort after the first error
        interrupt();
    endif;

    ;;; otherwise carry on after the next newline or dot
    skipto(if cucharin == charin then newline else "." endif);
    nl(1);
    ;;; Re-invoke -Readterm-, terminating its current invocation and
    ;;; clearing the user stack.
    erasenum(stacklength() fi_- oldstacklength);
    chainfrom(Readterm, Readterm);
enddefine;


/* Variable Environment */

vars
    readenv,
        ;;; a list of name/variable bindings of the form: (Name=Var)
;

;;; getvar:
;;;     looks up a variable name in -readenv-

define getvar(name) -> var;
    lvars entry, name, var;
    unless name == "_" do
        For entry in readenv do
            returnif(fast_prolog_arg(1, entry) == name)
                (fast_prolog_arg(2, entry) -> var);
        endfor;
    endunless;
    prolog_newvar() -> var;
    conspair(consprologterm(name, var, "=", 2), readenv) -> readenv;
enddefine;


/* Reading a Term */

vars prolog_term_terminator = ".";

normal_compile;

define lconstant mkassoc(list);
    lvars list, i;
    newproperty([% for i in list do [^i ^true] endfor %], 16, false, true);
enddefine;

end_normal_compile;

lconstant procedure (
    illegal_start_item =
        ;;; items which cannot start a term
        mkassoc([% ")", "]", "}", "|", "," %]),
    illegal_after_prefix_op =
        ;;; as above, plus "." to allow "spy." etc. for upward compatability
        mkassoc([% ")", "]", "}", "|", ",", "." %]),
    punctuation_in_term =
        mkassoc([% "(", ")", "[", "]", "{", "}", "|", "." %]),
    punctuation_in_argument =
        mkassoc([% ",", "(", ")", "[", "]", "{", "}", "|" %]),
    punctuation_in_bracketedterm =
        mkassoc([% "(", ")", "[", "]", "{", "}", "|" %])
);

lvars procedure (
    punctuation = punctuation_in_term,
);

define lconstant readterm1(prevprec) -> term;
    lvars item, term, c, prevprec, prec = 0, list, looked_ahead = false;

    define lconstant readfunctor(fn);
        lvars   arity = 0, fn;
        dlocal  punctuation = punctuation_in_argument;
        repeat
            readterm1(MAXPREC);
            arity fi_+ 1 -> arity;
        quitunless(prolog_lastitem == ",");
        endrepeat;
        unless prolog_lastitem == ")" then
            syntax_error('OPERATOR, \',\', OR \')\' EXPECTED');
        endunless;
        prolog_maketerm(fn, arity);
    enddefine;

    define lconstant readargument() with_nargs 1;
        dlocal punctuation = punctuation_in_argument;
        readterm1();
    enddefine;

    define lconstant readbracketedterm() with_nargs 1;
        dlocal punctuation = punctuation_in_bracketedterm;
        readterm1();
    enddefine;

    prolog_itemread() -> item;
    if isword(item) then
        if item == "(" then
            readbracketedterm(MAXPREC) -> term;
            unless prolog_lastitem == ")" then
                syntax_error('OPERATOR OR \')\' EXPECTED');
            endunless;
        elseif item == "[" then
            if prolog_try_nextitem("]") then
                [] -> term;
            else
                [%
                    repeat
                        readargument(MAXPREC);
                    quitif (prolog_lastitem /== ","
                    or  prolog_try_nextitem("'..'")
                    and ("|" ->> prolog_lastitem));
                    endrepeat;
                %] -> term;
                if prolog_lastitem == "|" then
                    term -> list;
                    until Back(list) == [] do Back(list) -> list enduntil;
                    readargument(MAXPREC) -> Back(list);
                    unless prolog_lastitem == "]" then
                        syntax_error('OPERATOR OR \']\' EXPECTED IN LIST');
                    endunless;
                elseif prolog_lastitem /== "]" then
                    syntax_error('OPERATOR, \',\', \'|\', OR \']\' EXPECTED IN LIST');
                endif;
            endif;
        elseif item == """ then
            prolog_readstring() -> term;
        elseif item == "{" then
            if prolog_try_nextitem("}") then
                "'{}'" -> term;
            else
                consprologterm(readbracketedterm(MAXPREC), "'{}'", 1) -> term;
                unless prolog_lastitem == "}" then
                    syntax_error('OPERATOR OR \'}\' EXPECTED');
                endunless;
            endif;
        elseif illegal_start_item(item) then
            syntax_error('EXPECTED THE START OF A TERM');
        elseif isuppercode(fast_subscrw(1, item) ->> c) or c == `_` then
            getvar(item) -> term;
        elseif prefix_prec(item) fi_< MAXPREC then
            prefix_prec(item) -> prec;
            if prec fi_> prevprec then
                syntax_error('PRECEDENCE OF PREFIX OPERATOR TOO HIGH');
            elseif illegal_after_prefix_op(prolog_nextitem()) then
                ;;; Return the prefix operator as an atom with the
                ;;; precedence of the operator.
                item -> term;
            else
                prolog_maketerm(readterm1(prefix_rprec(item)), item, 1) -> term;
                true -> looked_ahead;
            endif;
        elseif prolog_try_nextitem("(") then
            readfunctor(item) -> term;
        else
            item -> term;
        endif;
    elseif isstring(item) then
        consword(item) -> term;
        if term == "'[]'" then
            [] -> term;
        elseif prolog_try_nextitem("(") then
            readfunctor(term) -> term;
        endif;
    elseif item == termin then
        syntax_error('EXPECTED THE START OF A TERM');
    else
        item -> term;
    endif;

    unless looked_ahead then prolog_itemread() -> endunless;

    repeat
        ;;; We're expecting an operator: if we get a negative number,
        ;;; transform it into the infix operator "-" followed by its
        ;;; absolute value.
        if isnumber(prolog_lastitem ->> item) and item < 0
        and infix_prec("-") fi_< MAXPREC
        then
            -item :: proglist -> proglist;
            replace_in_buf("-" ->> prolog_lastitem ->> item);
        endif;
        if not(isword(item)) or punctuation(item)
        or isuppercode(Subscrw(1, item) ->> c) or c == `_`
        then
            quitloop;
        elseif infix_prec(item) fi_<= prevprec
        and prec fi_<= infix_lprec(item)
        then
            infix_prec(item) -> prec;
            prolog_maketerm(term, readterm1(infix_rprec(item)), item, 2)
                -> term;
        elseif postfix_prec(item) fi_<= prevprec
        and prec fi_<= postfix_lprec(item)
        then
            postfix_prec(item) -> prec;
            prolog_maketerm(term, item, 1) -> term;
            prolog_itemread() -> ;
        else
            quitloop;
        endif;
    endrepeat;
enddefine;

;;; Readterm, readterm:
;;;     reads a Prolog term, terminated by -prolog_term_terminator-.
;;;     $-prolog$-readterm can be redefined by users.

define Readterm();
    lvars cmd, end_of_file;
    dlocal
        weakref prolog_read_cmdname = Read_cmdname,
        weakref prolog_read_cmdarg  = Read_cmdarg,
        oldstacklength = stacklength(),
        prolog_term_terminator,
    ;
    [] -> readenv;
    if testdef prolog_read_command and weakref prolog_read_command() ->> cmd
    then
        return(cmd);
    endif;
    prolog_try_nextitem(termin) -> end_of_file;
    [] -> readenv;  ;;; again, in case it's been side-effected by a macro call
    returnif(end_of_file)("end_of_file");
    prolog_initbuf(false);
    readterm1(MAXPREC);
    unless prolog_lastitem == prolog_term_terminator then
        if prolog_term_terminator == termin then
            syntax_error('EXPECTED \'end_of_file\' AT END OF CLAUSE');
        else
            syntax_error('EXPECTED \'' sys_>< prolog_term_terminator sys_><
                '\' AT END OF CLAUSE');
        endif;
    endunless;
enddefine;

vars procedure readterm = Readterm;


;;; prolog_readterm, prolog_readterm_to:
;;;     allow prolog terms to be read safely from POP11.

define prolog_readterm();
    dlocal
        prolog_abort_on_syntax_errors,
        3 %prolog_switch_itemiser()%,           ;;; ensure Prolog itemisation
        weakref prolog_read_command = identfn(%false%), ;;; ignore commands
    ;
    unless prolog_abort_on_syntax_errors then
        true -> prolog_abort_on_syntax_errors;
    endunless;
    readterm();
enddefine;

define prolog_readterm_to(prolog_term_terminator);
    dlocal prolog_term_terminator;
    prolog_readterm();
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, May 10 1993
        Moved read*clause out to "prolog_compile.p".
--- Robert Duncan, Apr 26 1993
        weakrefed commands and term-expansion interfaces
--- Robert John Duncan, May 15 1992
        Made compileable in a system without VED.
--- Simon Nichols, Jan 21 1992
        Changed uses of -Arg- to -fast_prolog_arg-.
--- Simon Nichols, Jun 25 1991
        Changed -syntax_error- to abort after the first error if compiling
        from VED, whatever the value of -prolog_abort_on_syntax_errors-.
--- Simon Nichols, Jul 26 1990
        Changed -syntax_error- to call -prolog_abort_on_syntax_errors- if
        it is a procedure.
--- Simon Nichols, Jul 19 1990
        Changed prolog_expand_clause/2 to expand_term/2, for
        compatibility with other Prolog systems.
--- Simon Nichols, Jul 17 1990
        Changed -Readterm- and -prolog_readterm- to support the new
        implementation of commands.
--- Simon Nichols, Apr 24 1990
        Changed -readclause- to call prolog_expand_clause/2 on each clause
        read, to support clause pre-processing. This subsumes the mechanism
        for grammar rule expansion: consequently, the code for handling
        grammar rules has been removed.
--- Simon Nichols, Feb  6 1990
    - renamed -readterm- to -Readterm- and made -readterm- a variable whose
        initial value is -Readterm-, in order to provide a hook for an
        alternative reader;
    - changed -Readterm- to prevent it discarding initial full stops;
    - added a global variable -prolog_term_terminator- (not exported from
        section $-prolog) which -Readterm- expects to terminate a term
        (instead of full stop as previously);
    - rewrote -prolog_readterm- and -prolog_readterm_to- to use -readterm-;
    - made -readclause- check the value of -prolog_expand_macros- before
        checking the first item to see if its a macro.
--- Rob Duncan, Aug  8 1989
    - sectionised and added #_INCLUDEs for POPC;
    - removed -resetvarnumbers- and -reset_env-;
    - renamed global variable -env- to be -readenv-; changed the format
        of entries to the more conventional (Name = Var);
    - simplified printing in -syntax_error- to use new -items_read-
        procedure from "itemise.p";
    - changed the format of syntax error messages;
    - moved in declarations of operator tables from "operators.p";
    - changed macro handling in -readclause- so to use -prolog_macro-
        in place of -valof-;
    - tidied up.
--- John Gibson, Jun 18 1989
        Added -writeable- for -grammargoal- in -readclause-.
--- Rob Duncan, Mar 16 1988
    Renamed from plogparse.p
--- Simon Nichols, Nov  3 1987
    Changed -get_identifier- to -word_identifier-.
 */
