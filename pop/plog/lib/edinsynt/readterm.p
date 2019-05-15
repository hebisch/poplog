/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/plog/lib/edinsynt/readterm.p
 > Purpose:         Prolog: Read terms in Edinburgh (DEC-10) syntax
 > Author:          Robert Duncan and Simon Nichols, Mar 13 1990 (see revisions)
 > Related Files:   C.all/plog/lib/edinsynt/tokens.p
 >                  C.all/plog/lib/edinsynt/readtoken.p
 */

/*
 * Acknowledgement:
 *      based on "READ.PL" in the Edinburgh DEC-10 Prolog library, written
 *      by D.H.D.Warren & Richard O'Keefe.
 */

section $-prolog => edinburgh_readterm;

constant
    procedure edinburgh_readterm,
;

vars
    saved_tokens,
    saved_stacklength,
    choicepoint,
;

lconstant
    procedure read,
;


;;; fork:
;;;     saves the current state of the parser as a process record in
;;;     -choicepoint-, to enable subsquent backtracking to this point.
;;;     When the process is first invoked, this procedure returns <true>.
;;;     On the second (and final) time, it returns <false>.

define lconstant fork();
    lvars proc;
    if consproc_to(stacklength(), read, false) ->> proc then
        ;;; first time the process is run:
        ;;; save a copy of the process in -choicepoint- and return <true>
        copy(proc) -> choicepoint;
        true;
    else
        ;;; second time the process is run: return <false>
        false;
    endif;
enddefine;


;;; backtrack:
;;;     backtracks to the saved state of the parser represented by the
;;;     process record stored in -choicepoint-.

define lconstant backtrack();
    lvars proc = choicepoint;
    false -> choicepoint;
    kresume(false, 1, proc);
enddefine;


;;; syntax_error:
;;;     has three possible actions:
;;;     1) backtrack to a previous point in the parse;
;;;     2) print an error message and mishap;
;;;     3) print an error message and continue.
;;;     It also invokes -prolog_abort_on_syntax_errors- (if it's a
;;;     procedure) which may exit from the reader immediately (e.g.
;;;     -prolog_vedreadterm- in "plogved.p").

define lconstant syntax_error(message, tokens);
    lvars message, tokens;
    ;;; If there's a choicepoint, backtrack to it
    if choicepoint then backtrack() endif;
    ;;; If -prolog_abort_on_syntax_errors- is a procedure, call it (this
    ;;; may chain out of the reader)
    if isprocedure(prolog_abort_on_syntax_errors) then
        prolog_abort_on_syntax_errors();
    endif;
    ;;; Print the error message
    pr('\n;;; Syntax error: ');
    if isstring(message) then
        pr(message);
    else
        applist(message, spr);
    endif;
    nl(1);
    ;;; Print the list of tokens
    pr(';;; Reading: ');
    until saved_tokens == tokens do
        print_token(Destpair(saved_tokens) -> saved_tokens);
        sp(1);
    enduntil;
    pr('<<here>> ');
    until saved_tokens == [] do
        print_token(Destpair(saved_tokens) -> saved_tokens);
        sp(1);
    enduntil;
    npr(prolog_term_terminator);
    ;;; Print the filename and line number
    if popfilename then
        printf(poplinenum, popfilename, ';;; File: %p, line: %p\n');
    endif;
    ;;; Either cause an interrupt ...
    if prolog_abort_on_syntax_errors then interrupt() endif;
    ;;; ... or re-invoke -edinburgh_readterm-, terminating its current
    ;;; invocation and clearing the user stack
    nl(1);
    erasenum(stacklength() fi_- saved_stacklength);
    chainfrom(edinburgh_readterm, edinburgh_readterm);
enddefine;


;;; nexttoken:
;;;     returns the next token from -tokens-.
;;;     If -tokens- is nil, returns <false>.

define lconstant nexttoken(tokens);
    lvars tokens;
    tokens /== [] and Front(tokens);
enddefine;


;;; expect:
;;;     reads the next token and checks that it is the one expected.
;;;     If it's not, an error is raised.

define lconstant expect(token, tokens);
    lvars token, tokens;
    if tokens /== [] and token == Front(tokens) then
        Back(tokens);
    else
        syntax_error([^token or operator expected], tokens);
    endif;
enddefine;


;;; prefix_is_atom:
;;;     returns <true> when the right context (as represented by the next
;;;     token) of a prefix operator of precedence -oprec- forces it to be
;;;     treated as an atom.

define lconstant prefix_is_atom(token, oprec);
    lvars token, oprec, oprec2;
    if isatom(token) then
        (infix_lprec(token) ->> oprec2) /== NOPREC and oprec2 fi_>= oprec
        or (postfix_lprec(token) ->> oprec2) /== NOPREC and oprec2 fi_>= oprec;
    elseif token == BAR then
        1100 fi_>= oprec;
    elseif token == COMMA then
        1000 fi_>= oprec;
    else
        token == RPAR or token == RBRA or token == RBRACE or not(token);
    endif;
enddefine;


;;; cannot_follow_expression:
;;;     if -token- can't follow an expression, returns a description of the
;;;     offending token. Otherwise it returns <false>.

define lconstant cannot_follow_expression(token);
    lvars token;
    if isatom(token) then
        if infix_prec(token) or postfix_prec(token) then
            false;
        else
            "atom";
        endif;
    elseif isvar(token) then
        "variable";
    elseif isnumber(token) then
        "number";
    elseif fast_lmember(token, [% LPAR, SPACE_LPAR, LBRA, LBRACE %]) then
        "bracket"
    elseif ispair(token) then
        "string"
    else
        false;
    endif;
enddefine;


lconstant
    procedure Read, ;;; Forward declaration
;

;;; read_args:
;;;     parses a (possibly empty) sequence of expressions of precedence 999,
;;;     preceded by commas and terminated by a closing parenthesis.
;;;     Each expression is left on the stack. The unread tokens and the
;;;     number of expressions read are returned.

define lconstant read_args(tokens) -> tokens -> n;
    lvars n = 0, tokens, arg;
    while nexttoken(tokens) == COMMA do
        Read(Back(tokens), 999) -> arg -> tokens;
        arg;    ;;; leave arg on stack
        n fi_+ 1 -> n;
    endwhile;
    if nexttoken(tokens) == RPAR then
        Back(tokens) -> tokens;
    else
        syntax_error(', or ) expected in arguments', tokens);
    endif;
enddefine;


;;; read_list:
;;;     parses a (possibly empty) sequence of expressions of precedence 999,
;;;     preceded by commas and terminated by a closing bracket. The closing
;;;     bracket may optionally be preceded by a BAR token and an additional
;;;     expression. The expressions are returned in a list, together with
;;;     the unread tokens.

define lconstant read_list(tokens) -> list -> tokens;
    lvars n = 0, tokens, item, token, list;
    while nexttoken(tokens) == COMMA do
        Read(Back(tokens), 999) -> item -> tokens;
        item;   ;;; leave item on stack
        n fi_+ 1 -> n;
    endwhile;
    if (nexttoken(tokens) ->> token) == BAR then
        Read(Back(tokens), 999) -> item -> tokens;
        expect(RBRA, tokens) -> tokens;
        item;
    elseif token == RBRA then
        Back(tokens) -> tokens;
        [];
    else
        syntax_error(', | or ) expected in list', tokens);
    endif;
    fast_repeat n times conspair((), ()) endrepeat -> list;
enddefine;


;;; Read:
;;;     parses a token list in a context of given precedence, returning a
;;;     term and the unread tokens.

define lconstant Read(tokens, prec);
    lvars   tokens, token, token2, term, term2, arg1, args, n,
            prec, oprec, oprec2, term_prec = 0;

    if tokens == [] then
        syntax_error('expression expected', []);
    endif;

    ;;; Read a primary
    Destpair(tokens) -> tokens -> token;
    if isatom(token) then
        nexttoken(tokens) -> token2;
        if token2 == LPAR then
            Read(Back(tokens), 999) -> arg1 -> tokens;
            prolog_maketerm(arg1, read_args(tokens) -> tokens -> n, token,
                n fi_+ 1) -> term;
        elseif token == "-" and isnumber(token2) then
            Back(tokens) -> tokens;
            -token2 -> term;
        elseif (prefix_prec(token) ->> oprec) /== NOPREC then
            ;;; prefix operator
            if oprec fi_> prec then
                syntax_error([prefix operator ^token in context with
                    precedence ^prec], tokens);
            elseif prefix_is_atom(token2, oprec) and fork() then
                ;;; treat prefix operator as an atom
                token -> term;
                oprec -> term_prec;
            else
                Read(tokens, prefix_rprec(token)) -> term -> tokens;
                prolog_maketerm(term, token, 1) -> term;
                oprec -> term_prec;
            endif;
        else
            ;;; just an ordinary atom
            token -> term;
        endif;
    elseif token == LBRA then
        if nexttoken(tokens) == RBRA then
            Back(tokens) -> tokens;
            [] -> term;
        else
            Read(tokens, 999) -> arg1 -> tokens;
            read_list(tokens) -> args -> tokens;
            conspair(arg1, args) -> term;
        endif;
    elseif token == LPAR or token == SPACE_LPAR then
        Read(tokens, 1200) -> term -> tokens;
        expect(RPAR, tokens) -> tokens;
    elseif token == LBRACE then
        if nexttoken(tokens) == RBRACE then
            Back(tokens) -> tokens;
            "'{}'" -> term;
        else
            Read(tokens, 1200) -> term -> tokens;
            expect(RBRACE, tokens) -> tokens;
            prolog_maketerm(term, "'{}'", 1) -> term;
        endif;
    elseif isspecial(token) then
        syntax_error([^token cannot start an expression], tokens);
    else
        ;;; var, number, string or other POPLOG data
        token -> term;
    endif;

    ;;; Read the rest of the expression following the primary
    while tokens /== [] do
        Front(tokens) -> token;
        if isatom(token) then
            if (infix_prec(token) ->> oprec) /== NOPREC
            and oprec fi_<= prec
            and term_prec fi_<= infix_lprec(token)
            then
                if (postfix_prec(token) ->> oprec2) /== NOPREC
                and oprec2 fi_<= prec
                and term_prec fi_<= postfix_lprec(token)
                then
                    ;;; ambiguous: infix and postfix
                    if fork() then
                        ;;; try parsing as infix ...
                        Read(Back(tokens), infix_rprec(token))
                            -> term2 -> tokens;
                        prolog_maketerm(term, term2, token, 2) -> term;
                        oprec -> term_prec;
                    else
                        ;;; ... failed: try parsing as postfix
                        Back(tokens) -> tokens;
                        prolog_maketerm(term, token, 1) -> term;
                        oprec2 -> term_prec;
                    endif;
                else
                    ;;; infix
                    Read(Back(tokens), infix_rprec(token)) -> term2 -> tokens;
                    prolog_maketerm(term, term2, token, 2) -> term;
                    oprec -> term_prec;
                endif;
            elseif (postfix_prec(token) ->> oprec) /== NOPREC
            and oprec fi_<= prec
            and term_prec fi_<= postfix_lprec(token)
            then
                ;;; postfix
                Back(tokens) -> tokens;
                prolog_maketerm(term, token, 1) -> term;
                oprec -> term_prec;
            else
                quitloop;
            endif;
        elseif token == COMMA and prec fi_>= 1000 and term_prec fi_< 1000 then
            Read(Back(tokens), 1000) -> term2 -> tokens;
            1000 -> term_prec;
            prolog_maketerm(term, term2, ",", 2) -> term;
        elseif token == BAR and prec fi_>= 1100 and term_prec fi_< 1100 then
            Read(Back(tokens), 1100) -> term2 -> tokens;
            1100 -> term_prec;
            prolog_maketerm(term, term2, ";", 2) -> term;
        else
            quitloop;
        endif;
    endwhile;
    ;;; If there any tokens left over, check that the first of them can
    ;;; legally follow an expression
    if tokens /== [] and (cannot_follow_expression(token) ->> token) then
        syntax_error([^token follows expression], tokens);
    else
        (tokens, term);
    endif;
enddefine;


;;; read:
;;;     parses a token list, and returns a term.

define lconstant read(tokens) -> term;
    lvars tokens, term;
    dlocal
        saved_tokens,
        saved_stacklength = stacklength(),
        choicepoint = false,
    ;
    tokens -> saved_tokens;
    Read(tokens, 1200) -> term -> tokens;
    unless tokens == [] then
        syntax_error('operator expected after expression', tokens)
    endunless;
enddefine;


;;; edinburgh_readterm:
;;;     reads a term from the current input stream.

define edinburgh_readterm();
    lvars tokens;
    returnif(isprocedure(readtokens() ->> tokens))(tokens); ;;; command
    read(tokens);
enddefine;

endsection; /* $-prolog */

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, May 28 1992
        Changed -cannot_follow_expression- to return "variable" rather than
        "var" if -token- is a variable.
--- Simon Nichols, Jun 25 1991
        Added acknowledgement.
--- Simon Nichols, Jul 18 1990
        Changed -edinburgh_readterm- to support new implementation of
        commands: it now checks whether -readtokens- has returned a
        command (which is a procedure).
--- Simon Nichols, Jun 26 1990
        Changed format of error messages.
 */
