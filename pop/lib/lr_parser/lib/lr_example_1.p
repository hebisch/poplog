/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lr_parser/lib/lr_example_1.p
 > Purpose:         LALR(1) Parser Generator: example (1)
 > Author:          Robert John Duncan, Nov 27 1992
 > Documentation:   HELP * DEFINE_PARSER, REF * VMCODE
 > Related Files:   LIB * DEFINE_PARSER, * LR_EXAMPLE_2
 */

/*  This library defines a complete compiler for a tiny imperative
    language, using the parser generator and the Poplog Virtual Machine.
    The following examples are typical of the kind of programs which can
    be written in this language; they can be marked and loaded once the
    library has been compiled. To see the code that's generated, use
    LIB * SHOWCODE.

        compiler(cucharin);     /* 1 */
            /* Factorial */
            var n, m;
            n := read();
            m := 1;
            while n > 1 do
                m := m * n;
                n := n - 1;
            endwhile;
            write(m);

        compiler(cucharin);     /* 2 */
            /* Computes the N'th power of the integers 1 ... 10 */
            var N, i;
            N := read();
            i := 1;
            while i <= 10 do
                var x, n, res;
                x := i; n := N; res := 1;
                while n > 0 do
                    if n % 2 = 1 then
                        res := res * x;
                    endif;
                    x := x * x;
                    n := n / 2;
                endwhile;
                write(res);
                i := i + 1;
            endwhile;

 */

section;

uses lr_parser;
uses lr_state;

;;; compiler(input):
;;;     compiler for a tiny imperative language. The -input- argument
;;;     may be a character repeater, a file name or a list of items.

define compiler(input);
    lvars input;

    ;;; the real work is done by this local procedure: it's needed
    ;;; because all VM code-planting must be done inside a call of
    ;;; -sysCOMPILE-

    define:parser lconstant read_and_execute(input);

    /*
     *  OPTIONS Part
     */

    options
        input = readitem
            ;;; uses standard Poplog itemisation
        results = 0
            ;;; most rules plant code, so return no results
        parser
            ;;; needed for error reporting
    endoptions

    /*
     *  DEFINITIONS Part
     */

    definitions

        dlocal popprompt = '> ';
            ;;; for interactive testing
        dlocal proglist_state = proglist_new_state(input);
            ;;; initialise the itemiser from the input
        dlocal % item_chartype(`_`) % = item_chartype(`a`);
            ;;; forbid mixed alphabetic/symbolic identifiers

        lvars env = [];
            ;;; keeps the names of all variables declared so far;
            ;;; organised as a stack of lists, one for each lexical block

        define lconstant isidentifier(item);
            ;;; an identifier is a word starting with an alphabetic
            ;;; character
            lvars item;
            isword(item) and length(item) > 0 and item_chartype(item(1)) == 1;
        enddefine;

        define lconstant error(msg, items);
            ;;; prints a message on the error output and aborts
            lvars   msg, items;
            dlocal  cucharout = cucharerr;
            printf('\nError - %s\n', msg :: items);
            interrupt();
        enddefine;

        define dlocal lr_parse_error(item, token_n, state);
            ;;; redefines the default error action
            lvars item, token_n, state;

            lconstant
                ;;; token classes
                closers = [endif endwhile then do ) ; , ^termin],
                operators = [or and = /= >= > <= < + - * / %"%"% INTEGER];

            define lconstant closer(state) -> token;
                ;;; identifies some specific token "expected" in this
                ;;; state
                lvars state, token, tokens;
                lr_state_tokens(state, parser) -> (tokens, /*default*/);
                ;;; look for a closer
                for token in tokens do
                    returnif(member(token, closers));
                endfor;
                unless length(tokens) == 1 then
                    ;;; more than one token, and no closers among them
                    false -> token;
                endunless;
            enddefine;

            define lconstant operator(state);
                ;;; tests if all tokens valid in this state are operators
                lvars state, token, tokens;
                lr_state_tokens(state, parser) -> (tokens, /*default*/);
                for token in tokens do
                    returnunless(member(token, operators))(false);
                endfor;
                true;
            enddefine;

            define lconstant expression(state);
                ;;; tests if <expression> is a valid symbol in this state
                lvars state;
                member("expression", lr_state_symbols(state, parser));
            enddefine;

            lvars token;
            if token_n < 0 then
                ;;; item doesn't correspond to any token
                error('illegal input: %p', [^item]);
            elseif closer(state) ->> token then
                ;;; specific token expected
                error('expecting "%p" but found:  %p', [^token ^item]);
            elseif operator(state) then
                ;;; looking for an operator
                error('expecting an operator but found:  %p', [^item]);
            elseif expression(state) then
                ;;; looking for an expression
                error('expecting an <expression> but found:  %p', [^item]);
            else
                ;;; don't know what to say
                error('unexpected input: %p', [^item]);
            endif;
        enddefine;

    enddefinitions

    /*
     *  TOKENS Part
     */

    tokens

        'do' 'else' 'elseif' 'endif' 'endwhile' 'false'
        'if' 'read' 'then' 'true' 'var' 'while' 'write'

        '(' ')' ':=' ',' ';'

        9 'or'
        8 'and'
        7 ( '=' '/=' )
        6 ( '>=' '>' '<=' '<' )
        5 ( '+' '-' )
        4 ( '*' '/' '%' )

        IDENTIFIER  : isidentifier,
        5 INTEGER   : isintegral,
            ;;; NB: having a precedence for INTEGER is unusual -- see
            ;;; the rules for <expression> below

    endtokens

    /*
     *  RULES Part
     */

    rules

        program ::=
            block
                {   ;;; finished: execute any code planted
                    sysEXECUTE();
                }
        ;

        block ::=
                {   ;;; start a new block:
                    ;;; add a new layer to the environment for any
                    ;;; identifiers declared in the <declaration_list>
                    [] :: env -> env;
                    ;;; start a corresponding VM lexical block
                    sysLBLOCK(false);
                }
            declaration_list statement_list
                {   ;;; finish the block
                    sysENDLBLOCK();
                    ;;; remove any identifiers declared
                    tl(env) -> env;
                }
        ;

        declaration_list ::=
            /* empty */
        |   declaration_list declaration ';'
        ;

        declaration ::=
            /*  The only declaration supported is a variable declaration:
                as an exercise, try adding other declaration forms,
                e.g. constants or functions
            */
            var_declaration
        ;

        var_declaration ::=
            'var' identifier_list
        ;

        identifier_list ::=
            /* empty */
        |   identifier
        |   identifier ',' identifier_list
        ;

        identifier ::=
            IDENTIFIER:id
                {   ;;; declare the variable
                    id :: hd(env) -> hd(env);
                    ;;; create a corresponding VM lexical variable
                    sysLVARS(id, 0);
                    ;;; initialise it
                    sysPUSHQ(0), sysPOP(id);
                }
        ;

        statement_list ::=
            /* empty */
        |   statement_list statement ';'
        ;

        statement ::=
            while_statement
        |   if_statement
        |   assignment_statement
        |   write_statement
        ;

        while_statement ::=
            'while'
                {   ;;; label the loop start (before the test)
                    sysLABEL(dup(sysNEW_LABEL()));
                }:start_label
            condition 'do'
                {   ;;; do the test and quit the loop if it fails
                    sysIFNOT(dup(sysNEW_LABEL()))
                }:end_label
            block 'endwhile'
                {   ;;; restart the loop (after the loop body)
                    sysGOTO(start_label);
                    ;;; label the loop end
                    sysLABEL(end_label);
                }
        ;

        if_statement ::=
            'if' conditional_statement 'endif'
        ;

        conditional_statement ::=
            condition 'then'
                {   ;;; do the test and goto the else-case if it fails
                    sysIFNOT(dup(sysNEW_LABEL()))
                }:else_label
            block
                {   ;;; finished the if-case
                    sysGOTO(dup(sysNEW_LABEL()));
                    ;;; start the else-case
                    sysLABEL(else_label);
                }:end_label
            alternative
                {   sysLABEL(end_label);
                }
        ;

        alternative ::=
            /* empty */
        |   'else' block
        |   'elseif' conditional_statement
        ;

        assignment_statement ::=
            variable:x ':=' expression
                {   sysPOP(x);
                }
        ;

        write_statement ::=
            'write' '(' expression ')'
                {   sysCALL("npr");
                }
        ;

        condition ::=
            condition 'and'
                {   ;;; "short-circuit" conjunction:
                    ;;; test the first result, and if it fails,
                    ;;; skip the next expression without evaluating
                    sysAND(dup(sysNEW_LABEL()));
                }:label
            condition
                {   sysLABEL(label);
                }
        |   condition 'or'
                {   ;;; "short-circuit" disjunction:
                    ;;; test the first result, and if it succeeds,
                    ;;; skip the next expression without evaluating
                    sysOR(dup(sysNEW_LABEL()));
                }:label
            condition
                {   sysLABEL(label);
                }
        |   expression '=' expression
                {   sysCALL("=");
                }
        |   expression '/=' expression
                {   sysCALL("/=");
                }
        |   expression '<' expression
                {   sysCALL("<");
                }
        |   expression '<=' expression
                {   sysCALL("<=");
                }
        |   expression '>' expression
                {   sysCALL(">");
                }
        |   expression '>=' expression
                {   sysCALL(">=");
                }
        |   'true'
                {   sysPUSH("true");
                }
        |   'false'
                {   sysPUSH("false");
                }
        ;

        expression ::=
            expression '+' expression
                {   sysCALL("+");
                }
        |   expression '-' expression
                {   sysCALL("-");
                }
        |   expression INTEGER:i
                /*  This is a curious rule, necessitated by the
                    behaviour of the Poplog itemiser: faced with the
                    input "x-1" (for example) the itemiser returns this
                    as two tokens ("x" and "-1") where it only makes
                    sense as three ("x", "-" and "1").

                    This rule matches explicitly the case of an
                    expression followed immediately by an integer, and
                    if the integer's value is negative, reads it as a
                    '-' token, pushing the absolute value of the number
                    back on the input stream to be picked up as the
                    first token of the following expression.

                    This is safe, because the action is done before the
                    next expression is read (check this in the report
                    file). To ensure that the INTEGER token is valid in
                    just the same contexts as '-' would be, it is
                    declared in the tokens section as an operator with
                    precedence 5.
                */
                {   unless i < 0 then
                        error('expecting an operator but found:  %p', [^i]);
                    endunless;
                    abs(i) :: proglist -> proglist;
                }
            expression
                {   sysCALL("-");
                }
        |   expression '*' expression
                {   sysCALL("*");
                }
        |   expression '/' expression
                /*  integer division:
                */
                {   sysCALL("div");
                }
        |   expression '%' expression
                /*  integer remainder:
                */
                {   sysCALL("rem");
                }
        | 3 '-' expression
                /*  prefix negation:
                    the explicit precedence on this rule overrides the
                    default precedence for '-' (5) and ensures that this
                    rule is always done first
                */
                {   sysCALL("negate");
                }
        |   '(' expression ')'
        |   'read' '(' ')'
                {   define lconstant read() -> n;
                        ;;; read an integer from the current input
                        lvars l = readline(), n;
                        until length(l) == 1 and isintegral(hd(l)) do
                            readline() -> l;
                        enduntil;
                        hd(l) -> n;
                    enddefine;
                    sysCALLQ(read);
                }
        |   variable:x
                {   sysPUSH(x);
                }
        |   INTEGER:i
                {   sysPUSHQ(i);
                }
        ;

        variable:1 ::=
            /*  This symbol is used for an applied occurrence of an
                identifier, so it's the same as IDENTIFIER but with a check
                that it has been declared. The identifier name is returned
                as a result, hence the ':1' qualifier
            */
            IDENTIFIER:id
                {   lvars e = env;
                    until e == [] or member(id, hd(e)) do
                        tl(e) -> e;
                    enduntil;
                    if e == [] then
                        error('unbound identifier: %p', [^id]);
                    endif;
                    ;;; return it
                    id;
                }
        ;

    endrules

    enddefine;  /* read_and_execute */

    ;;; call the compiler inside a new VM compilation context
    sysCOMPILE(input, read_and_execute);
enddefine;

endsection;
