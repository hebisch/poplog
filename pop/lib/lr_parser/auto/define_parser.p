/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/lr_parser/auto/define_parser.p
 > Purpose:         Pop11 syntax interface to the LALR(1) parser generator
 > Author:          Robert John Duncan, Nov 27 1992 (see revisions)
 > Documentation:   HELP * DEFINE_PARSER
 > Related Files:   LIB * LR_PARSER
 */

compile_mode:pop11 +strict;

section;

uses lr_build;

include vm_flags;

weak global vars
    procedure lr_output_pr,
    pop_show_code
;

;;; =======================================================================

/*
 *  Identifiers exported from this file
 */

global constant syntax
    define_parser,
        ;;; the define-form itself, defined at the end
;

global vars
    define_parser_options,
        ;;; list of additional options to be applied to every parser
        ;;; definition
;

/*
 *  Local variables used throughout the file
 */

lvars

    parser_name,
        ;;; name of the procedure being defined
    parser_arg_names,
        ;;; names of its arguments

    parser_input_name,
        ;;; name of the parser's input procedure
    parser_input_p_name,
        ;;; name of the INPUT_P argument to -lr_parse-
    parser_reduce_p_name,
        ;;; name of the REDUCE_P argument to -lr_parse-
    parser_tables_name,
        ;;; name of the PARSER argument to -lr_parse-

    parser_tokens,
        ;;; list of terminal symbols
    parser_symbols,
        ;;; list of non-terminal symbols
    parser_start_symbol,
        ;;; start symbol
    parser_rules,
        ;;; list of productions

    parser_conflicts,
        ;;; number of conflicts expected in the parser
    parser_program_dev,
        ;;; device for program output
    parser_default_multiplicity,
        ;;; number of results returned by each nonterminal
    parser_termin_token,
        ;;; name of the end-of-input marker

;


/*
 *  Declaration part
 */

;;; declaration_part:
;;;     reads the name of the parser together with any additional
;;;     declarations. Returns the declaration both as text and as a
;;;     procedure.

define lconstant declaration_part() -> (decl, decl_p, name);
    lvars name, global_p = false, decl_p = false, idprops = false, decl = [];

    define lconstant is_decl_keyword =
        newproperty([
            [vars       sysVARS]
            [constant   sysCONSTANT]
            [lvars      sysLVARS]
            [lconstant  sysLCONSTANT]
        ], 8, false, "perm");
    enddefine;

    readitem() -> name;
    if name == "global" then
        [global] -> decl;
        sysGLOBAL -> global_p;
        readitem() -> name;
    endif;
    if is_decl_keyword(name) ->> decl_p then
        [^^decl ^name] -> decl;
        valof(decl_p) -> decl_p;
        readitem() -> name;
    endif;
    if name == "procedure" then
        [^^decl procedure] -> decl;
        "procedure" -> idprops;
        readitem() -> name;
    endif;
    ;;; return procedure for declaring the parser name
    pop11_define_declare(% global_p, decl_p, idprops %) -> decl_p;
enddefine;


/*
 *  Argument part
 */

;;; argument_part:
;;;     reads optional arguments to the parser procedure. Arguments
;;;     default to lvars, but this can be overriden by including an
;;;     alternative declaration in the argument list.

define lconstant argument_part() -> (decls, decl_ps, args);
    lvars decl, decl_p, arg, decls = [], decl_ps = [], args = [];

    ;;; read a single argument
    define lconstant argument() -> (decl, decl_p, arg);
        lvars arg, decl, decl_p, idprops;

        define lconstant is_decl_keyword =
            newproperty([
                [lvars      sysLVARS]
                [dlvars     sysDLVARS]
                [dlocal     sysLOCAL]
                [vars       sysVARS]
            ], 8, false, "perm");
        enddefine;

        readitem() -> arg;
        if is_decl_keyword(arg) ->> decl_p then
            [^arg] -> decl;
            valof(decl_p) -> decl_p;
            readitem() -> arg;
        else
            [lvars] -> decl;
            sysLVARS -> decl_p;
        endif;
        unless decl_p == sysLOCAL then
            if arg == "procedure" then
                [^^decl procedure] -> decl;
                "procedure" -> idprops;
                readitem() -> arg;
            else
                0 -> idprops;
            endif;
            decl_p(% idprops %) -> decl_p;
        endunless;
        if arg == ";" then
            arg :: proglist -> proglist;
            pop11_need_nextreaditem(")") -> ;
        elseif is_decl_keyword(arg) or lmember(arg, [, ) procedure]) then
            mishap(arg, 1, 'ARGUMENT NAME NEEDED');
        elseif not(isword(arg)) then
            mishap(arg, 1, 'WORD NEEDED');
        endif;
    enddefine;

    returnunless(pop11_try_nextreaditem("("));
    until pop11_try_nextreaditem(")") do
        argument() -> (decl, decl_p, arg);
        conspair(decl, decls) -> decls;
        conspair(decl_p, decl_ps) -> decl_ps;
        conspair(arg, args) -> args;
        pop11_try_nextreaditem(",") -> ;
    enduntil;
    fast_ncrev(decls) -> decls;
    fast_ncrev(decl_ps) -> decl_ps;
    fast_ncrev(args) -> args;
enddefine;


/*
 *  Options part
 */

;;; Checkr_X:
;;;     checking procedures for option values

define lconstant Checkr_nat(item) -> item;
    lvars item;
    unless isinteger(item) and item fi_>= 0 then
        mishap(item, 1, 'INTEGER >= 0 NEEDED');
    endunless;
enddefine;

define lconstant Checkr_string(item) -> item;
    lvars item;
    unless isstring(item) then
        mishap(item, 1, 'STRING NEEDED');
    endunless;
enddefine;

define lconstant Checkr_word(item) -> item;
    lvars item;
    unless isword(item) then
        mishap(item, 1, 'WORD NEEDED');
    endunless;
enddefine;

define lconstant Checkr_file(item) -> item;
    lvars item;
    unless isword(item) then
        Checkr_string(item) -> item;
    endunless;
enddefine;

;;; option:
;;;     table of options (property initialised by -define_parser-)

define lvars option =
    identfn(% false %);
enddefine;

;;; options_part:
;;;     reads the <options-part> of the define-form, recording any
;;;     options selected in the -option- property. Also checks for
;;;     global options from the -define_parser_options- list.

define lconstant options_part();
    lvars item, arg;

    ;;; add an entry for -item- to the option table
    define lconstant add_option(item);
        lvars item, name = item, args = [];

        ;;; item may be just an option name, or a 2-list: [^name ^arg]
        if ispair(item) then
            fast_destpair(item) -> (name, args);
            ;;; allow for: [^name = ^arg]
            if ispair(args) and fast_front(args) == "=" then
                fast_back(args) -> args;
            endif;
        endif;

        ;;; get and check the next value from -args-
        define lconstant get_arg(checkr_p, arg) -> arg;
            lvars checkr_p, arg;
            unless args == [] then
                if ispair(args) then
                    fast_destpair(args)
                else
                    (args, [])
                endif -> (arg, args);
                checkr_p(arg) -> arg;
            endunless;
        enddefine;

        if name == "conflicts" then
            ;;; number of conflicts expected: conflicts = <N>/<M>
            lvars n_sr, n_rr;
            get_arg(Checkr_nat, 0) -> n_sr;
            if ispair(args) and fast_front(args) == "/" then
                fast_back(args) -> args;
            endif;
            get_arg(Checkr_nat, 0) -> n_rr;
            conspair(n_sr, n_rr);
        elseif name == "input" then
            ;;; name for the input procedure: input = <name>
            get_arg(Checkr_word, "input");
        elseif name == "keep" then
            ;;; keep the parser in the -lr_parser- property
            true;
        elseif name == "parser" then
            ;;; name for the parser structure: parser = <name>
            get_arg(Checkr_word, "parser");
        elseif name == "program" then
            ;;; program output wanted: program = <filename>
            lvars file = get_arg(Checkr_file, parser_name);
            if isword(file) then
                file sys_>< '.p' -> file;
            endif;
            useslib("lr_output");
            file;
        elseif name == "raw_input" then
            ;;; input needs no processing:
            true;
        elseif name == "report" then
            ;;; output report wanted: report = <filename>
            lvars file = get_arg(Checkr_file, parser_name);
            useslib("lr_report");
            file;
        elseif name == "results" then
            ;;; change to the default multiplicity of non-terminal symbols:
            ;;; results = <N>
            get_arg(Checkr_nat, 1);
        elseif name == "showcode" then
            ;;; set -pop_show_code- (if lib showcode loaded)
            true;
        elseif name == "stack_checks" then
            ;;; plant extra code to count results returned
            true;
        elseif name == "start" then
            ;;; alternative start symbol: start = <symbol>
            get_arg(Checkr_word, false);
        elseif name == "termin" then
            ;;; alternative name for end of input marker: termin = <name>
            get_arg(Checkr_word, "termin");
        elseif name == "trace" then
            ;;; use -lr_trace- instead of -lr_parse-
            useslib("lr_trace");
            true;
        else
            mishap(name, 1, 'ILLEGAL OPTION NAME');
        endif -> option(name);
    enddefine;  /* add_option */

    /* options_part */

    ;;; process options from the define-form
    if pop11_try_nextreaditem("options") then
        until pop11_try_nextreaditem("endoptions") do
            Checkr_word(readitem()) -> item;
            if pop11_try_nextreaditem("=") then
                ;;; argument given
                readitem() -> arg;
                ;;; possible qualifier
                if pop11_try_nextreaditem("/") then
                    conspair(arg, readitem()) -> arg;
                endif;
                conspair(item, arg) -> item;
            endif;
            add_option(item);
            pop11_try_nextreaditem([, ;]) -> ;
        enduntil;
        ;;; allow for trailing ";"
        pop11_try_nextreaditem(";") -> ;
    endif;
    ;;; process any options specified globally
    if islist(define_parser_options) then
        applist(define_parser_options, add_option);
    endif;
enddefine;


/*
 *  Definitions part
 */

;;; outf:
;;;     like -printf-, but redirected to the program output device

define lconstant outf();
    dlocal  cucharout = charout, pop_charout_device = parser_program_dev,
            poplinewidth = false;
    printf();
enddefine;

;;; copy_compile:
;;;     compiles from -proglist- using
;;;         compile_p(closers) -> closer;
;;;     and copies out all characters consumed by the compiler to
;;;     the program output device

define lconstant copy_compile(closers, compile_p) -> closer;
    lvars closers, compile_p, closer, input;

    unless isincharitem(readitem) ->> input then
        warning(0, 'CAN\'T COPY PROGRAM TEXT');
        false -> parser_program_dev;
        chain(closers, compile_p);
    elseunless isdevice(parser_program_dev) then
        mishap(parser_program_dev, 1, 'DEVICE NEEDED');
    endunless;

    lconstant SPACE = 6, BUFSIZE = 128, buff = writeable inits(128);

    lvars buff_index = 1, copy_state = 1;

    define lconstant nextc() -> c;
        lvars c = nextchar(input);
        if copy_state == 2 then
            ;;; collect non-space characters in the buffer
            c -> fast_subscrs(buff_index, buff);
            if buff_index == BUFSIZE then
                fast_syswrite(parser_program_dev, 1, buff, BUFSIZE);
                1 -> buff_index;
            else
                buff_index fi_+ 1 -> buff_index;
            endif;
            if item_chartype(c) == SPACE then
                3 -> copy_state;
            endif;
        else
            if copy_state == 3 then
                ;;; flush out collected token
                fast_syswrite(parser_program_dev, 1, buff, buff_index fi_- 1);
                1 ->> buff_index -> copy_state;
            endif;
            if copy_state == 1 then
                c -> fast_subscrs(1, buff);
                if item_chartype(c) == SPACE then
                    ;;; copy out leading spaces
                    fast_syswrite(parser_program_dev, 1, buff, 1);
                else
                    2 ->> buff_index -> copy_state;
                endif;
            endif;
        endif;
    enddefine;

    define lconstant flush(closer);
        lvars closer, i = 0, n = buff_index fi_- 1, j;
        if isword(closer) then
            ;;; remove the last occurrence of closer from the buffer
            while issubstring_lim(closer, i fi_+ 1, false, n, buff) ->> j do
                j -> i;
            endwhile;
            if i fi_> 0 then i fi_- 1 -> n endif;
        endif;
        ;;; flush out the last token
        fast_syswrite(parser_program_dev, 1, buff, n);
        ;;; don't copy any single-character read-ahead
        0 -> copy_state;
    enddefine;

    dlocal proglist_state = proglist_new_state(nextc);

    compile_p(closers) -> closer;
    flush(closer);
    ;;; put back the last character for enclosing compile
    nextchar(readitem) -> nextchar(input);
enddefine;

;;; comp_user_code:
;;;     hook for compilation of user code: may also copy to the program
;;;     output device

define lconstant comp_user_code(closers, compile_p);
    lvars closers, compile_p;
    if parser_program_dev then
        copy_compile(closers, compile_p);
    else
        compile_p(closers);
    endif;
enddefine;

;;; definitions_part:
;;;     compile user code in the "definitions" section

define lconstant definitions_part();
    returnunless(pop11_try_nextreaditem("definitions"));
    comp_user_code("enddefinitions", pop11_comp_stmnt_seq_to) -> ;
    ;;; allow for trailing ";"
    pop11_try_nextreaditem(";") -> ;
    returnunless(parser_program_dev);
    ;;; separate following code
    outf('\n');
enddefine;


/*
 *  The symbol table
 */

lconstant
    ;;; Keywords of the parser syntax form: these can't be used as symbols
    KEYWORDS = [
        options endoptions
        definitions enddefinitions
        tokens endtokens
        rules endrules
        , ; | = : ( ) ::=
        %"{","}"%       ;;; { }
    ],
;

;;; symbols standing for intermediate actions:
defclass lconstant ias {
    ias_number,
};
;;;
procedure(ias);
    lvars ias;
    printf('{%p}', [% ias_number(ias) %]);
endprocedure -> class_print(ias_key);

;;; symbols standing for meta-symbol applications:
defclass lconstant mas {
    mas_symbol,
        ;;; meta-symbol applied
    mas_actuals,
        ;;; vector of actual arguments
};
;;;
procedure(mas);
    lvars mas;
    printf('%p(', [% mas_symbol(mas) %]);
    lvars i, actuals = mas_actuals(mas), n = datalength(actuals);
    fast_for i to n do
        pr(fast_subscrv(i, actuals));
        unless i == n then printf(',') endunless;
    endfor;
    printf(')');
endprocedure -> class_print(mas_key);

;;; meta-symbol definition:
defclass lconstant meta {
    meta_signature,
        ;;; vector of argument multiplicities
    meta_rules,
        ;;; list of grammar rules
    meta_labels,
        ;;; list of code labels, one per rule
    meta_expansions,
        ;;; list of applications of this symbol
};

;;; symbol table entry:
defclass lconstant ste {
    ste_type,
        ;;; "token", "symbol", "rule" or "meta-symbol"
    ste_nresults,
        ;;; number of results returned
    ste_plus,
        ;;; extra info: for tokens & rules, their declared precedence;
        ;;; for meta symbols, a meta structure
};

lvars
    local_syms = false,
        ;;; formal parameter bindings established by definition of a
        ;;; meta-symbol
;

;;; symtab:
;;;     the symbol table (property initialised by -define_parser-)

define lvars symtab =
    identfn(% false %);
enddefine;

;;; Checkr_symbol:
;;;     check that an item is valid as a symbol

define lconstant Checkr_symbol(item) -> item;
    lvars item;
    if isstring(item) then
        consword(item) -> item;
    elseif fast_lmember(Checkr_word(item), KEYWORDS) then
        mishap(item, 1, 'ILLEGAL USE OF PARSER KEYWORD');
    endif;
enddefine;

;;; getsym:
;;;     retrieve a symbol table entry

define lconstant getsym(symbol) -> ste;
    lvars symbol, ste;
    if local_syms then
        fast_for ste in local_syms do
            returnif(ste_plus(ste) == symbol);
        endfor;
    endif;
    symtab(symbol) -> ste;
enddefine;

;;; symbol_nresults:
;;;     get/set the multiplicity of a nonterminal symbol

define lconstant symbol_nresults(symbol) -> n;
    lvars symbol, ste, n;
    if getsym(symbol) ->> ste then
        ;;; previously declared
        ste_nresults(ste) -> n;
    else
        ;;; first occurrence: return the default multiplicity
        parser_default_multiplicity -> n;
        ;;; make sure the number is the same on subsequent calls
        consste("symbol", n, false) -> symtab(symbol);
    endif;
enddefine;
;;;
define updaterof lconstant symbol_nresults(n, symbol);
    lvars symbol, ste, n;
    if getsym(symbol) ->> ste then
        ;;; previously declared: check for consistency
        unless n == ste_nresults(ste) then
            if ste_nresults(ste) then
                mishap('REDECLARING MULTIPLICITY OF SYMBOL', [^symbol : ^n]);
            endif;
            ;;; <false> means a meta-symbol used before defined
            n -> ste_nresults(ste);
        endunless;
    else
        consste("symbol", n, false) -> symtab(symbol);
    endif;
enddefine;

;;; dummy_symbol:
;;;     generate a dummy symbol for an intermediate action

lvars dummy_symbol_cnt = 0;
define lconstant dummy_symbol() -> symbol;
    lvars symbol;
    dummy_symbol_cnt fi_+ 1 -> dummy_symbol_cnt;
    consias(dummy_symbol_cnt) -> symbol;
    conspair(symbol, parser_symbols) -> parser_symbols;
enddefine;

;;; declare_token:
;;;     declare a terminal symbol with precedence -prec-

define lconstant declare_token(token, prec);
    lvars token, prec;
    consste("token", 1, prec) -> symtab(token);
enddefine;

;;; declare_symbol:
;;;     declare a non-terminal symbol of multiplicity -n-

define lconstant declare_symbol(symbol, n);
    lvars symbol, n;
    lvars ste = symtab(symbol);
    if ste and ste_type(ste) /== "symbol" then
        mishap('REDECLARING ' <> lowertoupper(word_string(ste_type(ste))) <>
            ' AS SYMBOL', [^symbol]);
    endif;
    n -> symbol_nresults(symbol);
    conspair(symbol, parser_symbols) -> parser_symbols;
    ;;; start symbol defaults to the first one declared
    unless parser_start_symbol then symbol -> parser_start_symbol endunless;
enddefine;

;;; declare_meta:
;;;     declare a meta-symbol with given formals and multiplicity -n-

define lconstant declare_meta(symbol, signature, n) -> meta;
    lvars symbol, signature, n, meta;
    lvars ste;
    if symtab(symbol) ->> ste then
        unless ismeta(ste_plus(ste) ->> meta) then
            mishap('ILLEGAL REDECLARATION OF SYMBOL', [^symbol]);
        endunless;
        unless ste_nresults(ste) then
            n -> ste_nresults(ste);
        endunless;
        unless n == ste_nresults(ste)
        and signature = meta_signature(meta)
        then
            mishap('REDECLARING SIGNATURE OF META-SYMBOL',
                [^symbol ( %explode(signature)% ) : ^n]);
        endunless;
    else
        consmeta(signature, [], [], []) -> meta;
        consste("'meta-symbol'", n, meta) -> symtab(symbol);
    endif;
enddefine;

;;; declare_rule:
;;;     declare precedence for a rule

define lconstant declare_rule(rule, prec);
    lvars rule, prec;
    consste("rule", 1, prec) -> symtab(rule);
enddefine;


/*
 *  Tokens part
 */

;;; gen_var:
;;;     generate a variable name from the parser procedure name plus a
;;;     -suffix-

define lconstant gen_var(suffix) -> name;
    lvars suffix, name, i = 1;
    consword(#| explode(parser_name), `_`, explode(suffix) |#) -> name;
    ;;; we don't expect redeclarations, but just in case ...
    while sys_current_ident(name) do
        consword(#|
            explode(parser_name), `_`, explode(suffix), `_`,
            dest_characters(i)
        |#) -> name;
        i fi_+ 1 -> i;
    endwhile;
enddefine;

;;; precedence:
;;;     read an optional integer precedence for a token or rule

define lconstant precedence() -> prec;
    lvars prec = false;
    if isintegral(nextreaditem()) then
        readitem() -> prec;
    endif;
enddefine;

;;; tokens_part:
;;;     read the <tokens-part> of the define-form

define lconstant tokens_part();
    lvars item_v;

    ;;; start the input classification procedure
    define lconstant start_input_p();
        lvars lab;
        sysLCONSTANT(parser_input_p_name, "procedure");
        sysPROCEDURE(parser_input_p_name, 0);
        sysLVARS(item_v, 0);
        ;;; read the item
        sysCALL(parser_input_name);
        sysPOP(item_v);
        ;;; push it as first of two results
        sysPUSH(item_v);
        ;;; generate initial test for end of input
        sysPUSH(item_v), sysPUSH(parser_termin_token), sysCALL("==");
        sysIFNOT(sysNEW_LABEL() ->> lab);
        sysPUSHQ(0), sysGOTO("return");
        sysLABEL(lab);
        returnunless(parser_program_dev);
        ;;; write out procedure header
        outf(parser_input_p_name, 'define lconstant procedure %p;\n');
        outf(item_v, parser_input_name, item_v, 'lvars %p = %p();\n\t%p;\n');
        outf(parser_termin_token, item_v, '\tif %p==%p then\n\t\t0;\n');
    enddefine;

    ;;; compile a recogniser for -token-
    define lconstant comp_recogniser(token, i, closer);
        lvars   token, i, closer, item, lab;
        dlocal  pop_syntax_only, parser_program_dev;

        define lconstant pr_token(token);
            lvars   token;
            dlocal  pr = weakref lr_output_pr;
            outf(token, '%p');
        enddefine;

        if option("raw_input") then
            (true, false) -> (pop_syntax_only, parser_program_dev);
        endif;
        sysPUSH(item_v);
        if pop11_try_nextreaditem([= :]) ->> item then
            ;;; compile an explicit recogniser
            if parser_program_dev then
                if item == ":" then "." -> item endif;
                outf(item, item_v, '\telseif %p%p(');
            endif;
            comp_user_code([^closer , ;], pop11_comp_expr_to) :: proglist
                -> proglist;
            if item == "=" then sysCALL("=") else sysCALLS(0) endif;
            if parser_program_dev then
                outf(') then\n');
            endif;
        else
            ;;; token is its own recogniser
            sysPUSHQ(token), sysCALL("==");
            if parser_program_dev then
                outf(item_v, '\telseif %p=='), pr_token(token),
                    outf(' then\n');
            endif;
        endif;
        sysIFNOT(sysNEW_LABEL() ->> lab);
        sysPUSHQ(i), sysGOTO("return");
        sysLABEL(lab);
        returnunless(parser_program_dev);
        ;;; write out the result
        outf(i, '\t\t%p;\n');
    enddefine;

    ;;; end the input classification procedure
    define lconstant end_input_p();
        ;;; if we get here, item is unmatched: return an error value
        sysPUSHQ(-1);
        ;;; end of procedure
        sysLABEL("return");
        sysPASSIGN(sysENDPROCEDURE(), parser_input_p_name);
        returnunless(parser_program_dev);
        ;;; complete procedure definition
        outf('\telse\n\t\t-1;\n\tendif;\nenddefine;\n');
    enddefine;

    gen_var("item") -> item_v;
    if option("raw_input") then
        ;;; user input procedure is passed on unchanged
        parser_input_name -> parser_input_p_name;
    else
        ;;; start input classification procedure
        start_input_p();
    endif;

    ;;; read tokens and recognisers
    lvars token, n_tokens = 0, (prec, bracketed) = (false, false);
    pop11_need_nextreaditem("tokens") -> ;
    [%  until pop11_try_nextreaditem("endtokens") do
            unless bracketed then
                precedence() -> prec;
                if pop11_try_nextreaditem("(") then ")" -> bracketed endif;
            endunless;
            Checkr_symbol(readitem()) -> token;
            n_tokens + 1 -> n_tokens;
            comp_recogniser(token, n_tokens, bracketed or "endtokens");
            pop11_try_nextreaditem([, ;]) -> ;
            if bracketed and pop11_try_nextreaditem(")") then
                false -> bracketed;
                pop11_try_nextreaditem([, ;]) -> ;
            endif;
            ;;; add to symbol table
            declare_token(token, prec);
            ;;; include in tokens list
            token;
        enduntil;
    %] -> parser_tokens;
    if bracketed then
        ;;; missing closing bracket: force an appropriate error
        "endtokens" :: proglist -> proglist;
        pop11_need_nextreaditem(")") -> ;
    endif;
    ;;; allow for trailing ";"
    pop11_try_nextreaditem(";") -> ;

    unless option("raw_input") then
        ;;; end input classification procedure
        end_input_p();
    endunless;
enddefine;


/*
 *  Rules part
 */

;;; rules_part:
;;;     read the <rules-part> of the define-form

define lconstant rules_part();

    /* Generating the reduction procedure */

    lvars
        return_lab,         ;;; label for the end of the procedure
        go_on_lab,          ;;; label for the reduction switch
        rule_labels,        ;;; list of action labels, one for each rule
        default_labels,     ;;; vector of labels for common default actions
        label_cnt = 0,      ;;; number of labels generated
        meta_apps = [],     ;;; list of meta-calls to be expanded
    ;

    ;;; generate a label from the parser procedure name plus a count
    define lconstant gen_lab() -> lab;
        lvars lab;
        label_cnt fi_+ 1 -> label_cnt;
        consword(#|
            explode(parser_name),
            `_`,`l`,`a`,`b`,`_`,
            dest_characters(label_cnt)
        |#) -> lab;
    enddefine;

    ;;; start the reduction procedure: the body is just a switch on the
    ;;; argument (the reduction number) but that has to go at the end, after
    ;;; all the actions are compiled
    define lconstant start_reduce_p();
        sysLCONSTANT(parser_reduce_p_name, "procedure");
        sysPROCEDURE(parser_reduce_p_name, 1);
        ;;; local var for stack checks
        if option("stack_checks") then
            sysLVARS(gen_var("stack_size") ->> option("stack_checks"), 0);
        endif;
        ;;; jump straight to the switch at the end
        sysGOTO(gen_lab() ->> go_on_lab);
        gen_lab() -> return_lab;
        [] -> rule_labels;
        initv(16) -> default_labels;
        returnunless(parser_program_dev);
        ;;; write out procedure header
        outf(parser_reduce_p_name, 'define lconstant procedure %p;\n');
        outf(go_on_lab, '\tgoto %p;\n');
    enddefine;

    ;;; end the reduction procedure: does a switch on the reduction number
    ;;; still on the stack; we can assume it will be an integer, and
    ;;; because we've just entered the procedure, we can ignore back jump
    ;;; checks too
    define lconstant end_reduce_p();
        dlocal
            pop_debugging = false,
            pop_vm_flags = pop_vm_flags
                || VM_NO_CHECK_GO_ON_INT
                || VM_NO_BACK_JUMP_CHECKS
            ;
        sysLABEL(go_on_lab);
        ;;; sysGO_ON doesn't accept an empty list
        if rule_labels == [] then
            mishap('EMPTY RULE SET', []);
        endif;
        sysGO_ON(fast_ncrev(rule_labels) ->> rule_labels, false);
        sysLABEL(return_lab);
        sysLABEL("return");
        sysPASSIGN(sysENDPROCEDURE(), parser_reduce_p_name);
        returnunless(parser_program_dev);
        ;;; complete procedure definition
        outf(go_on_lab, '%p:\n');
        outf('\tcompile_mode:vm -bjmpch -goonch;\n');
        outf('\tgo_on () to');
        lvars lab, n = 1000;
        fast_for lab in rule_labels do
            datalength(lab) fi_+ n fi_+ 1 -> n;
            if n fi_> 72 then
                outf(lab, '\n\t\t%p');
                datalength(lab) fi_+ 8 -> n;
            else
                outf(lab, ' %p');
            endif;
        endfor;
        outf(return_lab, '\n\t;\n%p:\nenddefine;\n');
    enddefine;

    ;;; plant a new label at the start of a reduce action
    define lconstant start_action() -> lab;
        lvars lab = gen_lab();
        sysLABEL(lab);
        if parser_program_dev then outf(lab, '%p:\n') endif;
    enddefine;

    ;;; return after a reduce action
    define lconstant end_action();
        sysGOTO(return_lab);
        if parser_program_dev then outf('return;\n') endif;
    enddefine;

    ;;; check the number of results returned by an action
    define lconstant stackcheck(name, linenum, sl, nwanted);
        lvars name, linenum, sl, nwanted, n;
        stacklength() fi_- sl -> n;
        unless n == nwanted then
            mishap(n, if n > nwanted then ">" else "<" endif, nwanted, 3,
                'STACK CHECK FAILED IN PARSER ' sys_>< name sys_>< ', LINE '
                sys_>< linenum);
        endunless;
    enddefine;

    ;;; declare variables to the left of an action and initialise them from
    ;;; the stack; the list vs is in the reverse order of occurrence
    define lconstant pop_vars(vs);
        lvars v, vs;
        returnif(vs == []);
        fast_for v in vs do
            if v == "_" then
                sysERASE(v);
            else
                sysLVARS(v, 0), sysPOP(v);
            endif;
        endfor;
        if parser_program_dev then
            outf('lvars(');
            rev(vs) -> vs;
            repeat
                fast_destpair(vs) -> (v, vs);
                unless v == "_" then outf(v, '%p') endunless;
            quitif(vs == []);
                outf(',');
            endrepeat;
            outf(')=();\n');
        endif;
    enddefine;

    ;;; push results (vs in reverse order, as above)
    define lconstant push_vars(vs);
        lvars v, vs;
        returnif(vs == []);
        rev(vs) -> vs;
        fast_for v in vs do
            if v == "_" then
                ;;; anything will do
                sysPUSHQ("undef");
            else
                sysPUSH(v);
            endif;
        endfor;
        if parser_program_dev then
            outf('(');
            repeat
                fast_destpair(vs) -> (v, vs);
                outf(v == "_" and '"undef"' or v, '%p');
            quitif(vs == []);
                outf(',');
            endrepeat;
            outf(');\n');
        endif;
    enddefine;

    ;;; compile user code between {...}
    define lconstant start_user_action(vs) -> lab;
        lvars v, vs, lab;
        ;;; plant (and return) label for start of action
        start_action() -> lab;
        ;;; start a new block for local vars
        sysLBLOCK(false);
        if parser_program_dev then outf('lblock;\n') endif;
        ;;; pop results from RHS to variables
        pop_vars(vs);
        ;;; compile the action
        if option("stack_checks") then
            ;;; save current stacklength
            sysCALL("stacklength"), sysPOP(option("stack_checks"));
        endif;
        comp_user_code("}", pop11_comp_stmnt_seq_to) -> ;
        ;;; extra ';' + newline to terminate user code
        if parser_program_dev then outf(';\n') endif;
    enddefine;

    ;;; complete user action code
    define lconstant end_user_action(vs, nresults);
        lvars vs, nresults;
        if option("stack_checks") then
            ;;; check the number of results returned
            sysPUSHQ(parser_name), sysPUSHQ(poplinenum),
                sysPUSH(option("stack_checks")), sysPUSHQ(nresults),
                sysCALLQ(stackcheck);
        endif;
        unless vs == [] then
            ;;; values to the left have to be copied back to the stack for
            ;;; subsequent actions; save any results first
            lvars tmps = [% fast_repeat nresults times sysNEW_LVAR() endrepeat %];
            pop_vars(tmps);
            ;;; push the values back
            push_vars(vs);
            ;;; now restore the results
            push_vars(tmps);
        endunless;
        ;;; finish the lblock
        sysENDLBLOCK();
        if parser_program_dev then outf('endlblock;\n') endif;
        ;;; return
        end_action();
    enddefine;

    ;;; generate code for a default action to remove -n- items from the
    ;;; stack
    define lconstant gen_default_action(n) -> lab;
        lvars n, lab;
        if n == 0 then
            ;;; nothing to do
            return_lab -> lab;
        elseif n fi_<= datalength(default_labels) then
            ;;; share code for common cases
            fast_subscrv(n, default_labels) -> lab;
            if lab == undef then
                start_action() -> lab;
                fast_repeat n times sysERASE(0) endrepeat;
                if parser_program_dev then
                    outf('\t() -> (');
                    fast_repeat n fi_- 1 times outf(',') endrepeat;
                    outf(');\n');
                endif;
                end_action();
                lab -> fast_subscrv(n, default_labels);
            endif;
        else
            start_action() -> lab;
            sysPUSHQ(n), sysCALL("erasenum");
            if parser_program_dev then
                outf(n, '\terasenum(%p);\n');
            endif;
            end_action();
        endif;
    enddefine;

    /* Reading the rules */

    ;;; read an optional result qualifier after a symbol on the RHS of a
    ;;; rule; may be an integer or an identifier list
    define lconstant read_rhs_results();
        returnunless(pop11_try_nextreaditem(":"))(false);
        lvars item = readitem();
        if item == "(" then
            [%  until pop11_try_nextreaditem(")") do
                    Checkr_word(readitem());
                    quitunless(pop11_need_nextreaditem(#_<[),]>_#) == ",");
                enduntil;
            %];
        elseif isinteger(item) then
            Checkr_nat(item);
        else
            ;;; single identifier
            [% Checkr_word(item) %];
        endif;
    enddefine;

    ;;; check the number of results declared for a symbol on the RHS
    define lconstant check_nresults(symbol, results) -> results;
        lvars symbol, results;
        if not(results) then
            symbol_nresults(symbol) -> results;
        elseif isinteger(results) then
            results -> symbol_nresults(symbol);
        else
            listlength(results) -> symbol_nresults(symbol);
        endif;
    enddefine;

    ;;; record an application of a meta-symbol, allowing only one
    ;;; instance for each call
    define lconstant add_meta_app(symbol, actuals, meta) -> mas;
        lvars symbol, actuals, meta, mas;
        ;;; do nothing inside a meta-rule
        returnif(local_syms)(consmas(symbol, actuals) -> mas);
        ;;; look for a previous occurrence of this application
        fast_for mas in meta_expansions(meta) do
            returnif(actuals = mas_actuals(mas));
        endfor;
        ;;; first occurrence -- add it in
        consmas(symbol, actuals) -> mas;
        mas :: meta_expansions(meta) -> meta_expansions(meta);
        mas :: meta_apps -> meta_apps;
    enddefine;

    ;;; replace an application of a meta-symbol on the RHS of a rule
    define lconstant replace_meta(symbol, actuals) -> symbol;
        lvars symbol, actuals;
        lvars ste, meta;
        lvars signature = {% applist(actuals, symbol_nresults) %};
        if getsym(symbol) ->> ste then
            ;;; should be a meta-symbol
            unless ismeta(ste_plus(ste) ->> meta) then
                mishap('ILLEGAL USE OF SYMBOL', [^symbol ( ^^actuals )]);
            elseunless signature = meta_signature(meta) then
                mishap('ILLEGAL APPLICATION OF META-SYMBOL',
                    [^symbol ( ^^actuals )]);
            endunless;
        else
            ;;; forward reference
            consmeta(signature, [], [], []) -> meta;
            consste("'meta-symbol'", false, meta) ->> ste -> symtab(symbol);
        endif;
        add_meta_app(symbol, {^^actuals}, meta) -> symbol;
        if ste_nresults(ste) then
            ste_nresults(ste) -> symbol_nresults(symbol);
        endif;
    enddefine;

    ;;; read an item on the RHS of a rule
    define lconstant read_rhs_item(item) -> (symbol, results);
        lvars item, symbol, results;
        Checkr_symbol(item) -> symbol;
        if pop11_try_nextreaditem("(") then
            ;;; application of a meta-symbol
            lvars actuals = [%
                until pop11_try_nextreaditem(")") do
                    read_rhs_item(readitem()) -> /*results*/;
                    quitunless(pop11_need_nextreaditem(#_<[),]>_#) == ",");
                enduntil;
            %];
            replace_meta(symbol, actuals) -> symbol;
        endif;
        check_nresults(symbol, read_rhs_results()) -> results;
    enddefine;

    ;;; read the RHS of a rule
    define lconstant read_rhs(lhs) -> (rule, lab, item);
        lvars lhs, rule = [], lab = false, item;

        ;;; merge results returned by a symbol with those already seen
        define lconstant add_vars(results, rulevars) -> rulevars;
            lvars i, results, rulevars;
            if isinteger(results) then
                fast_for i to results do
                    conspair("_", rulevars) -> rulevars;
                endfor;
            else
                fast_for i in results do
                    conspair(i, rulevars) -> rulevars;
                endfor;
            endif;
        enddefine;

        lvars symbol, results, rulevars = [], prec = precedence();
        readitem() -> item;
        until item == "|" or item == ";" do
            if item == "{" then
                ;;; start of action code
                start_user_action(rulevars) -> lab;
                read_rhs_results() -> results;
                readitem() -> item;
                if item == "|" or item == ";" then
                    ;;; final action:
                    ;;; check results are OK for LHS
                    check_nresults(lhs, results) -> results;
                    ;;; complete the action code; number of results
                    ;;; must match the arity of the LHS
                    end_user_action([], symbol_nresults(lhs));
                else
                    ;;; intermediate action:
                    ;;; create a dummy symbol for it
                    dummy_symbol() -> symbol;
                    ;;; ... and push a single (empty) rule
                    {^symbol};
                    conspair(lab, rule_labels) -> rule_labels;
                    false -> lab;
                    ;;; add the dummy symbol to the current rule
                    ;;; in place of the action
                    conspair(symbol, rule) -> rule;
                    ;;; number of results defaults to 0 if not given
                    check_nresults(symbol, results or 0) -> results;
                    ;;; complete the action code: must copy back any vars
                    ;;; occurring to the left
                    end_user_action(rulevars, symbol_nresults(symbol));
                    ;;; augment the rule vars
                    add_vars(results, rulevars) -> rulevars;
                endif;
            else
                ;;; next symbol
                read_rhs_item(item) -> (symbol, results);
                conspair(symbol, rule) -> rule;
                ;;; augment the rule vars
                add_vars(results, rulevars) -> rulevars;
                readitem() -> item;
            endif;
        enduntil;
        ;;; if there was no final action, generate the default
        unless lab then
            lvars n = listlength(rulevars) fi_- symbol_nresults(lhs);
            if n fi_< 0 then
                mishap('TOO FEW RESULTS RETURNED BY RIGHT-HAND-SIDE', []);
            endif;
            gen_default_action(n) -> lab;
        endunless;
        ;;; pack the rule into a vector
        {^lhs ^^(fast_ncrev(rule))} -> rule;
        ;;; add it to the symbol table if it's got explicit precedence
        if prec then declare_rule(rule, prec) endif;
    enddefine;

    ;;; determine number of results returned by a symbol on the LHS
    define lconstant read_lhs_results() -> nresults;
        lvars nresults = parser_default_multiplicity;
        if pop11_try_nextreaditem(":") then
            Checkr_nat(readitem()) -> nresults;
        endif;
    enddefine;

    ;;; read a group of rules with the same LHS
    define lconstant read_rules(lhs);
        lvars lhs;
        declare_symbol(lhs, read_lhs_results());
        pop11_need_nextreaditem("::=") -> ;
        repeat
            lvars (lab, item) = read_rhs(lhs);  ;;; leaves rule on the stack
            conspair(lab, rule_labels) -> rule_labels;
            quitif(item == ";");
        endrepeat;
    enddefine;

    ;;; read a group of rules for a meta-symbol
    define lconstant read_meta_rules(lhs);
        lvars lhs;
        ;;; read formal parameters
        dlocal local_syms = [%
            until pop11_try_nextreaditem(")") do
                lvars formal = Checkr_symbol(readitem());
                consste("formal", read_lhs_results(), formal);
                quitunless(pop11_need_nextreaditem(#_<[),]>_#) == ",");
            enduntil;
        %];
        lvars signature = {% applist(local_syms, ste_nresults) %};
        lvars formals = {% applist(local_syms, ste_plus) %};
        lvars meta = declare_meta(lhs, signature, read_lhs_results());
        pop11_need_nextreaditem("::=") -> ;
        repeat
            lvars (rule, lab, item) = read_rhs(lhs);
            conspair(formals, rule) -> rule;
            conspair(rule, meta_rules(meta)) -> meta_rules(meta);
            conspair(lab, meta_labels(meta)) -> meta_labels(meta);
            quitif(item == ";");
        endrepeat;
    enddefine;

    ;;; push new rules for each distinct application of a meta-symbol
    define lconstant add_meta_rules();

        ;;; replace all occurrences of formals with actuals in a vector,
        ;;; starting at index n
        define lconstant replace(formals, actuals, n, vec) -> vec;
            lvars formals, actuals, n, vec;

            define lconstant present(item, vec);
                lvars i, item, vec;
                fast_for i to datalength(vec) do
                    returnif(fast_subscrv(i, vec) == item)(i);
                endfor;
                false;
            enddefine;

            lvars i;
            fast_for i from n to datalength(vec) do
                lvars item = fast_subscrv(i, vec);
                if present(item, formals) ->> n then
                    fast_subscrv(n, actuals) -> fast_subscrv(i, vec);
                elseif ismas(item) then
                    lvars (symbol, args) = explode(item);
                    add_meta_app(
                        symbol,
                        replace(formals, actuals, 1, copy(args)),
                        ste_plus(symtab(symbol))) -> fast_subscrv(i, vec);
                endif;
            endfor;
        enddefine;

        ;;; check that a meta-symbol has been properly defined
        lvars undeclared = [];
        define lconstant check_meta(symbol, ste);
            lvars symbol, ste, meta;
            if ismeta(ste_plus(ste) ->> meta) then
                if meta_rules(meta) == [] then
                    ;;; not defined
                    symbol :: undeclared -> undeclared;
                else
                    ;;; rules have been collected in reverse order
                    fast_ncrev(meta_rules(meta)) -> meta_rules(meta);
                    fast_ncrev(meta_labels(meta)) -> meta_labels(meta);
                endif;
            endif;
        enddefine;

        returnif(meta_apps == []);

        appproperty(symtab, check_meta);
        unless undeclared == [] then
            mishap('UNDECLARED META SYMBOL(S)', undeclared);
        endunless;

        ;;; cycle through all meta applications, generating a
        ;;; specialised set of rules for each one.
        ;;; Expanding a meta-rule may generate further
        ;;; meta-applications, so this is an iterative process, but it
        ;;; must terminate because the number of possible applications
        ;;; is finite
        until meta_apps == [] do
            lvars mas;
            fast_for mas in meta_apps, [] -> meta_apps do
                lvars (symbol, actuals) = explode(mas);
                lvars (, n, meta) = explode(symtab(symbol));
                ;;; declare meta-call as a new non-terminal symbol
                declare_symbol(mas, n);
                ;;; generate copies of all the meta-rules, specialised
                ;;; for this instance
                lvars rule, lab, formals;
                fast_for rule, lab in meta_rules(meta), meta_labels(meta) do
                    destpair(rule) -> (formals, rule);
                    ;;; copy the rule, perserving any symbol table entry
                    ;;; (for an explicit precedence)
                    symtab(rule), copy(rule) -> rule -> symtab(rule);
                    ;;; meta-call symbol becomes the new LHS
                    mas -> fast_subscrv(1, rule);
                    ;;; actual args replace formals on the RHS
                    replace(formals, actuals, 2, rule) -> rule;
                    ;;; add rule to the list
                    rule;
                    conspair(lab, rule_labels) -> rule_labels;
                endfor;
            endfor;
        enduntil;
    enddefine;

    /* rules_part */

    pop11_need_nextreaditem("rules") -> ;

    /* rules_part */
    [] -> parser_symbols;
    ;;; start the reduction procedure
    start_reduce_p();
    ;;; read the rules
    [%  until pop11_try_nextreaditem("endrules") do
            lvars lhs = Checkr_symbol(readitem());
            if pop11_try_nextreaditem("(") then
                ;;; definition of a meta-symbol
                read_meta_rules(lhs);
            else
                read_rules(lhs);
            endif;
        enduntil;
        ;;; allow for trailing ";"
        pop11_try_nextreaditem(";") -> ;
        ;;; add rules for any meta-symbols applied
        add_meta_rules();
    %] -> parser_rules;
    ;;; finish the reduction procedure
    end_reduce_p();
    ;;; the list of symbols has been constructed backwards
    fast_ncrev(parser_symbols) -> parser_symbols;
enddefine;


/*
 *  Generating the parser
 */

define lconstant make_parser();
    lvars parser;

    ;;; use declared precedences to resolve shift/reduce conflicts
    define lconstant resolve_p(token, rule) -> choice;
        lvars token, rule, ste, token_prec, rule_prec, i, choice = false;
        if (symtab(token) ->> ste)
        and ste_type(ste) == "token"
        and (ste_plus(ste) ->> token_prec)
        then
            ;;; -token- has declared precedence
            unless symtab(rule) ->> ste then
                ;;; no declared precedence: use that of the rightmost token
                fast_for i from datalength(rule) by -1 to 1 do
                    quitif(ste_type(symtab(subscrv(i,rule)) ->> ste)
                        == "token");
                endfor;
            endunless;
            if ste_plus(ste) ->> rule_prec then
                if abs(token_prec) < abs(rule_prec)
                or abs(token_prec) = abs(rule_prec) and token_prec < 0
                then
                    token
                else
                    rule
                endif -> choice;
            endif;
        endif;
    enddefine;

    ;;; warn about any unexpected conflicts
    define lconstant report_conflicts(parser);
        lvars parser, n;
        lr_parser_sr_conflicts(parser) -> n;
        unless n == front(parser_conflicts) then
            warning(n sys_>< ' SHIFT/REDUCE CONFLICTS', [^parser_name]);
        endunless;
        lr_parser_rr_conflicts(parser) -> n;
        unless n == back(parser_conflicts) then
            warning(n sys_>< ' REDUCE/REDUCE CONFLICTS', [^parser_name]);
        endunless;
    enddefine;

    ;;; write the parser tables to the program file
    define lconstant output_tables(parser);
        lvars parser;

        ;;; redefine lr_output_pr to cope with special non-terminals
        lvars save_pr = weakref lr_output_pr;
        define dlocal weakref lr_output_pr(item);
            lvars item;
            if isias(item) or ismas(item) then
                cucharout(`'`), syspr(item), cucharout(`'`);
            else
                save_pr(item);
            endif;
        enddefine;

        valof("lr_output")([lconstant], parser_tables_name, parser,
                           parser_program_dev);
    enddefine;

    lr_build(parser_name, parser_tokens, parser_symbols, parser_start_symbol,
             parser_rules, resolve_p, option("keep")) -> parser;
    report_conflicts(parser);
    if option("report") then
        valof("lr_report")(parser, option("report"));
    endif;
    unless option("trace") or option("keep") then
        ;;; strip symbolic information to save space: if the parser's
        ;;; not referenced at all, it can be stripped fully
        lr_strip(parser, not(option("parser")));
    endunless;
    sysPASSIGN(parser, parser_tables_name);
    returnunless(parser_program_dev);
    output_tables(parser);
enddefine;


/*
 *  The PARSER define-form
 */

define:define_form global parser();
    dlocal
        parser_name,
        parser_arg_names,
        parser_input_name,
        parser_input_p_name,
        parser_reduce_p_name,
        parser_tables_name,
        parser_tokens,
        parser_symbols,
        parser_start_symbol,
        parser_rules,
        parser_conflicts,
        parser_program_dev,
        parser_default_multiplicity,
        parser_termin_token,
        weakref pop_show_code,
    ;

    ;;; initialise the options table
    define dlocal option =
        newproperty([], 16, false, "perm");
    enddefine;

    ;;; initialise the symbol table
    define dlocal symtab =
        newanyproperty([], 128, 1, 120, false, false, "perm", false, false);
    enddefine;

    dlocal local_syms = false;
    dlocal dummy_symbol_cnt = 0;

    ;;; initialise vars from options or defaults
    define lconstant setup();
        ;;; number of conflicts expected: default is 0/0
        option("conflicts") or #_< conspair(0,0) >_# -> parser_conflicts;
        ;;; multiplicty of nonterminal symbols: default is 1
        option("results") or 1 -> parser_default_multiplicity;
        ;;; name for the parser tables
        option("parser") or gen_var("parser") -> parser_tables_name;
        ;;; name for the end-of-input marker: default is "termin"
        option("termin") or "termin" -> parser_termin_token;
        ;;; optional name for the input procedure: if not set here, will be
        ;;; declared automatically by -declare_input-
        option("input") -> parser_input_name;
        ;;; optional start symbol: if not set here, will be set from the
        ;;; first symbol declared in the RULES part
        option("start") -> parser_start_symbol;
        ;;; open a device for program output if needed
        if option("program") ->> parser_program_dev then
            syscreate(parser_program_dev, 1, false) -> parser_program_dev;
            outf(';;; This program was generated by LIB * DEFINE_PARSER\n\n');
        endif;
        ;;; set -pop_show_code- if wanted, and LIB * SHOWCODE has been loaded
        if option("showcode") and testdef pop_show_code then
            true -> weakref pop_show_code;
        endif;
        ;;; generate names for other local procedures
        gen_var("input_p") -> parser_input_p_name;
        gen_var("reduce_p") -> parser_reduce_p_name;
    enddefine;

    ;;; declare and initialise the input procedure (if necessary)
    define lconstant declare_input(decl, decl_p);
        lvars w, decl, decl_p;
        returnif(parser_input_name); ;;; set from option("input")
        parser_name <> "_input" -> parser_input_name;
        unless sys_current_ident(parser_input_name)
        or lmember(parser_input_name, parser_arg_names)
        then
            decl_p(parser_input_name);
            sysPASSIGN(readitem, parser_input_name);
            if parser_program_dev then
                ;;; write out a default definition
                fast_for w in [define ^^decl] do
                    outf(w, '%p ');
                endfor;
                outf(parser_input_name,
                     '%p =\n\treaditem(%%%%);\nenddefine;\n\n');
            endif;
        endunless;
    enddefine;

    ;;; start the parser procedure
    define lconstant start_procedure(decl, decl_p, arg_decls, arg_decl_ps);
        lvars w, decl, procedure decl_p, arg_decls, arg_decl_ps;
        ;;; declare it
        decl_p(parser_name);
        ;;; start the definition
        sysPROCEDURE(parser_name, 1);
        ;;; declare arguments
        fast_for w, decl_p in parser_arg_names, arg_decl_ps do
            decl_p(w);
        endfor;
        ;;; pop the arguments
        fast_for w in rev(parser_arg_names) do
            sysPOP(w);
        endfor;
        ;;; forward declaration
        sysLCONSTANT(parser_tables_name, 0);
        ;;; these are made local syntax words to act as proper terminators
        sysLCONSTANT("enddefinitions", "syntax");
        sysPASSIGN(undef, "enddefinitions");
        sysLCONSTANT("endtokens", "syntax");
        sysPASSIGN(undef, "endtokens");
        sysLCONSTANT("endrules", "syntax");
        sysPASSIGN(undef, "endrules");
        returnunless(parser_program_dev);
        ;;; start procedure text
        fast_for w in [define ^^decl] do
            outf(w, '%p ');
        endfor;
        outf(parser_name, '%p');
        if parser_arg_names == [] then
            outf('();\n');
        else
            outf(fast_front(parser_arg_names), '(%p');
            fast_for w in fast_back(parser_arg_names) do
                outf(w, ',%p');
            endfor;
            outf(');\n');
            fast_for decl, w in arg_decls, parser_arg_names do
                outf(front(decl), '%p ');
                unless fast_back(decl) == [] then outf(decl(2), '%p ') endunless;
                outf(w, '%p;\n');
            endfor;
        endif;
        ;;; forward declaration
        outf(parser_tables_name, 'lconstant %p;\n');
    enddefine;

    ;;; complete the procedure, planting the actual call to the parser.
    ;;; We know that the parser only calls its procedure arguments, so
    ;;; we can ignore these pushes in classifying lvars.
    define lconstant end_procedure();
        dlocal pop_vm_flags = pop_vm_flags || VM_DISCOUNT_LEX_PROC_PUSHES;
        sysPUSH(parser_input_p_name), sysPUSH(parser_reduce_p_name),
            sysPUSH(parser_tables_name);
        sysCALL(if option("trace") then "lr_trace" else "lr_parse" endif);
        sysLABEL("return");
        sysPASSIGN(sysENDPROCEDURE(), parser_name);
        returnunless(parser_program_dev);
        ;;; complete procedure text
        outf('compile_mode:vm +dislpp;\n');
        outf(parser_tables_name, parser_reduce_p_name, parser_input_p_name,
            'lr_parse(%p,%p,%p);\n');
        outf('enddefine;\n');
    enddefine;

    ;;; clean up on exit
    define lconstant cleanup(context);
        lvars context;
        if context <= 2 then
            if isdevice(parser_program_dev) then
                sysclose(parser_program_dev);
            endif;
        endif;
    enddefine;

    dlocal 0 %, cleanup(dlocal_context) %;

    /* define_parser */
    lvars decl, decl_p, arg_decls, arg_decl_ps;
    declaration_part() -> (decl, decl_p, parser_name);
    argument_part() -> (arg_decls, arg_decl_ps, parser_arg_names);
    pop11_need_nextreaditem(";") -> ;
    options_part();
    setup();
    declare_input(decl, decl_p);
    start_procedure(decl, decl_p, arg_decls, arg_decl_ps);
        definitions_part();
        tokens_part();
        rules_part();
        pop11_need_nextreaditem("enddefine") -> ;
        make_parser();
    end_procedure();
enddefine;

endsection;     /* $- */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Mar 11 1994
        Added weakrefs missing from previous fix.
--- Robert John Duncan, Feb 24 1994
        Fixed program output in the presence of intermediate actions and
        meta-symbols
--- Robert John Duncan, Jun 18 1993
        Added meta-symbols
--- Robert John Duncan, Jun 15 1993
        Changed compilation of intermediate actions so that they can modify
        variables ocurring to the left
--- Robert John Duncan, Apr 14 1993
        Fixed the last fix.
--- Robert John Duncan, Feb 24 1993
        Added "return" labels to all generated procedures so that the
        return syntax works properly.
 */
