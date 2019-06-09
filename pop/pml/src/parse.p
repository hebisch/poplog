/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/pml/src/parse.p
 > Purpose:         PML: Syntax analysis
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */

section $-ml;

/*
 *  Error reporting
 */

define vars parse_error(ml_errors_continue, msg, culprits, file, linenum);
    lvars msg, culprits, file, linenum;
    dlocal ml_errors_continue;
    ml_error(msg, culprits, file, linenum);
enddefine;

define lconstant parse_fail(item, msg);
    lvars item, msg, args = [];
    if islist(msg) then
        ;;; explicit arguments to msg
        ((), item, msg) -> (item, msg, args);
    else
        [^msg] -> args;
        if isstring(msg) then
            'Expecting %S\n'
        else
            'Expecting: "%p"\n'
        endif -> msg;
    endif;
    parse_error(false, '%s\n%s',
        [%  if item == termin then
                'unexpected end of input'
            else
                'at or before %S\n\t%p',
                if isshortid(item) then
                    lookup_opr(item) and 'infix identifier'
                        or 'identifier'
                elseif islongid(item) then
                    'long identifier'
                elseif istvid(item) then
                    'type variable'
                elseif isreserved(item) then
                    'reserved word'
                elseif isscon(item) then
                    if is_int_scon(item) then
                        'integer constant'
                    elseif is_real_scon(item) then
                        'real constant'
                    else
                        'string constant'
                    endif
                else
                    'token'
                endif, item,
            endif, msg
        %   ^^args],
        popfilename, ml_linenum);
enddefine;


/*
 *  Lexical Interface
 */

;;; getitem:
;;;     the current itemiser, passed to parse

lvars procedure getitem;

;;; peekitem:
;;;     look ahead at the next item

define lconstant peekitem() -> item;
    lvars item;
    getitem() ->> item -> getitem();
enddefine;

;;; testitem:
;;;     compares the next item with item_needed. The item is removed
;;;     only if it matches.

define lconstant testitem(item_needed);
    lvars item_needed, item = getitem();
    if item == item_needed then
        true;
    else
        item -> getitem();
        false;
    endif;
enddefine;

;;; needitem:
;;;     checks that the next item matches item_needed and gives an error
;;;     if not.  substitutes is a set of items that may be mistakenly
;;;     substituted for the needed item; followers is the set of items
;;;     which can legitimately come after the needed item

define lconstant needitem(item_needed, substitutes, followers);
    lvars item_needed, substitutes, followers;

    define lconstant is_in(item, set);
        lvars item, set;
        item == set
        or islist(set) and Lmember(item, set)
        or isprocedure(set) and set(item);
    enddefine;

    lvars item = getitem();
    unless item == item_needed then
        parse_fail(item,
            if is_in(item, followers) then
                'Expecting: "%p" (may be missing)\n', [^item_needed]
            elseif is_in(item, substitutes) and is_in(peekitem(), followers)
            then
                'Expecting: "%p" ("%p" may have been used instead)\n',
                    [^item_needed ^item]
            else
                item_needed
            endif);
    endunless;
enddefine;


/*
 *  Parser utilities
 */

;;; check_label:
;;;     check that a record label's not already been used

define lconstant check_label(label, row, kind, linenum);
    lvars label, row, kind, linenum;
    if alookup(label, row) then
        parse_error(true, 'duplicate record label in %S row\n\t%p\n',
            [^kind ^label], popfilename, linenum);
    endif;
enddefine;

;;; sort_row, sort_constrs:
;;;     for placing record fields and constructor lists into canonical
;;;     order. These are non-copying.

define lconstant sort_row(/* row */) with_nargs 1;
    syssort(/* row, */ false,
        procedure(field1, field2);
            lvars field1, field2;
            label_<=(Front(field1), Front(field2));
        endprocedure);
enddefine;

define lconstant sort_constrs(/* constrs */) with_nargs 1;
    syssort(/* constrs, */ false,
        procedure(constr1, constr2);
            lvars constr1, constr2;
            alphabefore(val_name(first(constr1)), val_name(first(constr2)));
        endprocedure);
enddefine;

;;; is_tuple_row:
;;;     tests whether a type, pattern or expression row is equivalent to
;;;     a tuple, i.e. has successive numeric labels 1..n where n > 1.

define lconstant is_tuple_row(row);
    lvars row, field, cnt = 0;
    For field in row do
        cnt fi_+ 1 -> cnt;
        returnunless(Front(field) == cnt)(false);
    endfor;
    cnt fi_> 1;
enddefine;

;;; PARSE_TO:
;;;     parses a construct terminated by terminator

define :inline PARSE_TO(parse, terminator, followers);
    (parse([% terminator %]), needitem(terminator, [], followers))
enddefine;

;;; parse_seq, parse_seq_to:
;;;     parse a non-empty sequence of constructs separated by separator,
;;;     stopping at terminator

define lconstant parse_seq(parse, separator, followers);
    lvars procedure parse, separator, followers;
    lvars after_each = conspair(separator, followers);
    [%  repeat
            parse(after_each);
            quitunless(testitem(separator));
        endrepeat;
    %];
    ;;; return token set to freelist
    sys_grbg_destpair(after_each) -> (,);
enddefine;

define lconstant parse_seq_to(parse, separator, terminator, followers);
    lvars procedure parse, separator, terminator, followers;
    lvars after_each = conspair(separator, conspair(terminator, []));
    [%  repeat
            parse(after_each);
            quitunless(testitem(separator));
        endrepeat;
    %];
    needitem(terminator, [], followers);
    ;;; return token set to freelist
    sys_grbg_list(after_each);
enddefine;


;;; == Identifiers and Labels =============================================

;;; parse_shortid:
;;;     reads a short identifier

define lconstant parse_shortid(msg) -> item;
    lvars item = getitem(), msg;
    unless isshortid(item) then
        if item == RESERVED 'op' and isshortid(peekitem()) then
            getitem() -> item;
            parse_error(true, 'the "op" prefix is not allowed here\n\top %p\n',
                [^item], popfilename, ml_linenum);
        else
            parse_fail(item, msg);
        endif;
    endunless;
enddefine;

;;; parse_id:
;;;     reads an identifier (long or short)

define lconstant parse_id(msg) -> item;
    lvars item = getitem(), msg;
    unless isid(item) then
        if item == RESERVED 'op' and isid(peekitem()) then
            getitem() -> item;
            parse_error(true, 'the "op" prefix is not allowed here\n\top %p\n',
                [^item], popfilename, ml_linenum);
        else
            parse_fail(item, msg);
        endif;
    endunless;
enddefine;

;;; parse_tycon_id:
;;;     reads a type constructor name (disallowing "*").
;;;     The argument may parse a long or short id.

define lconstant parse_tycon_id(parse_id) -> item;
    lvars item, parse_id;
    parse_id('a type constructor') -> item;
    if item == "*" then
        parse_fail(item, 'a type constructor');
    endif;
enddefine;

;;; parse_nonfix_id:
;;;     reads a short identifier in a non-operator context; i.e. it must
;;;     either be a nonfix identifier or else prefixed with "op"

define lconstant parse_nonfix_id(msg) -> item;
    lvars item = getitem(), msg;
    if isshortid(item) then
        if lookup_opr(item) then
            parse_error(true,
                'the "op" prefix is required before infix identifier\n\t%p\n',
                [^item], popfilename, ml_linenum);
        endif;
    elseif item == RESERVED 'op' then
        getitem() -> item;
        unless isshortid(item) then
            parse_fail(item, 'an infix identifier, following "op"');
        endunless;
    elseif msg then
        parse_fail(item, msg);
    else
        item -> getitem();
        false -> item;
    endif;
enddefine;

;;; parse_opr:
;;;     parses an operator in an infix pattern or expression

define lconstant parse_opr(prevop, in_exp);
    lvars prevop, in_exp;
    lvars item = getitem(), id, thisop;
    if isshortid(item ->> id)
    or item == RESERVED '=' and ("=" -> id, in_exp)
    then
        if lookup_opr(id) ->> thisop then
            if opr_prec(thisop) fi_> opr_prec(prevop)
            or opr_name(prevop) == id and opr_fixity(prevop) == "infixr"
            then
                return(thisop);
            endif;
        endif;
    endif;
    item -> getitem();
    false;
enddefine;

;;; parse_label:
;;;     parses a record label, or the record wildcard symbol if -in_pat-
;;;     is true.

define lconstant parse_label(in_pat) -> label;
    lvars in_pat, label, item = getitem();
    unless is_label(item) ->> label then
        unless item == RESERVED '...' then
            parse_fail(item, 'a record label');
        elseunless in_pat then
            parse_fail(item,
                'A record wildcard is only allowed in a pattern row\n', []);
        endunless;
        item -> label;
    endunless;
enddefine;


;;; == Type Expressions ===================================================

constant procedure parse_tyexp;

define lconstant is_tyexp_opener(item);
    lvars item;
    if istvid(item) then
        true;
    elseif isid(item) then
        item /== "*";
    elseif item == RESERVED '('
        or item == RESERVED '{'
    then
        true;
    else
        false;
    endif;
enddefine;

define lconstant parse_record_tyexp(followers);
    lvars followers, tyrow = [], linenum = ml_linenum;
    returnif(testitem(RESERVED '}'))(mkRecordTyexp(linenum, [], []));
    lconstant after_tyexp = [% RESERVED ',', RESERVED '}' %];
    repeat
        lvars label = parse_label(false);
        needitem(RESERVED ':', RESERVED '=', is_tyexp_opener);
        check_label(label, tyrow, 'type-expression', ml_linenum);
        acons(label, parse_tyexp(after_tyexp), tyrow) -> tyrow;
        quitunless(testitem(RESERVED ','));
    endrepeat;
    needitem(RESERVED '}', [], followers);
    if is_tuple_row(sort_row(tyrow) ->> tyrow) then
        mkTupleTyexp(linenum, map(tyrow, Back));
    else
        mkRecordTyexp(linenum, map(tyrow, Front), map(tyrow, Back));
    endif;
enddefine;

define lconstant parse_con_tyexp(followers) -> tyexp;
    lvars followers, item = getitem(), tyexp, tyexps;
    if istvid(item) then
        ;;; type variable
        mkVarTyexp(ml_linenum, item) -> tyexp;
        set_tyvar(tyexp);
    elseif isid(item) and item /== "*" then
        ;;; nullary type constructor
        mkConsTyexp(ml_linenum, item, []) -> tyexp;
        set_tycon(tyexp);
    elseif item == RESERVED '(' then
        ;;; either  `(ty)'  or  `(ty, __, ty) tycon'
        lconstant after_tyexp = [% RESERVED ',', RESERVED ')' %];
        parse_tyexp(after_tyexp) -> tyexp;
        if testitem(RESERVED ',') then
            ;;; postfix type constructor with 2 or more arguments
            conspair(tyexp,
                parse_seq_to(parse_tyexp, RESERVED ',', RESERVED ')', [])
            ) -> tyexps;
            parse_tycon_id(parse_id) -> item;
            mkConsTyexp(ml_linenum, item, tyexps) -> tyexp;
            set_tycon(tyexp);
        else
            needitem(RESERVED ')', [], followers);
        endif;
    elseif item == RESERVED '{' then
        parse_record_tyexp(followers) -> tyexp;
    else
        parse_fail(item, 'a type-expression');
    endif;
    while isid(getitem() ->> item) and item /== "*" do
        mkConsTyexp(ml_linenum, item, [^tyexp]) -> tyexp;
        set_tycon(tyexp);
    endwhile;
    item -> getitem();
enddefine;

define lconstant parse_tuple_tyexp(followers) -> tyexp;
    lvars followers, tyexp = parse_con_tyexp(followers);
    if testitem("*") then
        mkTupleTyexp(ml_linenum, conspair(tyexp,
            parse_seq(parse_con_tyexp, "*", followers))) -> tyexp;
    endif;
enddefine;

define parse_tyexp(followers) -> tyexp;
    lvars followers, tyexp = parse_tuple_tyexp(followers);
    if testitem(RESERVED '->') then
        mkFunTyexp(ml_linenum, tyexp, parse_tyexp(followers)) -> tyexp;
    endif;
enddefine;


;;; == Patterns ===========================================================

constant procedure parse_pat;

lconstant PAT_OPENERS = [%
    RESERVED '{',
    RESERVED '(',
    RESERVED '[',
%];

define lconstant is_pat_opener(item);
    lvars item;
    if isscon(item) then
        true;
    elseif isid(item) then
        true;
    else
        Lmember(item, PAT_OPENERS);
    endif;
enddefine;

lvars
    infun = false,
        ;;; flag indicating whether we're parsing the LHS of a "fun":
        ;;; may allow one infixed variable if so. This is to cover the
        ;;; horrible infix function declarations.
;

define lconstant mkIdPat(linenum, id) -> pat;
    lvars linenum, id, pat, con;
    if lookup_con(id) ->> con then
        if val_isexn(con) then
            mkExnPat(linenum, con) -> pat;
        else
            mkConPat(linenum, con) -> pat;
        endif;
    elseif isshortid(id) then
        mkVarPat(linenum, declare_var(id, linenum)) -> pat;
    else
        ;;; long identifier -- ought to be a constructor
        mkConPat(linenum, id) -> pat;
        ;;; this call will raise an 'unbound constructor' error
        set_con(pat);
    endif;
enddefine;

define lconstant mkAppPat(linenum, id, pat) -> pat;
    lvars linenum, id, pat, con;
    if lookup_con(id) ->> con then
        if val_isexn(con) then
            mkExnAppPat(linenum, con, pat) -> pat;
        else
            mkConAppPat(linenum, con, pat) -> pat;
        endif;
    else
        mkConAppPat(linenum, id, pat) -> pat;
        if infun == true and isshortid(id) and lookup_opr(id) then
            ;;; possible infix function name: record the construction in
            ;;; -infun-, leaving -id- as its constructor
            pat -> infun;
        else
            ;;; force an error
            set_con(pat);
        endif;
    endif;
enddefine;

define lconstant parse_record_pat(followers);
    lvars followers, patrow = [], wildcard = false, linenum = ml_linenum;
    returnif(testitem(RESERVED '}'))(mkUnitPat(ml_linenum));
    lconstant after_pat = [% RESERVED ',', RESERVED '}' %];
    repeat
        lvars label = parse_label(true), label_linenum = ml_linenum, pat;
        if label == RESERVED '...' then
            ;;; must be the last thing in the row
            if testitem(RESERVED ',') then
                parse_fail(label,
                    'A record wildcard is only allowed at the end of a pattern row\n',
                    []);
            endif;
            true -> wildcard;
            quitloop;
        elseif testitem(RESERVED '=') then
            parse_pat(after_pat) -> pat;
        elseif isshortid(label) then
            ;;; var << : ty >> << as pat >>
            mkIdPat(label_linenum, label) -> pat;
            lvars constraint = testitem(RESERVED ':')
                and parse_tyexp(RESERVED 'as' :: after_pat);
            if testitem(RESERVED 'as') then
                lvars var = first(pat);
                if isvar(var) then
                    parse_pat(after_pat) -> pat;
                    if constraint then
                        mkTypedPat(label_linenum, pat, constraint) -> pat;
                    endif;
                    mkLayeredPat(label_linenum, var, pat) -> pat;
                else
                    ;;; must be a constructor/exception
                    parse_error(true,
                        'the identifier in a layered pattern must be a variable\n\t%p\n',
                        [% entry_name(var) %], popfilename, label_linenum);
                endif;
            elseif constraint then
                mkTypedPat(label_linenum, pat, constraint) -> pat;
            endif;
        else
            needitem(RESERVED '=', RESERVED ':', is_pat_opener);
        endif;
        check_label(label, patrow, 'pattern', label_linenum);
        acons(label, pat, patrow) -> patrow;
    quitunless(testitem(RESERVED ','));
    endrepeat;
    needitem(RESERVED '}', [], followers);
    sort_row(patrow) -> patrow;
    if wildcard then
        mkWRecordPat(linenum, map(patrow, Front), map(patrow, Back), []);
    elseif is_tuple_row(patrow) then
        mkTuplePat(linenum, map(patrow, Back));
    else
        mkRecordPat(linenum, map(patrow, Front), map(patrow, Back));
    endif;
enddefine;

define lconstant try_parse_apat(followers);
    lvars followers, item = getitem(), linenum = ml_linenum;
    if isscon(item) then
        mkConstPat(linenum, item);
    elseif isshortid(item) then
        returnunless(lookup_opr(item))(mkIdPat(linenum, item));
        item -> getitem();
        false;
    elseif islongid(item) then
        mkIdPat(linenum, item);
    elseif item == RESERVED 'op' then
        parse_id('an infix identifier, following "op"') -> item;
        mkIdPat(ml_linenum, item);
    elseif item == RESERVED '_' then
        mkWildCardPat(linenum);
    elseif item == RESERVED '(' then
        returnif(testitem(RESERVED ')'))(mkUnitPat(linenum));
        lconstant after_pat = [% RESERVED ',', RESERVED ')' %];
        lvars pat = parse_pat(after_pat);
        if testitem(RESERVED ',') then
            mkTuplePat(linenum, conspair(pat,
                parse_seq_to(parse_pat, RESERVED ',', RESERVED ')', followers)));
        else
            needitem(RESERVED ')', [], followers);
            pat;
        endif;
    elseif item == RESERVED '[' then
        returnif(testitem(RESERVED ']'))(mkConPat(linenum, first(nil_node)));
        mkListPat(linenum,
            parse_seq_to(parse_pat, RESERVED ',', RESERVED ']', followers));
    elseif item == RESERVED '{' then
        parse_record_pat(followers);
    else
        item -> getitem();
        false;
    endif;
enddefine;

define lconstant parse_infix_pat(prevop, followers) -> pat;
    lvars prevop, followers, pat;
    lvars item = getitem();
    if isshortid(item) and not(lookup_opr(item))
    or islongid(item)
    or item == RESERVED 'op'
        and (parse_id('an infix identifier, following "op"') ->> item)
    then
        lvars linenum = ml_linenum;
        if try_parse_apat(followers) ->> pat then
            mkAppPat(linenum, item, pat) -> pat;
        else
            mkIdPat(linenum, item) -> pat;
        endif;
    else
        item -> getitem();
        unless try_parse_apat(followers) ->> pat then
            parse_fail(item,
                if opr_prec(prevop) >= 0 then
                    ;;; after an operator
                    if isshortid(item) then
                        ;;; must be infix itself
                        'Argument to operator (%p) cannot start with another operator\n'
                            <> 'Use the "op" prefix: op %p\n',
                            [% opr_name(prevop), item %]
                    else
                        'Expecting an argument to operator: %p\n',
                            [% opr_name(prevop) %]
                    endif;
                elseif isshortid(item) then
                    'Expecting the start of a pattern\n'
                        <> 'Use the "op" prefix: op %p\n', [^item]
                else
                    'the start of a pattern'
                endif);
        endunless;
    endif;
    ;;; infixed construction
    lvars thisop;
    while parse_opr(prevop, false) ->> thisop do
        mkAppPat(ml_linenum, opr_name(thisop),
            mkTuplePat(ml_linenum,
                [% pat, parse_infix_pat(thisop, followers) %])) -> pat;
    endwhile;
enddefine;

define lconstant parse_typed_pat(followers) -> pat;
    lvars followers, pat = parse_infix_pat(nonfix_opr, followers);
    while testitem(RESERVED ':') do
        mkTypedPat(ml_linenum, pat,
            parse_tyexp(RESERVED ':' :: followers)) -> pat;
    endwhile;
enddefine;

define parse_pat(followers) -> pat;
    lvars followers, pat = parse_typed_pat(followers);
    if testitem(RESERVED 'as') then
        lvars idpat = isTypedPat(pat) and first(pat) or pat;
        if isVarPat(idpat) then
            mkLayeredPat(nodeline(idpat), first(idpat),
                if isTypedPat(pat) then
                    mkTypedPat(nodeline(pat), parse_pat(followers),
                        second(pat))
                else
                    parse_pat(followers)
                endif) -> pat;
        elseif isConPat(idpat) or isExnPat(idpat) then
            parse_error(true,
                'the identifier in a layered pattern must be a variable\n\t%p\n',
                [% entry_name(first(idpat)) %], popfilename, nodeline(idpat));
        else
            parse_fail(RESERVED 'as',
                'A layered pattern is only allowed after a (typed) variable\n',
                []);
        endif;
    endif;
enddefine;


;;; == Expressions ========================================================

constant procedure ( parse_exp, parse_decs );

lconstant EXP_OPENERS = [%
    RESERVED '{',
    RESERVED '(',
    RESERVED '[',
    RESERVED '#',
    RESERVED 'let',
    RESERVED 'raise',
    RESERVED 'if',
    RESERVED 'while',
    RESERVED 'case',
    RESERVED 'fn',
%];

define lconstant is_exp_opener(item);
    lvars item;
    if isscon(item) then
        true;
    elseif isid(item) then
        true;
    else
        Lmember(item, EXP_OPENERS);
    endif;
enddefine;

define lconstant mkIdExp(linenum, id) -> e;
    lvars id, linenum, e, con;
    if lookup_con(id) ->> con then
        if val_isexn(con) then
            mkExnExp(linenum, con) -> e;
        else
            mkConExp(linenum, con) -> e;
        endif;
    else
        mkVarExp(linenum, id) -> e;
        set_var(e);
    endif;
enddefine;

define lconstant parse_record_exp(followers);
    lvars followers, exprow = [], linenum = ml_linenum;
    returnif(testitem(RESERVED '}'))(mkUnitExp(linenum));
    lconstant after_exp = [% RESERVED ',', RESERVED '}' %];
    repeat
        lvars label = parse_label(false);
        needitem(RESERVED '=', RESERVED ':', is_exp_opener);
        check_label(label, exprow, 'expression', ml_linenum);
        acons(label, parse_exp(after_exp), exprow) -> exprow;
    quitunless(testitem(RESERVED ','));
    endrepeat;
    needitem(RESERVED '}', [], followers);
    if is_tuple_row(sort_row(exprow) ->> exprow) then
        mkTupleExp(linenum, map(exprow, Back));
    else
        mkRecordExp(linenum, map(exprow, Front), map(exprow, Back));
    endif;
enddefine;

define lconstant try_parse_aexp(followers);
    lvars followers, item = getitem(), linenum = ml_linenum;
    if isscon(item) then
        mkConstExp(linenum, item);
    elseif isshortid(item) then
        returnunless(lookup_opr(item))(mkIdExp(linenum, item));
        item -> getitem();
        false;
    elseif islongid(item) then
        mkIdExp(linenum, item);
    elseif item == RESERVED '=' then
        returnunless(lookup_opr("="))(mkIdExp(linenum, "="));
        item -> getitem();
        false;
    elseif item == RESERVED 'op' then
        if isid(getitem() ->> item) then
            mkIdExp(ml_linenum, item);
        elseif item == RESERVED '=' then
            mkIdExp(ml_linenum, "=")
        else
            parse_fail(item, 'an infix identifier, following "op"');
        endif;
    elseif item == RESERVED '(' then
        returnif(testitem(RESERVED ')'))(mkUnitExp(linenum));
        lconstant after_exp = [% RESERVED ',', RESERVED ';', RESERVED ')' %];
        lvars e = parse_exp(after_exp);
        if testitem(RESERVED ',') then
            mkTupleExp(linenum, conspair(e,
                parse_seq_to(parse_exp, RESERVED ',', RESERVED ')', followers)));
        elseif testitem(RESERVED ';') then
            mkSeqExp(linenum, conspair(e,
                parse_seq_to(parse_exp, RESERVED ';', RESERVED ')', followers)));
        else
            needitem(RESERVED ')', [], followers);
            e;
        endif;
    elseif item == RESERVED '[' then
        returnif(testitem(RESERVED ']'))(mkConExp(linenum, first(nil_node)));
        mkListExp(linenum, parse_seq_to(parse_exp, RESERVED ',', RESERVED ']',
            followers));
    elseif item == RESERVED '{' then
        parse_record_exp(followers);
    elseif item == RESERVED '#' then
        mkSelectorExp(linenum, parse_label(false), []);
    elseif item == RESERVED 'let' then
        begin_scope();
        mkLetExp(linenum,
            parse_decs(false, RESERVED 'in', EXP_OPENERS),
            mkSeqExp(ml_linenum,
                parse_seq_to(parse_exp, RESERVED ';', RESERVED 'end', followers)));
        end_scope();
    else
        item -> getitem();
        false;
    endif;
enddefine;

define lconstant parse_infix_exp(prevop, followers) -> e;
    lvars prevop, followers, e;
    unless try_parse_aexp(followers) ->> e then
        lvars item = getitem();
        parse_fail(item,
            if opr_prec(prevop) >= 0 then
                ;;; after an operator
                if isshortid(item) then
                    ;;; must be infix itself
                    'Argument to operator (%p) cannot start with another operator\n'
                        <> 'Use the "op" prefix: op %p\n',
                        [% opr_name(prevop), item %]
                elseif is_exp_opener(item) then
                    ;;; can start an expression, but not an atomic one:
                    ;;; i.e., needs bracketing in this context
                    'Argument to operator (%p) cannot start with "%p"\n'
                        <> 'Use parentheses: (%p __)\n', [%opr_name(prevop), item, item%]
                else
                    'Expecting an argument to operator: %p\n',
                        [% opr_name(prevop) %]
                endif;
            elseif isshortid(item) then
                'Expecting the start of an expression\n'
                    <> 'Use the "op" prefix: op %p\n', [^item]
            elseif item == RESERVED 'local' then
                'Expecting the start of an expression;\n'
                <> '"local" may have been used instead of "let"\n', []
            else
                'the start of an expression'
            endif);
    endunless;
    ;;; application
    lvars arg, linenum = ml_linenum;
    while try_parse_aexp(followers) ->> arg do
        mkAppExp(linenum, e, arg) -> e;
        ml_linenum -> linenum;
    endwhile;
    ;;; infixed application
    lvars thisop;
    while parse_opr(prevop, true) ->> thisop do
        mkAppExp(ml_linenum,
            mkIdExp(ml_linenum, opr_name(thisop)),
            mkTupleExp(ml_linenum, [% e, parse_infix_exp(thisop, followers) %])
        ) -> e;
    endwhile;
enddefine;

define lconstant parse_typed_exp(followers) -> e;
    lvars followers, e = parse_infix_exp(nonfix_opr, followers);
    while testitem(RESERVED ':') do
        lvars after_tyexp = [%
            RESERVED 'handle',
            RESERVED 'andalso',
            RESERVED 'orelse',
            RESERVED ':'
        %   ^^followers];
        mkTypedExp(ml_linenum, e, parse_tyexp(after_tyexp)) -> e;
    endwhile;
enddefine;

define lconstant parse_andalso_exp(followers) -> e;
    lvars followers, e = parse_typed_exp(followers);
    while testitem(RESERVED 'andalso') do
        mkAndExp(ml_linenum, e, parse_typed_exp(followers)) -> e;
    endwhile;
    if peekitem() == RESERVED 'and'
    and not(Lmember(RESERVED 'and', followers))
    then
        ;;; a common error is to use and in place of andalso
        parse_fail(RESERVED 'and',
            '"and" may have been used instead of "andalso"\n', []);
    endif;
enddefine;

define lconstant parse_orelse_exp(followers) -> e;
    lvars followers, e = parse_andalso_exp(followers);
    while testitem(RESERVED 'orelse') do
        mkOrExp(ml_linenum, e, parse_andalso_exp(followers)) -> e;
    endwhile;
enddefine;

define lconstant parse_rule(followers);
    lvars followers, pat;
    lconstant after_pat = [% RESERVED '=>' %];
    begin_rule();
    parse_pat(after_pat) -> pat;
    needitem(RESERVED '=>', RESERVED '=', is_exp_opener);
    mkRule(ml_linenum, pat, parse_exp(followers));
    end_rule();
enddefine;

define lconstant parse_match(followers);
    lvars followers;
    mkMatch(ml_linenum, parse_seq(parse_rule, RESERVED '|', followers));
enddefine;

define parse_exp(followers);
    lvars followers, item = getitem();
    if item == RESERVED 'if' then
        mkIfExp(ml_linenum,
            PARSE_TO(parse_exp, RESERVED 'then', EXP_OPENERS),
            PARSE_TO(parse_exp, RESERVED 'else', EXP_OPENERS),
            parse_exp(followers));
    elseif item == RESERVED 'while' then
        mkWhileExp(ml_linenum,
            PARSE_TO(parse_exp, RESERVED 'do', EXP_OPENERS),
            parse_exp(followers));
    elseif item == RESERVED 'case' then
        mkCaseExp(ml_linenum,
            PARSE_TO(parse_exp, RESERVED 'of', EXP_OPENERS),
            parse_match(followers));
    elseif item == RESERVED 'fn' then
        mkFnExp(ml_linenum, parse_match(followers));
    elseif item == RESERVED 'fun'
    and not(Lmember(item, followers))
    and is_pat_opener(peekitem())
    then
        parse_fail(item,
            'Expecting the start of an expression;\n"fun" may have been used instead of "fn"\n',
            []);
    elseif item == RESERVED 'raise' then
        mkRaiseExp(ml_linenum, parse_exp(followers));
    else
        item -> getitem();
        lvars e = parse_orelse_exp(followers);
        if testitem(RESERVED 'handle') then
            mkHandleExp(ml_linenum, e, parse_match(followers)) -> e;
        endif;
        e;
    endif;
enddefine;


;;; == Structure Expressions ==============================================

constant procedure parse_sigexp;

define lconstant is_strexp_opener(item);
    lvars item;
    if isid(item) then
        true;
    else
        item == RESERVED 'struct' or item == RESERVED 'let';
    endif;
enddefine;

;;; parse_strexp:
;;;     returns a structure environment as well as a syntax tree

define lconstant parse_strexp(followers) -> (strexp, struct);
    lvars item = getitem(), followers, strexp, struct;
    if isshortid(item) and testitem(RESERVED '(') then
        ;;; functor application
        mkAppStrexp(ml_linenum, item, false) -> strexp;
        if is_strexp_opener(peekitem()) then
            lconstant after_actual = [% RESERVED ')' %];
            parse_strexp(after_actual) -> (second(strexp),);
            needitem(RESERVED ')', [], followers);
        else
            begin_struct();
            mkGenStrexp(ml_linenum, false,
                parse_decs(true, RESERVED ')', followers)) -> second(strexp);
            end_struct() -> first(second(strexp));
        endif;
        set_fnc(strexp);
        ;;; every occurrence of a named functor generates a unique instance
        functor_instance(first(strexp)) -> first(strexp);
        fnc_env(first(strexp)) -> struct;
    elseif isid(item) then
        mkIdStrexp(ml_linenum, item) -> strexp;
        set_str(strexp);
        str_env(first(strexp)) -> struct;
    elseif item == RESERVED 'struct' then
        begin_struct();
        mkGenStrexp(ml_linenum, false,
            parse_decs(true, RESERVED 'end', followers)) -> strexp;
        end_struct() ->> struct -> first(strexp);
    elseif item == RESERVED 'let' then
        lconstant after_strexp = [% RESERVED 'end' %];
        begin_scope();
        mkLetStrexp(ml_linenum, parse_decs(true, RESERVED 'in', followers),
            parse_strexp(after_strexp) -> struct) -> strexp;
        needitem(RESERVED 'end', [], followers);
        end_scope();
    else
        parse_fail(item, 'a structure expression');
    endif;
enddefine;


;;; == Declarations =======================================================

lconstant DEC_OPENERS = [%
    RESERVED 'val',
    RESERVED 'fun',
    RESERVED 'type',
    RESERVED 'datatype',
    RESERVED 'abstype',
    RESERVED 'exception',
    RESERVED 'structure',
    RESERVED 'abstraction',
    RESERVED 'local',
    RESERVED 'open',
    RESERVED 'infix',
    RESERVED 'infixr',
    RESERVED 'nonfix',
%];

define lconstant parse_valbinds(followers);
    lvars followers, pat;
    repeat
        returnif(testitem(RESERVED 'rec'))(true);
        lvars pat = PARSE_TO(parse_pat, RESERVED '=', EXP_OPENERS);
        mkValBind(ml_linenum, pat, parse_exp(followers));
        returnunless(testitem(RESERVED 'and'))(false);
    endrepeat;
enddefine;

define lconstant parse_recvalbinds(followers);
    lvars followers;
    begin_valrec();
    repeat
        while testitem(RESERVED 'rec') do /* nothing */ endwhile;
        lvars pat = PARSE_TO(parse_pat, RESERVED '=', EXP_OPENERS);
        lvars linenum = ml_linenum;
        lvars e = parse_exp(followers);
        mkValBind(linenum, pat, e);
        ;;; check constraint on RHS of val rec
        while isTypedExp(e) do first(e) -> e endwhile;
        unless isFnExp(e) then
            parse_error(true,
                'the right-hand-side of a "val rec" binding must have the form\n\t%S\n',
                ['fn <match>'], popfilename, linenum);
        endunless;
        quitunless(testitem(RESERVED 'and'));
    endrepeat;
    end_valrec();
enddefine;

define lconstant parse_valdec(followers);
    lvars followers, linenum = ml_linenum;
    lvars after_bind = conspair(RESERVED 'and', followers);
    begin_valdec();
    lvars rec, vbs = [% parse_valbinds(after_bind) -> rec %];
    lvars recvbs = [% if rec then parse_recvalbinds(after_bind) endif %];
    lvars (tyvars, varbinds) = end_valdec();
    mkValDec(linenum, varbinds, tyvars, vbs, recvbs);
    ;;; return token set to the freelist
    sys_grbg_destpair(after_bind) -> (,);
enddefine;

define lconstant parse_funbind(followers);
    lvars followers;

    dlvars name, arity, clause_num = 0;
    define lconstant parse_clause(followers);
        lvars followers;

            ;;; parse_formal: parse exactly one atomic argument
        define lconstant parse_formal(followers) -> formal;
            lvars followers, formal;
            unless try_parse_apat(followers) ->> formal then
                parse_fail(getitem(), 'a function formal parameter');
            endunless;
        enddefine;

            ;;; parse_formals: parse zero or more atomic arguments
        define lconstant parse_formals(followers);
            lvars followers;
            [% while dup(try_parse_apat(followers)) do endwhile -> %];
        enddefine;

        lconstant
            after_formals    = [% RESERVED '=', RESERVED ':' %],
            after_formal     = [^^after_formals ^^PAT_OPENERS],
            after_constraint = [% RESERVED '=' % ^^EXP_OPENERS],
        ;

        begin_rule();

        lvars id = parse_nonfix_id(false), linenum = ml_linenum, formals;
        if id then
            lvars item = peekitem();
            if isshortid(item) and lookup_opr(item) then
                ;;; fun x id y = ...
                lvars x = mkIdPat(linenum, id);
                getitem() -> id;
                lvars y = parse_formal(after_formals);
                [% mkTuplePat(linenum, [^x ^y]) %] -> formals;
            elseif (parse_formals(after_formal) ->> formals) == [] then
                parse_formal(after_formals) -> ;    ;;; forces an error
            endif;
        else
            lvars x, y;
            dlocal infun = true;
            unless try_parse_apat(after_formal) ->> x then
                parse_fail(getitem(), 'a function name');
            endunless;
            infun -> y; false -> infun;
            if y /== true then
                ;;; infix identifier was seen -- must be the outermost
                ;;; constructor
                unless x == y then set_con(y) endunless;
                first(x) -> id;
                conspair(second(x), parse_formals(after_formal)) -> formals;
            else
                getitem() -> id;
                unless isshortid(id) and lookup_opr(id) then
                    parse_fail(id, 'an infix function name');
                endunless;
                parse_formal(after_formals) -> y;
                [% mkTuplePat(linenum, [^x ^y]) %] -> formals;
            endif;
        endif;

        lvars constraint = false;
        if testitem(RESERVED ':') then
            parse_tyexp(after_constraint) -> constraint;
        elseif isshortid(peekitem() ->> item) and lookup_opr(item) then
            parse_fail(item, 'Expecting a function formal parameter\n%s\n', [%
                if lookup_con(item) then
                    'For an infixed construction, use parentheses: (__ %p __)'
                else
                    'For an infix parameter, use the "op" prefix: op %p'
                endif, item
            %]);
        endif;
        needitem(RESERVED '=', RESERVED '=>', is_exp_opener);
        lvars body = parse_exp(followers);

        end_rule();

        clause_num fi_+ 1 -> clause_num;
        if clause_num == 1 then
            ;;; first clause: check the function name is a variable
            if lookup_con(id) ->> name then
                parse_error(true, '%S rebound as function name\n\t%p\n', [%
                    if val_isexn(name) then
                        'exception constructor'
                    else
                        'constructor'
                    endif, id
                %], popfilename, linenum);
            endif;
            declare_var(id, linenum) -> name;
            listlength(formals) -> arity;
        else
            ;;; subsequent clauses: check name and arity match
            unless id == val_name(name) then
                parse_error(false,
                    'in definition of function\n\t%p\nClause %p binds a different name\n\t%p\n',
                    [^(val_name(name)) ^clause_num ^id], popfilename, linenum);
            elseunless listlength(formals) == arity then
                parse_error(false,
                    'in definition of function\n\t%p\nClause %p has a different number of arguments\n',
                    [^id ^clause_num], popfilename, linenum);
            endunless;
        endif;

        mkClause(linenum, formals, body, constraint);
    enddefine;

    lvars linenum = ml_linenum;
    lvars clauses = parse_seq(parse_clause, RESERVED '|', followers);
    mkFunBind(linenum, name, arity, clauses);
enddefine;

define lconstant parse_fundec(followers);
    lvars followers, linenum = ml_linenum;
    begin_fundec();
    lvars fbs = parse_seq(parse_funbind, RESERVED 'and', followers);
    lvars (tyvars, varbinds) = end_fundec();
    mkFunDec(linenum, varbinds, tyvars, fbs);
enddefine;

define lconstant parse_tyvar_seq();
    lvars item = getitem();
    if istvid(item) then
        [% declare_tyvar(item, ml_linenum) %];
    elseif item == RESERVED '(' then
        parse_seq_to(
            procedure(followers);
                lvars followers, item = getitem();
                unless istvid(item) then
                    parse_fail(item, 'a type variable');
                endunless;
                declare_tyvar(item, ml_linenum);
            endprocedure,
            RESERVED ',', RESERVED ')', []);
    else
        item -> getitem();
        [];
    endif;
enddefine;

define lconstant parse_tycon_bind() -> (tycon, tyvars);
    lvars tyvars = parse_tyvar_seq();
    lvars tycon = declare_tycon(parse_tycon_id(parse_shortid), ml_linenum);
enddefine;

define lconstant parse_typebind(followers);
    lvars followers;
    begin_typebind();
    lvars (tycon, tyvars) = parse_tycon_bind();
    lvars linenum = ml_linenum;
    needitem(RESERVED '=', [], is_tyexp_opener);
    lvars tyexp = parse_tyexp(followers);
    end_typebind();
    mkTypeBind(linenum, tycon, tyvars, tyexp);
enddefine;

define lconstant parse_conbinds(parse_con, followers);
    dlvars procedure parse_con, followers;
    sort_constrs(parse_seq(
        procedure(followers);
            lvars followers, id = parse_con('a constructor');
            mkConBind(ml_linenum,
                declare_con(id, ml_linenum),
                testitem(RESERVED 'of') and parse_tyexp(followers))
        endprocedure,
        RESERVED '|', followers));
enddefine;

define lconstant parse_databind(followers);
    lvars followers;
    begin_typebind();
    lvars (tycon, tyvars) = parse_tycon_bind();
    lvars linenum = ml_linenum;
    needitem(RESERVED '=', [], isshortid);
    lvars constrs = parse_conbinds(parse_nonfix_id, followers);
    ;;; include a list of constructors in the type constructor
    map(constrs, first) ->> tycon_orig_cons(tycon) -> tycon_cons(tycon);
    end_typebind();
    mkDataBind(linenum, tycon, tyvars, constrs);
enddefine;

define lconstant parse_withtype(followers);
    lvars followers;
    returnunless(testitem(RESERVED 'withtype'))([]);
    begin_withtype();
    parse_seq(parse_typebind, RESERVED 'and', followers);
    end_withtype();
enddefine;

define lconstant parse_typedec(followers);
    lvars followers, linenum = ml_linenum;
    begin_typedec();
    lvars tbs = parse_seq(parse_typebind, RESERVED 'and', followers);
    lvars tycons = end_typedec();
    mkTypeDec(linenum, tycons, tbs);
enddefine;

define lconstant parse_datatypedec(followers);
    lvars followers, linenum = ml_linenum;
    begin_datatypedec();
    lvars dbs = parse_seq(parse_databind, RESERVED 'and', followers);
    lvars tbs = parse_withtype(followers);
    lvars (tycons, cons) = end_datatypedec();
    mkDatatypeDec(linenum, tycons, cons, dbs, tbs);
enddefine;

define lconstant parse_abstypedec(followers);
    lvars followers, linenum = ml_linenum;
    lconstant after_dec = [% RESERVED 'with' %];
    lvars datatypedec = parse_datatypedec(after_dec);
    needitem(RESERVED 'with', RESERVED 'in', DEC_OPENERS);
    lvars tycons = begin_with();
    lvars decs = parse_decs(false, RESERVED 'end', followers);
    end_with(tycons);
    mkAbstypeDec(linenum, tycons, datatypedec, decs);
enddefine;

define lconstant parse_exnbind(followers);
    lvars followers;
    lvars exn = declare_exn(parse_nonfix_id('an exception constructor'),
        ml_linenum);
    mkExnBind(ml_linenum, exn,
        if testitem(RESERVED 'of') then
            parse_tyexp(followers), false
        elseif testitem(RESERVED '=') then
            testitem(RESERVED 'op') -> ;
            false, get_exn(parse_id('an exception constructor'), ml_linenum)
        else
            false, false
        endif);
enddefine;

define lconstant parse_exceptiondec(followers);
    lvars followers, linenum = ml_linenum;
    begin_exceptiondec();
    lvars ebs = parse_seq(parse_exnbind, RESERVED 'and', followers);
    lvars exns = end_exceptiondec();
    mkExceptionDec(linenum, exns, ebs);
enddefine;

define lconstant parse_opendec(followers);
    lvars followers, item;
    mkOpenDec(ml_linenum, [%
        while isid(getitem() ->> item) do
            lvars str = get_str(item, ml_linenum);
            open(str_env(str));
            str;
        endwhile
    %]);
    item -> getitem();
enddefine;

define lconstant parse_fixity_directive(fix, followers);
    lvars fix, followers;
    ;;; Check for the optional precedence; default is 0
    lvars item = getitem(), prec = 0, linenum = ml_linenum;
    if fix == "nonfix" then
        ;;; turn off operator status
        -1 -> prec;
    elseif isscon(item) and is_int_scon(item) then
        scon_value(item) -> prec;
        unless 0 fi_<= prec and prec fi_<= 9 then
            parse_error(false,
                'operator precedence must be between 0 and 9\n\t%p %p\n',
                [^fix ^item], popfilename, ml_linenum);
        endunless;
        getitem() -> item;
    endif;
    lvars ids = [%
        repeat
            if item == RESERVED '=' then "=" -> item endif;
            quitunless(isshortid(item));
            item, getitem() -> item;
        endrepeat;
    %];
    ;;; At least one identifier must appear
    if ids == [] then parse_fail(item, 'an identifier') endif;
    item -> getitem();
    declare_oprs(ids, fix, prec);
    mkDirective(linenum, fix, prec, ids);
enddefine;

define lconstant parse_strbind(followers);
    lvars followers;
    lvars str = declare_str(parse_shortid('a structure name'), ml_linenum);
    lvars linenum = ml_linenum;
    begin_modbind();
    lvars sigexp = false, sig;
    if testitem(RESERVED ':') then
        lconstant after_sigexp = [% RESERVED '=' %];
        parse_sigexp(after_sigexp) -> (sigexp, sig);
    endif;
    needitem(RESERVED '=', [], is_strexp_opener);
    lvars (strexp, struct) = parse_strexp(followers);
    end_modbind();
    if not(sigexp)
    or structenv_signame(struct)
    and structenv_signame(struct) == structenv_signame(sig)
    then
        ;;; no signature, or the same signature on both sides:
        ;;; use the structure environment unchanged
        struct -> str_env(str);
    else
        ;;; signature match needed
        sig -> str_env(str);
    endif;
    mkStrBind(linenum, str, sigexp, strexp);
enddefine;

define lconstant parse_structuredec(followers) -> strdec;
    lvars followers, strdec;
    begin_structuredec();
    lvars strdec = mkStructureDec(ml_linenum, false,
        parse_seq(parse_strbind, RESERVED 'and', followers));
    end_structuredec() -> first(strdec);
enddefine;

;;; try_parse_dec:
;;;     if the next item is a declaration opener, parse the appropriate
;;;     declaration; otherwise, return <false>. A structure declaration
;;;     is allowed when as_strdec is true.

lvars abstraction_warning = true;
define lconstant try_parse_dec(as_strdec, followers);
    lvars as_strdec, followers;
    lvars item = getitem();
    if item == RESERVED 'val' then
        parse_valdec(followers);
    elseif item == RESERVED 'fun' then
        parse_fundec(followers);
    elseif item == RESERVED 'type' then
        parse_typedec(followers);
    elseif item == RESERVED 'datatype' then
        parse_datatypedec(followers);
    elseif item == RESERVED 'abstype' then
        parse_abstypedec(followers);
    elseif item == RESERVED 'exception' then
        parse_exceptiondec(followers);
    elseif item == RESERVED 'open' then
        parse_opendec(followers);
    elseif item == RESERVED 'infix' then
        parse_fixity_directive("infix", followers);
    elseif item == RESERVED 'infixr' then
        parse_fixity_directive("infixr", followers);
    elseif item == RESERVED 'nonfix' then
        parse_fixity_directive("nonfix", followers);
    elseif item == RESERVED 'structure' and as_strdec then
        parse_structuredec(followers);
    elseif item == RESERVED 'abstraction' and as_strdec then
        ;;; abstraction is just a synonym for structure
        if abstraction_warning then
            lvars name = peekitem();
            ml_warning(
                'the abstraction declaration is not supported\n',
                if isshortid(name) then <> '\tabstraction %p\n' endif,
                <> 'This and subsequent occurrences will be compiled as structure declarations\n',
                [^name], popfilename, ml_linenum);
            ;;; warn once only
            false -> abstraction_warning;
        endif;
        parse_structuredec(followers);
    elseif item == RESERVED 'local' then
        if as_strdec then mkLocalStrDec else mkLocalDec endif
        (   ml_linenum,
            begin_scope(),
            parse_decs(as_strdec, RESERVED 'in', DEC_OPENERS),
            begin_scope(),
            parse_decs(as_strdec, RESERVED 'end', followers),
            chop_scope());
    else
        item -> getitem();
        false;
    endif;
enddefine;

;;; parse_decs:
;;;     parse a sequence of zero or more declarations with optional ";"
;;;     separators, stopping at terminator.

define parse_decs(as_strdec, terminator, followers);
    lvars as_strdec, terminator, followers, item;
    lvars after_dec =
        ;;; NB: garbaged below
        conspair(RESERVED ';', conspair(terminator, DEC_OPENERS));
    [%  until (getitem() ->> item) == terminator do
            item -> getitem();
            lvars dec = try_parse_dec(as_strdec, after_dec);
            unless dec then
                ;;; item cannot start a declaration
                if Lmember(item, followers) then
                    needitem(terminator, [], followers);
                else
                    getitem() -> item;
                    parse_fail(item,
                        if item == RESERVED 'structure'
                        or item == RESERVED 'abstraction'
                        then
                            'A structure declaration is only allowed at top-level,\n'
                            <> 'or within a structure expression\n', []
                        elseif item == RESERVED 'signature'
                            or item == RESERVED 'functor'
                        then
                            'A %p declaration is only allowed at top-level\n',
                            [^item]
                        elseif item == RESERVED 'eqtype'
                            or item == RESERVED 'include'
                        then
                            'An %p specification is not allowed as a declaration\n',
                            [^item]
                        else
                            'Expecting the start of a declaration';
                            if item == RESERVED ';'
                            and Lmember(peekitem(), DEC_OPENERS)
                            then
                                <> ';\na ";" may have been inserted by mistake\n'
                            elseif item == RESERVED 'fn' then
                                <> ';\n"fn" may have been used instead of "fun"\n'
                            elseif item == RESERVED 'let' then
                                <> ';\n"let" may have been used instead of "local"\n'
                            else
                                <> '\n'
                            endif, [];
                        endif);
                endif;
            endunless;
            dec;
            testitem(RESERVED ';') -> ;
        enduntil;
    %];
    ;;; return token set to the freelist
    sys_grbg_destpair(sys_grbg_destpair(after_dec)) -> (,,);
enddefine;


;;; == Specifications =====================================================

constant procedure parse_specs;

lconstant SPEC_OPENERS = [%
    RESERVED 'val',
    RESERVED 'type',
    RESERVED 'eqtype',
    RESERVED 'datatype',
    RESERVED 'exception',
    RESERVED 'structure',
    RESERVED 'local',
    RESERVED 'open',
    RESERVED 'include',
    RESERVED 'sharing',
%];

define lconstant parse_valdesc(followers);
    lvars followers, item = getitem();
    unless isshortid(item) then
        if item == RESERVED 'op' and isshortid(peekitem()) then
            getitem() -> item;
            parse_error(true, 'the "op" prefix is not allowed here\n\top %p\n',
                [^item], popfilename, ml_linenum);
        elseunless item == RESERVED '=' then
            parse_fail(item, 'a variable');
        endif;
        "=" -> item;
    endunless;
    mkValDesc(ml_linenum, declare_var(item, ml_linenum),
        needitem(RESERVED ':', [], is_tyexp_opener),
        parse_tyexp(followers));
enddefine;

define lconstant parse_valspec(followers);
    lvars followers, linenum = ml_linenum;
    begin_valdec();
    lvars valdescs = parse_seq(parse_valdesc, RESERVED 'and', followers);
    lvars (tyvars, vs) = end_valdec();
    mkValSpec(linenum, vs, tyvars, valdescs);
enddefine;

define lconstant parse_typedesc(followers);
    lvars followers, linenum = ml_linenum;
    begin_typebind();
    lvars (tycon, tyvars) = parse_tycon_bind();
    end_typebind();
    mkTypeDesc(linenum, tycon, tyvars);
enddefine;

define lconstant parse_typespec(followers);
    lvars followers, linenum = ml_linenum;
    begin_typedec();
    lvars typedescs = parse_seq(parse_typedesc, RESERVED 'and', followers);
    lvars tycons = end_typedec();
    mkTypeSpec(linenum, tycons, typedescs);
enddefine;

define lconstant parse_eqtypespec(followers);
    lvars followers, linenum = ml_linenum;
    begin_typedec();
    lvars typedescs = parse_seq(parse_typedesc, RESERVED 'and', followers);
    lvars tycons = end_typedec();
    mkEqtypeSpec(linenum, tycons, typedescs);
enddefine;

define lconstant parse_datadesc(followers);
    lvars followers;
    begin_typebind();
    lvars (tycon, tyvars) = parse_tycon_bind();
    lvars linenum = ml_linenum;
    needitem(RESERVED '=', [], isshortid);
    lvars constrs = parse_conbinds(parse_shortid, followers);
    ;;; include a list of constructors in the type constructor
    map(constrs, first) ->> tycon_orig_cons(tycon) -> tycon_cons(tycon);
    end_typebind();
    mkDataDesc(linenum, tycon, tyvars, constrs);
enddefine;

define lconstant parse_datatypespec(followers);
    lvars followers, linenum = ml_linenum;
    begin_datatypedec();
    lvars datadescs = parse_seq(parse_datadesc, RESERVED 'and', followers);
    lvars (tycons, cons) = end_datatypedec();
    mkDatatypeSpec(linenum, tycons, cons, datadescs);
enddefine;

define lconstant parse_exndesc(followers);
    lvars followers;
    mkExnDesc(ml_linenum,
        declare_exn(parse_shortid('an exception constructor'), ml_linenum),
        testitem(RESERVED 'of') and parse_tyexp(followers));
enddefine;

define lconstant parse_exceptionspec(followers);
    lvars followers, linenum = ml_linenum;
    begin_exceptiondec();
    lvars exndescs = parse_seq(parse_exndesc, RESERVED 'and', followers);
    lvars exns = end_exceptiondec();
    mkExceptionSpec(linenum, exns, exndescs);
enddefine;

define lconstant parse_strdesc(followers);
    lvars followers;
    lvars str = declare_str(parse_shortid('a structure name'), ml_linenum);
    lvars linenum = ml_linenum;
    needitem(RESERVED ':', [], RESERVED 'sig');
    begin_modbind();
    lvars sigexp = (parse_sigexp(followers) -> str_env(str));
    end_modbind();
    mkStrDesc(linenum, str, sigexp);
enddefine;

define lconstant parse_structurespec(followers);
    lvars followers, strs, strdescs, linenum = ml_linenum;
    begin_structuredec();
    lvars strdescs = parse_seq(parse_strdesc, RESERVED 'and', followers);
    lvars strs = end_structuredec();
    mkStructureSpec(linenum, strs, strdescs);
enddefine;

define lconstant parse_shareq(followers);
    lvars followers, linenum = ml_linenum;
    if testitem(RESERVED 'type') then
        define lvars get_name();
            get_tycon(parse_tycon_id(parse_id), ml_linenum);
        enddefine;
    else
        testitem(RESERVED 'structure') -> ; ;;; allow this
        define lvars get_name();
            get_str(parse_id('a structure name'), ml_linenum);
        enddefine;
    endif;
    lvars names = [%
        repeat
            get_name();
            quitunless(testitem(RESERVED '='));
        endrepeat;
    %];
    if Back(names) == [] then
        parse_error(true, 'a sharing equation must involve at least two names\n',
            [], popfilename, ml_linenum);
    endif;
    mkShareq(linenum, names);
enddefine;

define lconstant parse_sharingspec(followers);
    lvars followers;
    mkSharingSpec(ml_linenum,
        parse_seq(parse_shareq, RESERVED 'and', followers));
enddefine;

define lconstant parse_openspec(followers);
    lvars followers, item;
    mkOpenSpec(ml_linenum, [%
        while isid(getitem() ->> item) do
            lvars str = get_str(item, ml_linenum);
            open(str_env(str));
            str;
        endwhile
    %]);
    item -> getitem();
enddefine;

define lconstant parse_includespec(followers);
    lvars followers, item;
    mkIncludeSpec(ml_linenum, [%
        while isshortid(getitem() ->> item) do
            lvars sig = signature_instance(get_sig(item, ml_linenum));
            open(sig_env(sig));
            sig;
        endwhile
    %]);
    item -> getitem();
enddefine;

define lconstant try_parse_spec(followers);
    lvars followers, item = getitem();
    if item == RESERVED 'val' then
        parse_valspec(followers);
    elseif item == RESERVED 'type' then
        parse_typespec(followers);
    elseif item == RESERVED 'eqtype' then
        parse_eqtypespec(followers);
    elseif item == RESERVED 'datatype' then
        parse_datatypespec(followers);
    elseif item == RESERVED 'exception' then
        parse_exceptionspec(followers);
    elseif item == RESERVED 'structure' then
        parse_structurespec(followers);
    elseif item == RESERVED 'sharing' then
        parse_sharingspec(followers);
    elseif item == RESERVED 'open' then
        parse_openspec(followers);
    elseif item == RESERVED 'include' then
        parse_includespec(followers);
    elseif item == RESERVED 'local' then
        mkLocalSpec(
            ml_linenum,
            begin_scope(),
            parse_specs(RESERVED 'in', SPEC_OPENERS),
            begin_scope(),
            parse_specs(RESERVED 'end', followers),
            chop_scope());
    else
        item -> getitem();
        false;
    endif;
enddefine;

define parse_specs(terminator, followers);
    lvars terminator, followers, item;
    lvars after_spec =
        ;;; NB: garbaged below
        conspair(RESERVED ';', conspair(terminator, SPEC_OPENERS));
    [%  until (getitem() ->> item) == terminator do
            item -> getitem();
            lvars spec = try_parse_spec(after_spec);
            unless spec then
                ;;; item cannot start a specification
                if Lmember(item, followers) then
                    needitem(terminator, [], followers);
                else
                    getitem() -> item;
                    parse_fail(item,
                        if item == RESERVED 'fun'
                        or item == RESERVED 'abstype'
                        or item == RESERVED 'signature'
                        or item == RESERVED 'functor'
                        then
                            '%S %p declaration is not allowed as a specification\n',
                            [% item == RESERVED 'abstype' and 'An' or 'A', item %]
                        elseif item == RESERVED ';'
                        and Lmember(peekitem(), SPEC_OPENERS)
                        then
                            'Expecting the start of a specification;\n'
                            <> 'a ";" may have been inserted by mistake\n', []
                        else
                            'the start of a specification'
                        endif);
                endif;
            endunless;
            spec;
            testitem(RESERVED ';') -> ;
        enduntil;
    %];
    ;;; return token set to the freelist
    sys_grbg_destpair(sys_grbg_destpair(after_spec)) -> (,,);
enddefine;


;;; == Signatures and Functors ============================================

define parse_sigexp(followers) -> (sigexp, sig);
    lvars item = getitem(), followers, sigexp, sig;
    if isshortid(item) then
        mkIdSigexp(ml_linenum, item) -> sigexp;
        set_sig(sigexp);
        ;;; each occurrence of a named signature generates a unique instance
        signature_instance(first(sigexp)) -> first(sigexp);
        sig_env(first(sigexp)) -> sig;
    elseif item == RESERVED 'sig' then
        begin_struct();
        mkGenSigexp(ml_linenum, false, parse_specs(RESERVED 'end', followers))
            -> sigexp;
        end_struct() ->> sig -> first(sigexp);
    else
        parse_fail(item, 'a signature expression');
    endif;
enddefine;

define lconstant parse_sigbind(followers);
    lvars followers, linenum = ml_linenum;
    lvars sig = declare_sig(parse_shortid('a signature name'), ml_linenum);
    needitem(RESERVED '=', [], RESERVED 'sig');
    begin_modbind();
    lvars sigexp = (parse_sigexp(followers) -> sig_env(sig));
    end_modbind();
    mkSigBind(linenum, sig, sigexp);
enddefine;

define parse_signaturedec(followers) -> dec;
    lvars followers, dec;
    begin_signaturedec();
    lvars dec = mkSignatureDec(ml_linenum, false,
        parse_seq(parse_sigbind, RESERVED 'and', followers));
    end_signaturedec() -> first(dec);
enddefine;

define lconstant parse_functor_formal(followers) -> (sigexp, str);
    lvars followers, sigexp, str = false;
    needitem(RESERVED '(', [], SPEC_OPENERS);
    lvars item = peekitem();
    lvars linenum = ml_linenum;
    if isshortid(item) then
        lconstant after_sigexp = [% RESERVED ')' %];
        begin_structuredec();
        declare_str(getitem(), ml_linenum) -> str;
        needitem(RESERVED ':', [], RESERVED 'sig');
        parse_sigexp(after_sigexp) -> str_env(str) -> sigexp;
        end_structuredec() -> ;
        needitem(RESERVED ')', [], followers);
    else
        begin_struct();
        mkGenSigexp(linenum, false, parse_specs(RESERVED ')', followers))
            -> sigexp;
        end_struct() -> first(sigexp);
        ;;; make bindings visible in result signature and functor body
        open(first(sigexp));
    endif;
enddefine;

define lconstant parse_fncbind(followers);
    lvars followers;
    lconstant after_formal = [% RESERVED ':', RESERVED '=' %];
    lvars fnc = declare_fnc(parse_shortid('a functor name'), ml_linenum);
    lvars linenum = ml_linenum;
    begin_modbind();
    lvars (sigexp, str) = parse_functor_formal(after_formal);
    lvars sigexp1 = false, sig1;
    if testitem(RESERVED ':') then
        parse_sigexp(Back(after_formal)) -> (sigexp1, sig1);
    endif;
    needitem(RESERVED '=', [], is_strexp_opener);
    lvars (strexp, struct) = parse_strexp(followers);
    end_modbind();
    if not(sigexp1)
    or structenv_signame(struct)
    and structenv_signame(struct) == structenv_signame(sig1)
    then
        ;;; no signature, or the same signature on both sides:
        ;;; use the structure environment unchanged
        struct -> fnc_env(fnc);
    else
        ;;; signature match needed
        sig1 -> fnc_env(fnc);
    endif;
    mkFncBind(linenum, fnc, str, sigexp, sigexp1, strexp);
enddefine;

define parse_functordec(followers) -> dec;
    lvars followers, dec;
    begin_functordec();
    lvars dec = mkFunctorDec(ml_linenum, false,
        parse_seq(parse_fncbind, RESERVED 'and', followers));
    end_functordec() -> first(dec);
enddefine;


;;; == Poplog Specials (pervasive and external) ===========================

constant procedure (        ;;; forward in "external.p"
    external_type,
    external_val,
    external_exception,
    external_structure,
    comp_external_structure,
    new_pop_itemiser,
);

define lconstant parse_pervasivedec(followers);
    lvars followers, item;
    mkPervasiveDec(ml_linenum, [%
        while isid(getitem() ->> item) do
            lvars str = get_str(item, ml_linenum);
            pervasive(str_env(str));
            str;
        endwhile
    %]);
    item -> getitem();
enddefine;

define parse_external_type() -> (tycon, tyvars);
    lvars tycon, tyvars;
    dlocal getitem = new_pop_itemiser();
    begin_typedec();
    begin_typebind();
    parse_tycon_bind() -> (tycon, tyvars);
    end_typebind();
    end_typedec() -> ;
enddefine;

define parse_external_val() -> (var, tyvars, tyexp);
    lvars id, var, tyexp, tyvars;
    dlocal getitem = new_pop_itemiser();
    begin_valdec();
    unless isshortid(getitem() ->> id) then
        unless id == RESERVED '=' then
            parse_fail(id, 'a variable');
        endunless;
        "=" -> id;
    endunless;
    if lookup_con(id) ->> var then
        parse_error(false, '%S rebound as external variable\n\t%p\n', [%
            if val_isexn(var) then
                'exception constructor'
            else
                'constructor'
            endif, id
        %], popfilename, ml_linenum);
    endif;
    declare_var(id, ml_linenum) -> var;
    needitem(RESERVED ':', [], []);
    parse_tyexp([]) -> tyexp;
    end_valdec() -> -> tyvars;
enddefine;

define parse_external_exception() -> (exn, tyexp);
    lvars exn, tyexp;
    dlocal getitem = new_pop_itemiser();
    begin_exceptiondec();
    declare_exn(parse_shortid('an exception constructor'), ml_linenum) -> exn;
    testitem(RESERVED 'of') and parse_tyexp([]) -> tyexp;
    end_exceptiondec() -> ;
enddefine;

define parse_external_structure() -> (str, sig, struct);
    lvars id, str, sig = false, struct, item;
    dlocal getitem = new_pop_itemiser();
    begin_structuredec();
    declare_str(parse_shortid('a structure name'), ml_linenum) -> str;
    if testitem(RESERVED ':') then
        parse_shortid('a signature name') -> id;
        signature_instance(get_sig(id, ml_linenum)) -> sig;
    endif;
    needitem(RESERVED '=', [], []);
    if isid(getitem() ->> item) then
        str_env(get_str(item, ml_linenum)) -> struct;
    else
        item -> getitem();
        begin_modbind();
        begin_struct();
        comp_external_structure();
        end_struct() -> struct;
        end_modbind();
    endif;
    end_structuredec() -> ;
enddefine;

define parse_externaldec(followers);
    lvars followers, item, entries, linenum = ml_linenum;
    dlocal proglist = pdtolist(incharitem(ml_nextchar(% getitem %)));
    if (getitem() ->> item) == RESERVED 'val' then
        conspair(external_val(), []) -> entries;
    elseif item == RESERVED 'type' then
        conspair(external_type(false)) -> entries;
    elseif item == RESERVED 'eqtype' then
        conspair(external_type(true)) -> entries;
    elseif item == RESERVED 'exception' then
        conspair(external_exception(), []) -> entries;
    elseif item == RESERVED 'structure' then
        conspair(external_structure(), []) -> entries;
    else
        parse_fail(item, 'an external binding');
    endif;
    tryreserved(readitem()) -> getitem();   ;;; should be ";"
    nextchar(readitem) -> ml_nextchar(getitem);
    mkExternalDec(linenum, item, entries);
enddefine;


;;; == Top Level Declarations =============================================

lconstant TOPDEC_OPENERS = [%
    RESERVED 'functor',
    RESERVED 'signature',
    RESERVED 'pervasive',
    RESERVED 'external',
%   ^^DEC_OPENERS
];

;;; parse_it:
;;;     parses a top-level expression, interpreted as a binding to "it"

define lconstant parse_it(followers);
    lvars followers, linenum = ml_linenum;
    begin_valdec();
    lvars pat = mkVarPat(linenum, declare_var("it", linenum));
    lvars vb = mkValBind(linenum, pat, parse_exp(followers));
    lvars (tyvars, varbinds) = end_valdec();
    mkStrTopDec(linenum, [% mkValDec(linenum, varbinds, tyvars, [^vb], []) %]);
enddefine;

;;; parse:
;;;     parses a top-level declaration or expression

define parse(getitem) -> (dec, local_env);
    lvars dec, item = getitem(), linenum = ml_linenum;
    dlocal getitem, local_env, popprompt = '= '; ;;; after first item read
    lconstant after_topdec = [% RESERVED ';', termin % ^^TOPDEC_OPENERS];
    lconstant after_topexp = [% RESERVED ';', termin %];
    lvars followers = after_topdec;
    begin_topdec();
    if item == RESERVED 'functor' then
        mkFncTopDec(linenum, [% parse_functordec(followers) %]);
    elseif item == RESERVED 'signature' then
        mkSigTopDec(linenum, [% parse_signaturedec(followers) %]);
    elseif item == RESERVED 'pervasive' then
        mkPerTopDec(linenum, [% parse_pervasivedec(followers) %]);
    elseif item == RESERVED 'external' then
        mkExtTopDec(linenum, [% parse_externaldec(followers) %]);
    elseif (item -> getitem(), try_parse_dec(true, followers) ->> dec) then
        mkStrTopDec(linenum, dec ::
            [%  while try_parse_dec(true, followers) ->> dec do
                    dec;
                endwhile;
            %]);
    elseif is_exp_opener(item) then
        ;;; top-level expression: must be properly terminated
        parse_it(after_topexp ->> followers);
    else
        parse_fail(item, 'the start of a declaration or expression');
    endif -> dec;
    unless testitem(RESERVED ';') then
        unless Lmember(peekitem(), followers) then
            needitem(RESERVED ';', [], []);
        endunless;
    endunless;
    end_topdec();
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Mar 30 1995
        Changed parse_funbind/parse_clause to abort on the first clause
        with a different name or arity, to avoid cascades of errors when
        it's really the first clause that's wrong. A better strategy would
        be to keep going, but only report the first error ...
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Nov 17 1994
        Added abstraction reserved word (treated as a synonym for structure)
        for compatibility with SML/NJ
--- Robert John Duncan, Nov 16 1994
        Change to parsing of top-level declarations to be less pedantic
        about semicolons: it's now possible to mix all kinds of declarations
        at top-level. This also does away with the need for dynamic lists,
        with each topdec processed individually instead.
--- Robert John Duncan, Nov  8 1994
        Changes to error reporting based on passing around lists of
        followers -- i.e. sets of tokens that could legitimately follow the
        current construct -- to give slightly more informative messages,
        plus several more special cases spotted.
        Also, errors can now continue by setting ml_errors_continue to true.
--- Robert John Duncan, Oct 24 1994
        Changes to error reporting: parse_error now defined here
--- Robert John Duncan, Sep 27 1991
        Added assignment to -tycon_orig_cons- field of new tycons
--- Simon Nichols, Jun 26 1991
        Changed -parse- to use -begin_topdec- and -end_topdec-.
--- Robert John Duncan, Mar  1 1991
        New lexical items.
        Abolished -ml*_readitem-: itemiser now passed as argument to the
        parser.
        Allow "=" as a variable in fixity directives, value descriptions
        and external value bindings.
--- Robert John Duncan, Feb 11 1991
        Exceptions, constructors and variables now represented by a common
        "val" record.
        New style error messages
--- Robert John Duncan, Feb  4 1991
        Parser now returns the environment as well as the syntax tree.
        New representation of operators.
        Redundant constructors field removed from some syntax nodes.
--- Robert John Duncan, Nov  8 1990
        Added -parse_tycon_id- to stop "*" being allowed as a type
        constructor
--- Robert John Duncan, Oct  2 1990
        Added local structure declarations.
--- Rob Duncan, May 24 1990
        Changed "{}" to read as UnitExp/Pat
--- Rob Duncan, Apr 19 1990
        Allowed -termin- as a terminator
--- Rob Duncan, Sep 12 1989
        Changed -parse- to return TopDec sequences rather than single
        declarations.
        Functor and signature declarations are returned as dynamic lists,
        to allow type checking to be interleaved with parsing: signature
        and functor declarations must be typechecked before any identifiers
        they bind are referenced.
--- Rob Duncan, Sep  5 1989
        Added missing check for "end" in LetStrexp.
 */
