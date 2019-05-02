/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/readutils.p
 > Purpose:         Basic Common Lisp reading and itemising utilities
 > Author:          John Williams, Sep 26 1986 (see revisions)
 > Documentation:   CLtL, p374-382
 > Related Files:   C.all/lisp/src/itemise.p
 */

lisp_compile_mode;

section $-lisp;


/* User variables */

define lconstant Convert_sys_list(list);
    lvars item;
    for item in list do
        if isword(item) then
            word_to_sym(item, keyword_package)
        elseif isinteger(item) then
            item
        elseif isdecimal(item) then
            number_coerce(item, decimal_0)
        endif
    endfor
enddefine;


vars
    features
        =   [% @:POPLOG,
               @:COMMON,
               @:CLOS,
               Convert_sys_list(sys_machine_type),
               Convert_sys_list(sys_processor_type),
               Convert_sys_list(sys_os_type),
            %],
    read_eval
        =   true,
    read_suppress
        =   nil,
    ;


lvars Read_base = 10;

vars active read_base
    = integer_range_variable(% ident Read_base, 2, 36, @*READ-BASE*%);


global vars default_float_char = `S`;

lvars Float_format = @SINGLE-FLOAT;


define active read_default_float_format;
    Float_format
enddefine;


define updaterof read_default_float_format(item);
    lconstant Formats
                = [^@SINGLE-FLOAT ^@DOUBLE-FLOAT ^@SHORT-FLOAT ^@LONG-FLOAT];
    if fast_lmember(item, Formats) then
        item -> Float_format;
        fast_subscrs(1, symbol_string(item)) -> default_float_char
    else
        simple_type_error(
            '~S is not a legal value for *read-default-float-format*',
            item,
            #_< [^@OR ^^Formats] >_#)

    endif
enddefine;


propsheet_idents
    popprompt, read_base, read_default_float_format, read_eval;


/* Internal variables */

constant procedure (derref, Set_lexcase, skipform, terminpdr, white);

vars
    ;;; Not localised
    lexchar,
    procedure lexcase = identfn,
    procedure lexdot = identfn,
    procedure lextype,

    ;;; Localised in lispreadform
    lex_eof             =   nil,
    lex_eof_error       =   nil,

    ;;; Localised in top_level_read
    lexbqdepth
    lexref
    lexrefs
    procedure lexket,

    ;;; Localised in lispreaditem
    lex_redo
    lexcontinue
    lexflags
    lexfloatchar
    lexfrac
    lexnum
    lexpkg
    lexpt
    lexstacklength
    lextop
    ;


/* End of file handling */

define eof();
    if pop_true(lex_eof_error) then
        lisp_error(@END-OF-FILE, {^@:STREAM ^standard_input})
    else
        lisp_true(lex_eof)
    endif
enddefine;


/* Case inversion */

define invertcase() with_nargs 1;
    ;;; this is a truly awful hack:
    ;;; we use colour 1 to mark characters as potentially invertable
    ;;; (the colour is needed to distinguish them from characters that
    ;;; were escaped and hence mustn't be case converted).

    fi_|| `\[1]`
enddefine;


define lconstant Invertable(s);
    lvars c, upper = false, lower = false;
    fast_for c in_dstring s do
        if c &&/=_0 `\[1]` then
            if isuppercode(c) then
                returnif (lower) (false);
                uppertolower -> upper
            elseif islowercode(c) then
                returnif (upper) (false);
                lowertoupper -> lower
            endif
        endif
    endfast_for;
    upper or lower
enddefine;


define invertstring(s) -> s;
    lvars p, c;
    if (Invertable(s) ->> p) then
        consstring
            (#| fast_for c in_dstring s do
                    if c &&/=_0 `\[1]` then p(c) else c endif
                endfast_for
            |#) -> s
    endif
enddefine;


/* Set up active variable readtable (i.e. CL *READTABLE*) */

lvars Readtable, Lextable;


define lconstant Set_lexcase(case);
    if case == @:UPCASE then
        lowertoupper
    elseif case == @:PRESERVE then
        identfn
    elseif case == @:DOWNCASE then
        uppertolower
    elseif case == @:INVERT then
        invertcase
    else
        warn('Ignoring illegal READTABLE-CASE value: ~S', [^case]) ->;
        return
    endif -> lexcase
enddefine;


define readtable_case() with_nargs 1;
    readtable_case_sym()
enddefine;


define updaterof readtable_case(case, r);
    case -> readtable_case_sym(r);
    if r == Readtable then
        Set_lexcase(case)
    endif
enddefine;


define active readtable;
    Readtable
enddefine;


define updaterof active readtable(item);
    unless item == Readtable do
        readtable_vector(item) -> Lextable;
        Set_lexcase(readtable_case_sym(item));
        item -> Readtable
    endunless
enddefine;


define lexget();
    if isinteger(cucharin() ->> lexchar) then
        fast_subscrv0(lexchar, Lextable)
    else
        terminpdr
    endif -> lextype
enddefine;


define lexput();
    lexchar -> cucharin()
enddefine;


define is_whitespace(char);
    isinteger(char) and fast_subscrv0(char, Lextable) == white
enddefine;


/* Basic reading procedures */

constant macro
    _LEX_GOT_DIGIT   =   1 << 0,
    _LEX_NEG         =   1 << 1,
    _LEX_/_OK        =   1 << 2,
    _LEX_SIGN_OK     =   1 << 3,
    _LEX_DOT_OK      =   1 << 4,
    _LEX_EXPT_OK     =   1 << 5,
    _LEX_NEG_EXPT    =   1 << 6,
    ;

lconstant
    Lexflags = _LEX_/_OK _biset _LEX_DOT_OK _biset _LEX_EXPT_OK,
    ;


define lispreaditem();
    dlocal
        lexcontinue         =     1,
        lexstacklength      =     stacklength(),
        lex_redo            =     (Read_base /== 10),
        lexflags            =     Lexflags,
        lexfrac             =     false,
        lexfloatchar        =     `E`,
        lexnum              =     0,
        lexpkg              =     false,
        lexpt               =     false,
        lextop              =     false,
        ;
    while (lexget(), lextype == white) do endwhile;
    if read_suppress /== nil then
        skipform()
    else
        lextype();
        if lexcontinue then
            true -> lexcontinue;
            repeat
                lexget();
                lextype();
                quitunless(lexcontinue)
            endrepeat
        endif
    endif
enddefine;


define top_level_read(procedure read_p) -> form;
    dlocal
        lexbqdepth  =  0,
        lexket      =  #_< mishap(% 0, 'Unexpected closing bracket' %) >_#,
        lexref      =  not,
        lexrefs     =  false,
        ;
    SET_CUCHARIN;
    read_p() -> form;
    if lexrefs then
        derref(form)
    endif
enddefine;


define nested_read(procedure read_p);
    dlocal lex_eof_error = true;
    SET_CUCHARIN;
    read_p()
enddefine;


define lispreadform() with_props 'read';
    dlocal
        lex_eof         =  termin,
        lex_eof_error   =  false,
        ;
    top_level_read(lispreaditem)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 30 1995
        Added is_whitespace (used in lisp/src/read.p).
--- John Williams, Aug 25 1995
        read_default_float_format is now active (and sets default_float_char).
--- John Williams, Aug 11 1995
        Removed redundant lvar declarations.
--- John Williams, Apr  3 1995
        Changes for readtable_case.
--- John Williams, Mar 30 1995
        Changes for CLtL 2 streams.
--- John Williams, Apr 27 1994
        Added :CLOS to features.
--- John Williams, Aug 11 1993
        Enhanced features. Added read_eval.
 */
