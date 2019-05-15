/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/plog/lib/edinsynt/tokens.p
 > Purpose:         Prolog: token definitions for parsing
 > Author:          Simon Nichols, Mar 14 1990 (see revisions)
 > Related Files:   C.all/plog/lib/edinsynt/readtoken.p
 >                  C.all/plog/lib/edinsynt/readterm.p
 */

section $-prolog;

;;; Special tokens -- represented by constant strings

constant
    COMMA       = ',',
    DOT_SPACE   = '. ',
    LPAR        = '(',
    SPACE_LPAR  = ' (',
    RPAR        = ')',
    LBRA        = '[',
    RBRA        = ']',
    LBRACE      = '{',
    RBRACE      = '}',
    BAR         = '|',
;

lconstant
    special_tokens =
    [%
        COMMA,
        LPAR,
        SPACE_LPAR,
        RPAR,
        LBRA,
        RBRA,
        LBRACE,
        RBRACE,
        BAR
    %],
;


;;; Token recognisers

identof("isword")       -> identof("isatom");
identof("isprologvar")  -> identof("isvar");

define isspecial = fast_lmember(% special_tokens %) enddefine;


;;; Print a token

define print_token(token);
    lvars token, entry;
    if isvar(token) then
        For entry in readenv do
            if fast_prolog_arg(2, entry) == token then
                pr(fast_prolog_arg(1, entry));
                return;
            endif;
        endfor;
        mishap(token, 'NO ENTRY FOR VARIABLE IN ENVIRONMENT');
    elseif isspecial(token) then
        pr(token);
    elseif ispair(token) then
        cucharout(`"`);
        applist(token, cucharout);
        cucharout(`"`);
    else
        prolog_writeq(token);
    endif;
enddefine;

endsection; /* $-prolog */

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Jan 27 1992
        Changed uses of -Arg- to -fast_prolog_arg-.
--- Simon Nichols, Jun 26 1990
        Added DOT_SPACE as a special token.
 */
