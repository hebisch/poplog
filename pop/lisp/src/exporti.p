/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/src/exporti.p
 > Purpose:         Inline code planting versions of some CL functions
 > Author:          John Williams, Mar 25 1988 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/inlines.p
 */

lisp_compile_mode;

section $-lisp;

define lconstant Exporti(sym, pdr, flag);
    lvars ftok;
    if flag then
        proclaim([^@INLINE ^sym])
    endif;
    sf_token(sym) -> ftok;
    pdr -> f_inline(valof(ftok));
    if updater(pdr) then
        if (isfunction_token(ft_setf_method(ftok)) ->> sym) then
            Exporti(sym, updater(pdr), flag)
        else
            /* The updater of pdr (inline code planter for sym)
                is an accidental effect of constructing closures
                in inlines.p, and is not going to be used, so junk it.
            */
            if isinheap(pdr) then
                false -> updater(pdr)
            endif
        endif
    endif
enddefine;


/* Functions that are compiled inline by default */

Exporti(@*,                     inline_*,                       true);
Exporti(@+,                     inline_+,                       true);
Exporti(@-,                     inline_-,                       true);
Exporti(@/,                     inline_/,                       true);
Exporti(@/=,                    inline_/=,                      true);
Exporti(@<,                     inline_<,                       true);
Exporti(@<=,                    inline_<=,                      true);
Exporti(@=,                     inline_=,                       true);
Exporti(@>,                     inline_>,                       true);
Exporti(@>=,                    inline_>=,                      true);
Exporti(@APPEND,                inline_append,                  true);
Exporti(@APPLY,                 inline_apply,                   true);
Exporti(@BOOLE,                 inline_boole,                   true);
Exporti(@CHAR<,                 inline_char_<,                  true);
Exporti(@CHAR<=,                inline_char_<=,                 true);
Exporti(@CHAR=,                 inline_char_=,                  true);
Exporti(@CHAR>,                 inline_char_>,                  true);
Exporti(@CHAR>=,                inline_char_>=,                 true);
Exporti(@COMPLEMENT,            inline_complement,              true);
Exporti(@CONCATENATE,           inline_concatenate,             true);
Exporti(@CONSTANTLY,            inline_constantly,              true);
Exporti(@EQL,                   inline_equal,                   true);
Exporti(@EQUAL,                 inline_equal,                   true);
Exporti(@FUNCALL,               inline_funcall,                 true);
Exporti(@LIST*,                 inline_list_*,                  true);
Exporti(@LIST,                  inline_list,                    true);
Exporti(@LOGAND,                inline_logand,                  true);
Exporti(@LOGEQV,                inline_logeqv,                  true);
Exporti(@LOGIOR,                inline_logior,                  true);
Exporti(@LOGXOR,                inline_logxor,                  true);
Exporti(@NCONC,                 inline_nconc,                   true);
Exporti(@POP,                   inline_pop,                     true);
Exporti(@SET,                   inline_set,                     true);
Exporti(@TYPEP,                 inline_typep,                   true);
Exporti(@VALUES,                inline_values,                  true);
Exporti(@VECTOR,                inline_vector,                  true);

Exporti(@AREF,                  inline_aref,                    false);
Exporti(@CAAR,                  inline_caar,                    false);
Exporti(@CADR,                  inline_cadr,                    false);
Exporti(@CAR,                   inline_car,                     false);
Exporti(@CDAR,                  inline_cdar,                    false);
Exporti(@CDDR,                  inline_cddr,                    false);
Exporti(@CDR,                   inline_cdr,                     false);
Exporti(@CHAR-CODE,             inline_char_code,               false);
Exporti(@CHAR-INT,              inline_char_int,                false);
Exporti(@CODE-CHAR,             inline_code_char,               false);
Exporti(@EIGHTH,                inline_nth(% 8, pair_key %),    false);
Exporti(@FIFTH,                 inline_nth(% 5, pair_key %),    false);
Exporti(@FIRST,                 inline_car,                     false);
Exporti(@FOURTH,                inline_nth(% 4, pair_key %),    false);
Exporti(@GET,                   inline_get,                     false);
Exporti(@NINTH,                 inline_nth(% 9, pair_key %),    false);
Exporti(@REST,                  inline_cdr,                     false);
Exporti(@RPLACA,                inline_rplaca,                  false);
Exporti(@RPLACD,                inline_rplacd,                  false);
Exporti(@SCHAR,                 inline_schar,                   false);
Exporti(@SECOND,                inline_cadr,                    false);
Exporti(@SEVENTH,               inline_nth(% 7, pair_key %),    false);
Exporti(@SIXTH,                 inline_nth(% 6, pair_key %),    false);
;;; Exporti(@SVREF,                 inline_svref,                   false);
Exporti(@TENTH,                 inline_nth(% 10, pair_key %),   false);
Exporti(@THIRD,                 inline_nth(% 3, pair_key %),    false);


/* This is here because it needs to be executed after char_code is
    exported to Lisp in exports
*/

false -> setf_method(@CHAR-CODE);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 17 1996
        Added inline_set.
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun 13 1995
        Added inline_concatenate and removed inline_setf. Also re-arranged.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 26 1994
        Changes for function names.
--- John Williams, Aug 27 1993
        Added inline_char_< and friends. Also inline_rassoc and
        inline_complement.
 */
