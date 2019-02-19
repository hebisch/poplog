/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/number_coerce.p
 > Purpose:
 > Author:          John Gibson, Feb  3 1988
 > Documentation:   REF *NUMBERS
 */

;;; -------------- COERCION BETWEEN NUMBER CLASSES -------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

global constant
        procedure (destcomplex, Sys$-Float_->_Int, Sys$-Checkr_num_class)
    ;

;;; -----------------------------------------------------------------------

section $-Sys => number_coerce;

define number_coerce(x, to_num);
    lvars x, to_num, _type;
    dlocal popdprecision = true;

    define lconstant Float_->_Rat() with_nargs 1;
        lvars i, _arg = _dfop1, _fexp;
        _CLAWBACK_SAVE;
        Float_->_Int(if issimple(dup()) then
                        _pf_dfloat_dec((), _arg), _:ARGTYPE_DECIMAL
                     else
                        _pf_dfloat_ddec((), _arg), _:ARGTYPE_DDECIMAL
                     endif, _arg) -> _fexp -> i;
        Clawback_num(i *  2 ** _fexp)
    enddefine;

    _shift(Checkr_num_class(x) _sub _:NUMTYPE_RATIO, _2) -> _type;
    go_on _pint(Checkr_num_class(to_num) _sub (_:NUMTYPE_RATIO _sub _1) _add _type)
    to
        ;;;   00      01      02      03
        ;;; rat-rat rat-dec rat-ddc rat-cpx
            RETURN  REA_DEC REA_DDC REA_CPX
        ;;;   10      11      12      13
        ;;; dec-rat dec-dec dec-ddc dec-cpx
            DEC_RAT RETURN  REA_DDC REA_CPX
        ;;;   20      21      22      23
        ;;; ddc-rat ddc-dec ddc-ddc ddc-cpx
            DEC_RAT REA_DEC RETURN  REA_CPX
        ;;;   30      31      32      33
        ;;; cpx-rat cpx-dec cpx-ddc cpx-cpx
            CPX_RAT CPX_DEC CPX_DDC CPX_CPX
    else CPX_CPX;   ;;; dummy

    REA_DEC:    _:ARGTYPE_DECIMAL -> _type, goto FLOAT;
    REA_DDC:    _:ARGTYPE_DDECIMAL -> _type;
    FLOAT:
        Dfloat(x, _dfop1) -> ;
        return(Consdecimal(_type, _dfop1));

    DEC_RAT:
        return(Float_->_Rat(x));

    CPX_RAT:
        unless isdecimal(x!CX_REAL) then goto RETURN endunless;
        return(CXWEAK Get_complex(Float_->_Rat(x!CX_REAL),
                                            Float_->_Rat(x!CX_IMAG)));

    CPX_DEC:
        if issimple(x!CX_REAL) and not(isinteger(x!CX_REAL)) then
            goto RETURN
        endif;
        _:ARGTYPE_DECIMAL -> _type, goto FLOAT_CPX;
    CPX_DDC:
        if iscompound(x!CX_REAL) and x!CX_REAL!KEY == ddecimal_key then
            goto RETURN
        endif;
        _:ARGTYPE_DDECIMAL -> _type;
    FLOAT_CPX:
        Dfloat(x!CX_REAL, _dfop1) -> ;
        Dfloat(x!CX_IMAG, _dfop2) -> ;
        return(CXWEAK Get_df_complex(_type, _dfop1, _dfop2));

    REA_CPX:
        return(CXWEAK Get_complex(
                    CXWEAK destcomplex(number_coerce(x, to_num!CX_REAL))));

    CPX_CPX:
        number_coerce(x!CX_REAL, to_num!CX_REAL) -> to_num;
        if x!CX_REAL == to_num then goto RETURN endif;
        return(CXWEAK Get_complex(to_num, number_coerce(x!CX_IMAG, to_num)));

    RETURN:
        x
enddefine;

endsection;     /* $-Sys */
