/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.80386/src/closure_cons.p
 > Purpose:         Constructing closures, Intel 80386
 > Author:          Robert Duncan, Aug 25 1988 (see revisions)
 */


#_INCLUDE 'declare.ph'

constant
        procedure (Sys$-Exec_closure)
    ;


section $-Sys;

;;; Macros for dropping code at -drop_ptr-

lconstant macro (
        QUAD    = [_drop_ptr!(l)++ -> _drop_ptr],
    LONG    = [_drop_ptr!(i)++ -> _drop_ptr],
    SHORT   = [_drop_ptr!(s)++ -> _drop_ptr],
    BYTE    = [_drop_ptr!(b)++ -> _drop_ptr],
);


;;; Cons_closure:
;;;     Construct a raw closure record for -_nfroz- frozen arguments

define Cons_closure(_nfroz) -> _clos;
    lvars _drop_ptr, _nfroz, _size, _offs, _clos;

    ;;; Compute size of closure code
    if _zero(_nfroz) then
        _6
    elseif _nfroz _gr _16 then
        _19
    else
        _nfroz _mult _8 _add _10
    endif -> _size;
    ##(w)[_size | b.r] -> _size;

    ;;; add header size and frozval size to get total
    @@PD_CLOS_FROZVALS[_nfroz _add _size] _sub @@POPBASE -> _size;
    Get_store(_size) -> _clos;
    ##(w){_size} ->> _size -> _clos!PD_LENGTH;
    unless _clos!PD_LENGTH == _size then
        _clos -> Get_store();       ;;; junk it
        mishap(0, 'CLOSURE EXCEEDS MAXIMUM ALLOWABLE SIZE')
    endunless;

    ;;; Plant the code
    _clos@PD_CLOS_FROZVALS[_nfroz] ->> _drop_ptr -> _clos!PD_EXECUTE;

    ;;; On entry, closure address should be in ARG_REG_0 (EAX)
    if _nfroz _gr _16 then
        ;;; Call via -Exec_closure-
        ;;; subq $8, %USP
                _16:48 -> BYTE,
        _16:EB83 -> SHORT, _8 -> BYTE;  ;;; 4 bytes
        ;;; movq %eax, (%USP)
                _16:48 -> BYTE,
        _16:0389 -> SHORT;  ;;; 3 bytes
        ;;; movl $[Exec_closure!PD_EXECUTE], %eax
                _16:48 -> BYTE,
        _16:B8 -> BYTE, 
                Exec_closure!PD_EXECUTE -> QUAD;    ;;; 10 bytes
        ;;; jmp *%eax
        _16:E0FF -> SHORT;                                  ;;; 2 bytes
    else
        ;;; Push the frozen arguments and chain the pdpart
        _0 -> _offs;
        until _zero(_nfroz) do
            ;;; movq [@@PD_CLOS_FROZVALS{_offs}](%rax), %rcx
            ;;; (the offset should be byte-sized)
                        _16:48 -> BYTE,
            _16:488B -> SHORT, @@PD_CLOS_FROZVALS{_offs} -> BYTE;
                                                         ;;; 4 bytes
            @@(w){_offs}++ -> _offs;
            ;;; movq %rcx, [-offs](%USP)
                        _16:48 -> BYTE,
            _16:4B89 -> SHORT, _negate(_offs) -> BYTE;  ;;; 4 bytes
            _nfroz _sub _1 -> _nfroz;
        enduntil;
        if _nonzero(_offs) then
            ;;; adjust the userstack pointer
            ;;; subq $[offs], %USP
                        _16:48 -> BYTE,
            _16:EB83 -> SHORT, _offs -> BYTE;  ;;; 4 bytes
        endif;
        ;;; now get PD_CLOS_PDPART address in ARG_REG_0
        ;;; movl [@@PD_CLOS_PDPART](%eax), %eax
                _16:48 -> BYTE,
        _16:408B -> SHORT, @@PD_CLOS_PDPART -> BYTE;    ;;; 4 bytes
        ;;; and jump straight to its execute address
        ;;; (assuming @@PD_EXECUTE == 0)
        ;;; jmp *(%rax)
        _16:20FF -> SHORT;                                  ;;; 2 bytes
    endif;
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 23 1992
        Added check for size not being too large
 */
