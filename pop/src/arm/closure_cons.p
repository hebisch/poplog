/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   File:        src/arm/closure_cons.p
   Purpose:     Construction of closures for ARM
   Author:      Waldek Hebisch
*/

#_INCLUDE 'declare.ph'

constant
        procedure (Sys$-Exec_closure)
    ;


section $-Sys;

;;; Fill in machine specific part of a closure record.
;;; _nfroz is number of frozen arguments.

define Cons_closure(_nfroz) -> _clos;
    lvars _drop_ptr, _nfroz, _size, _offs, _clos;

    ;;; Compute size in words of closure code
    if _nfroz _gr _16 then
        ;;; 4 instructions + 2 address words
        _6
    else
        _nfroz _mult _2 _add _3
    endif -> _offs;

    ;;; add header size and frozval size to get total
    @@PD_CLOS_FROZVALS[_nfroz _add _offs] _sub @@POPBASE -> _size;

    Get_store(_size) -> _clos;
    ##(w){_size} ->> _size -> _clos!PD_LENGTH;
    unless _clos!PD_LENGTH == _size then
        _clos -> Get_store();       ;;; junk it
        mishap(0, 'CLOSURE EXCEEDS MAXIMUM ALLOWABLE SIZE');
    endunless;

    ;;; Plant the code
    _nfroz -> _drop_ptr;
    if _nfroz _gr _16 then
        ;;; Call via Exec_closure, need address before code
        Exec_closure!PD_EXECUTE -> _clos!PD_CLOS_FROZVALS[_drop_ptr];
        _drop_ptr _add _1 -> _drop_ptr;
        ;;; need offset: we subtract 3 for tail code and 2 for words
        ;;; before base and add 2 since on ARM pc gives address
        ;;; incremented by 2 words
        _size _sub _3 -> _offs;
        _offs -> _clos!PD_CLOS_FROZVALS[_drop_ptr];
        _drop_ptr _add _1 -> _drop_ptr;
    endif;
    _clos@PD_CLOS_FROZVALS[_drop_ptr] -> _clos!PD_EXECUTE;

    if _nfroz _gr _16 then
        ;;; load closure offset to r0
        ;;; ldr    r0, [pc, #-12]
        _16:E51F000C -> _clos!PD_CLOS_FROZVALS[_drop_ptr];
        _drop_ptr _add _1 -> _drop_ptr;
        ;;; compute closure address in r0
        ;;; sub    r0, pc, r0, lsl #2 
        _16:E04F0100 -> _clos!PD_CLOS_FROZVALS[_drop_ptr];
        _drop_ptr _add _1 -> _drop_ptr;
        ;;; push closure address on user stack
        ;;; str     r0, [USP, #-4]!
        _16:E52A0004 -> _clos!PD_CLOS_FROZVALS[_drop_ptr];
        _drop_ptr _add _1 -> _drop_ptr;
        ;;; jump to Exec_closure
        ;;;   ldr    pc, [pc, #-28]
        _16:E51FF01C -> _clos!PD_CLOS_FROZVALS[_drop_ptr];
    else
        ;;; Push the frozen arguments and chain the pdpart
        ;;; Compute offset of closure to pc in words.
        _size _sub _offs -> _offs;
        ;;; load closure address to r0
        _16:E24F0F00 _biset _offs -> _clos!PD_CLOS_FROZVALS[_drop_ptr];
        _drop_ptr _add _1 -> _drop_ptr;
        _clos@PD_CLOS_FROZVALS[_0] _sub _clos -> _offs;
        until _zero(_nfroz) do
            ;;; load frozval to r1
            ;;;   ldr   r1, [r0, #_offs]
            _16:E5901000 _biset _offs -> _clos!PD_CLOS_FROZVALS[_drop_ptr];
            _drop_ptr _add _1 -> _drop_ptr;
            _offs _add _4 -> _offs;
            ;;; push frozval on user stack
            ;;;   str   r1, [USP, #-4]!
            _16:E52A1004 -> _clos!PD_CLOS_FROZVALS[_drop_ptr];
            _drop_ptr _add _1 -> _drop_ptr;
            _nfroz _sub _1 -> _nfroz;
        enduntil;
        ;;; now get PD_CLOS_PDPART address in ARG_REG_0
        ;;;   ldr  r0, [r0, #16]
        _16:E5900010 ->  _clos!PD_CLOS_FROZVALS[_drop_ptr];
        _drop_ptr _add _1 -> _drop_ptr;
        ;;; and jump to its execute address
        ;;;   ldr  pc, [r0]
        _16:E590F000 -> _clos!PD_CLOS_FROZVALS[_drop_ptr];
    endif;
enddefine;

endsection;     /* $-Sys */
