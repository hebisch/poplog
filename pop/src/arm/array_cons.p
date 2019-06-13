/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   File:        src/arm/array_cons.p
   Purpose:     Construction of array procedures for ARM
   Author:      Waldek Hebisch
*/

#_INCLUDE 'declare.ph'

global constant
    _array_sub,
;

;;; ---------------------------------------------------------------------

section $-Sys;


    /*  Fill in machine specific part of array procedure.  _tabsize
        is size in bytes of the array params starting at PD_ARRAY_TABLE.
    */
define Array$-Cons(_tabsize) -> _arrayp;
    lvars _tabsize, _arrayp, _drop_ptr, _size, _offs;

    ;;; Get procedure record -- 9 words of code, 36 bytes
    @@PD_ARRAY_TABLE{_tabsize _add _36} _sub @@POPBASE -> _size;
    Get_store(_size) -> _arrayp;

    ;;; initialise some of procedure header
    ##(w){_size} ->> _size -> _arrayp!PD_LENGTH;
    _0  ->> _arrayp!PD_REGMASK
        ->> _arrayp!PD_NUM_STK_VARS
        ->> _arrayp!PD_NUM_PSTK_VARS
        ->> _arrayp!PD_NLOCALS
        ->  _arrayp!PD_GC_SCAN_LEN;
    ##SF_LOCALS -> _arrayp!PD_GC_OFFSET_LEN;
    ##SF_LOCALS _sub ##SF_RETURN_ADDR -> _arrayp!PD_FRAME_LEN;

    ;;; Start of code
    @@PD_ARRAY_TABLE{_tabsize} -> _drop_ptr;
    _arrayp@(w){_drop_ptr} -> _arrayp!PD_EXECUTE;
    

    ;;; Compute offset of array to pc in words.
    ;;; We subtract 9 for code and 2 for words before
    ;;; base and add 2 since on ARM pc gives address
    ;;; incremented by 2 words
    _size _sub _9 -> _offs;

    ;;; Create stack frame
    ;;; load our address to r11
    _16:E24FBF00 _biset _offs -> _arrayp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;; Store lr and PB
    ;;;   stmfd sp!, {r11, lr}
    _16:E92D4800 -> _arrayp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;; Call the array subscript routine
    ;;; load address of assembler routine
    ;;;   ldr     r0, [pc, #_disp]
    _16:E59F0010 -> _arrayp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;; call it
    ;;;   blx    r0
    _16:E12FFF30 -> _arrayp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;

    _arrayp@(w){_drop_ptr} -> _arrayp!PD_EXIT;
    ;;; Unwind stack frame and return
    ;;;   add  sp, sp, #4
    _16:E28DD004 -> _arrayp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;;   ldr lr, [sp], #4
    _16:E49DE004 -> _arrayp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;;   ldr  PB, [sp]
    _16:E59DB000 -> _arrayp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;;   bx      lr
    _16:E12FFF1E -> _arrayp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;; drop address of array subscript routine
    _array_sub -> _arrayp!(w){_drop_ptr};
enddefine;

endsection;     /* $-Sys */

