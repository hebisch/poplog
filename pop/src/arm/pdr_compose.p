/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   File:        src/arm/pdr_compose.p
   Purpose:     Composition of two procedures, ARM
   Author:      Waldek Hebisch
*/

#_INCLUDE 'declare.ph'

section $-Sys;

;;; Fill in machine specific part of composition of two procedures.

define Cons_pcomposite() -> _comp;
    lvars _drop_ptr, _comp, _size, _offs;

    ;;; Determine size of procedure, code is 12 words
    @@PD_COMPOSITE_TABLE[_12] _sub @@POPBASE -> _size;

    ;;; Allocate new procedure record
    Get_store(_size) -> _comp;

    ;;; Initialise some of the header
    ##(w){_size} ->> _size -> _comp!PD_LENGTH;
    _0 ->> _comp!PD_REGMASK -> _comp!PD_NLOCALS;
    _0 ->> _comp!PD_NUM_STK_VARS -> _comp!PD_NUM_PSTK_VARS;
    ##SF_LOCALS -> _comp!PD_GC_OFFSET_LEN;
    _0 -> _comp!PD_GC_SCAN_LEN;
    ##SF_LOCALS _sub ##SF_RETURN_ADDR -> _comp!PD_FRAME_LEN;

    ;;; Plant the executable code:
    @@PD_COMPOSITE_TABLE -> _drop_ptr;
    _comp@(w){_drop_ptr} -> _comp!PD_EXECUTE;

    ;;; Compute offset of composite to pc in words.
    ;;; We subtract 12 for code and 2 for words before
    ;;; base and add 2 since on ARM pc gives address
    ;;; incremented by 2 words
    _size _sub _12 -> _offs;

    ;;; Create stack frame
    ;;; load our address to r11
    _16:E24FBF00 _biset _offs -> _comp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;; Store lr and PB
    ;;;   stmfd sp!, {r11, lr}
    _16:E92D4800 -> _comp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;

    ;;; Call PD_COMPOSITE_P1 and PD_COMPOSITE_P2
    ;;;   ldr     r0, [PB, #PD_COMPOSITE_P1]
    _16:E59B0000 _biset @@PD_COMPOSITE_P1 -> _comp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;;   ldr     r1, [r0, #PD_EXECUTE]
    _16:E5901000 _biset @@PD_EXECUTE -> _comp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;;   blx     r1
    _16:E12FFF31 -> _comp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;;   ldr     r0, [PB, #PD_COMPOSITE_P2]
    _16:E59B0000 _biset @@PD_COMPOSITE_P2 -> _comp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;;   ldr     r1, [r0, #PD_EXECUTE]
    _16:E5901000 _biset @@PD_EXECUTE -> _comp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;;   blx     r1
    _16:E12FFF31 -> _comp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;

    _comp@(w){_drop_ptr} -> _comp!PD_EXIT;

    ;;; Unwind stack frame and return
    ;;;   add  sp, sp, #4
    _16:E28DD004 -> _comp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;;   ldr lr, [sp], #4
    _16:E49DE004 -> _comp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;;   ldr  PB, [sp]
    _16:E59DB000 -> _comp!(w){_drop_ptr};
    _drop_ptr _add _4 -> _drop_ptr;
    ;;;   bx      lr
    _16:E12FFF1E -> _comp!(w){_drop_ptr};
enddefine;

endsection;     /* $-Sys */

