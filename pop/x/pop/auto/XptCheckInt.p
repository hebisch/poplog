/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCheckInt.p
 > Purpose:         Check for Integer ranges
 > Author:          John Gibson, Apr 13 1993
 */
compile_mode :pop11 +strict;

section;

lconstant
;;; Maximum and minimum values for ints, uints and ulongs - 13/09/90
    int_bitsize = SIZEOFTYPE(:int, :1),
    uint_bitsize = SIZEOFTYPE(:uint, :1),
    ulong_bitsize = SIZEOFTYPE(:ulong, :1),

    int_min = -2**(int_bitsize-1),
    int_max = 2**(int_bitsize-1)-1,
    ulong_max = (2**ulong_bitsize)-1,
    uint_max = (2**int_bitsize)-1,

;;; Max and min values for shorts, ushorts - JM 31/1/91
    short_bitsize = SIZEOFTYPE(:short, :1),
    short_min = -2**(short_bitsize-1),
    short_max = 2**(short_bitsize-1),
    ushort_max = (2**short_bitsize)-1,
;

;;; General integer checking routine - 13/09/90
define lconstant checkintegral(num, lo, hi);
    lvars num, lo, hi;
    unless isintegral(num) and num >= lo and num <= hi do;
        mishap(num, 1, 'INTEGER BETWEEN '
                        sys_>< lo sys_>< ' AND ' sys_>< hi sys_>< ' NEEDED'
              );
    else
        num;
    endunless;
enddefine;

;;; Checks argument as short - JM 31/1/91
define XptCheckShort =
    checkintegral(%short_min, short_max%);
enddefine;

;;; Checks argument as unsigned short - JM 31/1/91
define XptCheckUnsignedShort =
    checkintegral(%0, ushort_max%);
enddefine;

;;; Checks argument is a int - 09/08/90
define XptCheckInt =
    checkintegral(%int_min, int_max%);
enddefine;

;;; Check for uint - 09/08/90
define XptCheckUnsignedInt =
    checkintegral(%0, uint_max%);
enddefine;

;;; Checks argument is an ulong - 09/08/90
define XptCheckUnsignedIntegral =
    checkintegral(%0, ulong_max%);
enddefine;

endsection;
