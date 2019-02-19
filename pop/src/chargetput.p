/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/chargetput.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ---------------- DEVICE CHARACTER INPUT/OUTPUT ---------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'

;;; ----------------------------------------------------------------------

section $-Sys$-Io;

    ;;; set by rawcharout
vars _default_output16_char = false;

lconstant
    charbuf = writeable inits(2);   ;;; 1st used for input, 2nd for output

    ;;; next is temporary until Popc can generate string16s
struct STRING16_1
  { word    V_LENGTH;
    full    KEY;
>-> word    S16_DATA;
  };

lconstant
    icharbuf16 = writeable struct STRING16_1 =>> {% _1, string16_key, _0 %},
    ocharbuf16 = writeable struct STRING16_1 =>> {% _1, string16_key, _0 %},
    ;

define Get_char(dev);
    lvars dev, _count, _c;
    _CHECKINTERRUPT;
    if fast_idval(dev!D_ENCODING_ID) then
        ;;; device expects string16 buffer
        fast_apply(dev, 1, icharbuf16, 1, dev!D_READ) -> _count;
        icharbuf16!V_SHORTS[_0] -> _c
    else
        fast_apply(dev, 1, charbuf, 1, dev!D_READ) -> _count;
        charbuf!V_BYTES[_0] -> _c
    endif;
    if _count == 0 then
        unless systrmdev(dev)
        or (testdef popdevin and dev == weakref popdevin) then
            sysclose(dev)
        endunless;
        termin
    elseif _count fi_< 0 then
        chain(dev, Get_char)
    else
        if _c == _:`\n` and testdef poplinenum then
            weakref poplinenum fi_+ 1 -> weakref poplinenum
        endif;
        _pint(_c)
    endif
enddefine;

define Put_char(char, dev);
    lvars char, dev, _sav1, _sav2, _c, _len;
    lconstant macro INV_MASK = ~~16:FFFFFF;
    dlocal 0
        % (charbuf!V_BYTES[_1] -> _sav1, ocharbuf16!V_SHORTS[_0] -> _sav2),
          (_sav2 -> ocharbuf16!V_SHORTS[_0], _sav1 -> charbuf!V_BYTES[_1])
        %;

    _CHECKINTERRUPT;

    if char == termin then
        unless systrmdev(dev) then sysclose(dev) endunless
    elseif isinteger(char) and not((_int(char) ->> _c) _bitst _:INV_MASK) then
        unless _c _bitst _16:FF00 then
            _c -> charbuf!V_BYTES[_1];
            fast_apply(dev, 2, charbuf, 1, dev!D_WRITE)
        elseif fast_idval(dev!D_ENCODING_ID) then
            ;;; device can handle string16 buffer
            _c -> ocharbuf16!V_SHORTS[_0];
            fast_apply(dev, 1, ocharbuf16, 1, dev!D_WRITE)
        elseif _default_output16_char ->> _c then
            _int(_c) -> charbuf!V_BYTES[_1];
            fast_apply(dev, 2, charbuf, 1, dev!D_WRITE)
        else
            mishap(char, dev, 2, 'DEVICE NOT ENABLED FOR 16-BIT CHARACTER OUTPUT')
        endunless
    elseif iscompound(char)
    and ((char!KEY!K_FLAGS ->> _c) _bitst _:M_K_STRING
        or (_c _bitst _:M_K_VECTOR and char!KEY!K_FIELD_CODE_V == _:t_BYTE))
    then
        char!V_LENGTH -> _len;
        if not(_c _bitst _:M_K_STRING16) or fast_idval(dev!D_ENCODING_ID) then
            fast_apply(dev, 1, char, _pint(_len), dev!D_WRITE)
        else
            ;;; string16 -- output each char separately
            _0 -> _c;
            until _zero(_len) do
                Put_char(_pint(char!V_SHORTS[_c]), dev);
                _c _add _1 -> _c;
                _len _sub _1 -> _len
            enduntil
        endif
    else
        mishap(char, 1, 'CHARACTER (INTEGER 0 TO 16:FFFF) OR STRING NEEDED')
    endif
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 29 1997
        Added _default_output16_char
--- John Gibson, Apr  4 1997
        Replaced D_ENCODING by D_ENCODING_ID
--- John Gibson, Feb 19 1997
        String16 changes
--- John Gibson, Oct 22 1990
        Device read/write procedures now take args as for sysread/write.
--- John Gibson, Sep 11 1990
        Made separate chars in -charbuf- be used for input and output
        (otherwise, the char put in by -Put_char- can get corrupted
        -Get_char-).
--- John Gibson, Mar 24 1990
        Changed test in -Put_char- for byte vector.
--- John Gibson, Jul 25 1989
        Changed _char to char in -Put_char- (in case it's a string
        and there's an interrupt ...)
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Gibson, Mar 16 1988
        Renamed chargetput.p (previously chario.p)
 */
