/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/auto/cons_network_device.p
 > Purpose:         Wrapper device for (eg) converting network ASCII
 > Author:          John Gibson, Dec 23 1997
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

include cons_network_device.ph;

define cons_network_device(iodev, mode, buffer, net_ascii, close_p,
                                                    name, fullname) -> dev;
    lvars dd;

    define lconstant Flush(dev);
        lvars dev;
        chain(subscrv(NTD_IODEV,device_user_data(dev)), sysflush)
    enddefine;

    define lconstant Write(dev, bsub, userbuf, nbytes);
        lvars   n, len, bsub, pos, userbuf, lim, buf, nbytes, dev,
                dd = device_user_data(dev), iodev = subscrv(NTD_IODEV,dd);
        if fast_subscrv(NTD_NET_ASCII,dd) then
            ;;; net ascii -- add c/r before newline
            fast_subscrv(NTD_BUF,dd) -> buf;
            unless buf and datalength(buf) fi_>= nbytes fi_+ nbytes then
                inits(nbytes fi_+ nbytes fi_+ 512) ->> buf
                                                    -> fast_subscrv(NTD_BUF,dd)
            endunless;

            1 -> pos;
            bsub fi_+ nbytes -> lim;
            while bsub fi_< lim do
                unless (locchar(`\n`, bsub, userbuf) ->> n) and n fi_< lim then
                    lim fi_- bsub -> len;
                    move_subvector(bsub, userbuf, pos, buf, len);
                    pos fi_+ len -> pos;
                    quitloop
                endunless;
                n fi_- bsub -> len;
                if len == 1 then
                    fast_subscrs(bsub,userbuf) -> fast_subscrs(pos,buf)
                elseif len /== 0 then
                    move_subvector(bsub, userbuf, pos, buf, len)
                endif;
                pos fi_+ len -> pos;
                `\r` -> fast_subscrs(pos,buf);
                `\n` -> fast_subscrs(pos fi_+ 1,buf);
                pos fi_+ 2 -> pos;
                n fi_+ 1 -> bsub
            endwhile;
            (iodev, 1, buf, pos fi_- 1)
        else
            (iodev, bsub, userbuf, nbytes)
        endif;
        if fast_subscrv(NTD_REM_NBYTES,dd) ->> nbytes then
            ;;; count output bytes
            dup() fi_+ nbytes -> fast_subscrv(NTD_REM_NBYTES,dd)
        endif;
        chain((), fast_syswrite)
    enddefine;

    define lconstant Read(dev, bsub, userbuf, nbytes);
        lvars   dev, bsub, userbuf, nbytes, dd = device_user_data(dev),
                buf, buf_nbytes, iodev, len, pos, n, strip, rem_nbytes,
                org_nbytes = nbytes;

        returnif(nbytes == 0) (0);
        subscrv(NTD_IODEV,dd) -> iodev;
        fast_subscrv(NTD_BUF,dd) -> buf;
        if buf and (fast_subscrv(NTD_BUF_NBYTES,dd) ->> buf_nbytes) == 0 then
            datalength(buf) -> len;
            if nbytes >= len then
                false -> buf
            else
                fast_sysread(iodev, 1, buf, len) -> buf_nbytes;
                1 -> subscrv(NTD_BUF_POS,dd)
            endif
        endif;
        if fast_subscrv(NTD_REM_NBYTES,dd) ->> rem_nbytes then
            fi_min(nbytes, rem_nbytes) -> nbytes
        endif;
        if buf then
            fast_subscrv(NTD_BUF_POS,dd) -> pos;
            fi_min(buf_nbytes, nbytes) -> nbytes;
            if nbytes == 1 then
                fast_subscrs(pos,buf) -> fast_subscrs(bsub,userbuf)
            else
                move_subvector(pos, buf, bsub, userbuf, nbytes)
            endif;
            buf_nbytes fi_- nbytes -> fast_subscrv(NTD_BUF_NBYTES,dd);
            pos fi_+ nbytes -> fast_subscrv(NTD_BUF_POS,dd)
        else
            fast_sysread(iodev, bsub, userbuf, nbytes) -> nbytes
        endif;
        if rem_nbytes then
            rem_nbytes fi_- nbytes -> fast_subscrv(NTD_REM_NBYTES,dd)
        endif;

        returnunless(fast_subscrv(NTD_NET_ASCII,dd) ->> strip) (nbytes);

        ;;; net ascii -- strip c/r before newline
        bsub fi_+ nbytes fi_- 1 -> len;     ;;; subscr of last char in buffer
        if isinteger(strip) then
            ;;; buffered char from last read
            true -> fast_subscrv(NTD_NET_ASCII,dd);
            if nbytes == 0 then
                strip -> fast_subscrs(bsub,userbuf);
                return(1)
            elseunless strip == `\r`
            and fast_subscrs(bsub,userbuf) == `\n` then
                if nbytes == org_nbytes then
                    ;;; rebuffer the last char before shifting up
                    fast_subscrs(len,userbuf) -> fast_subscrv(NTD_NET_ASCII,dd);
                    nbytes fi_- 1 -> nbytes
                endif;
                move_subvector(bsub, userbuf, bsub fi_+ 1, userbuf, nbytes);
                bsub fi_+ nbytes -> len;
                nbytes fi_+ 1 -> nbytes;
                strip -> fast_subscrs(bsub,userbuf)
            endif
        endif;

        bsub -> pos;
        while pos fi_<= len and (locchar(`\r`, pos, userbuf) ->> pos)
        and pos fi_<= len do
            fast_subscrv(NTD_NET_ASCII,dd) -> strip;
            if pos == len then
                if isinteger(strip) then
                    if strip == `\n` then
                        `\n` -> fast_subscrs(pos,userbuf);
                        true -> fast_subscrv(NTD_NET_ASCII,dd)
                    endif;
                    return(nbytes)
                else
                    ;;; don't know next char -- buffer the c/r
                    `\r` -> fast_subscrv(NTD_NET_ASCII,dd);
                    returnunless(nbytes == 1) (nbytes fi_- 1);
                    chain(dev, bsub, userbuf, org_nbytes, Read)
                endif
            elseif fast_subscrs(pos fi_+ 1 ->> n,userbuf) == `\n` then
                ;;; newline follows -- remove c/r
                if n == len then
                    `\n` -> fast_subscrs(pos,userbuf)
                else
                    move_subvector(n, userbuf, pos, userbuf, len fi_- n fi_+ 1)
                endif;
                if isinteger(strip) then
                    ;;; can unbuffer the char
                    strip -> fast_subscrs(len,userbuf);
                    true -> fast_subscrv(NTD_NET_ASCII,dd)
                else
                    nbytes fi_- 1 -> nbytes;
                    len fi_- 1 -> len
                endif
            endif;
            n -> pos
        endwhile;
        return(nbytes)
    enddefine;      /* Read */

    define lconstant Test_input(dev);
        lvars dev, dd = device_user_data(dev), buf_nbytes = 0;
        if subscrv(NTD_BUF,dd) then
            fast_subscrv(NTD_BUF_NBYTES,dd) -> buf_nbytes
        endif;
        if isinteger(fast_subscrv(NTD_NET_ASCII,dd)) then
            buf_nbytes fi_+ 1 -> buf_nbytes
        endif;
        if buf_nbytes /== 0 then
            buf_nbytes
        else
            sys_input_waiting(subscrv(NTD_IODEV,dd))
        endif
    enddefine;

    define lconstant Clear_input(dev);
        lvars dev, dd = device_user_data(dev);
        0 -> subscrv(NTD_BUF_NBYTES,dd);
        1 -> subscrv(NTD_BUF_POS,dd);
        subscrv(NTD_NET_ASCII,dd) and true -> subscrv(NTD_NET_ASCII,dd);
        sys_clear_input(subscrv(NTD_IODEV,dd))
    enddefine;

    lvars p_vec = {%
        mode == 0 and {^Read ^Test_input ^Clear_input},
        mode == 1 and {^Write ^Flush},
        false,
        close_p
    %};

    initv(NTD_VEC_LEN) -> dd;
    iodev       -> subscrv(NTD_IODEV, dd);
    false       -> subscrv(NTD_REM_NBYTES, dd);
    buffer      -> subscrv(NTD_BUF, dd);
    0           -> subscrv(NTD_BUF_NBYTES, dd);
    1           -> subscrv(NTD_BUF_POS, dd);
    net_ascii   -> subscrv(NTD_NET_ASCII, dd);
    []          -> subscrv(NTD_ATTR_LIST, dd);
    consdevice(name, fullname, dd, 0, p_vec) -> dev;
    sysclose -> sys_process_destroy_action(dev);
enddefine;

endsection;
