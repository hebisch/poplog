/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/lib/lib/item_stream_saver.p
 > Purpose:
 > Author:          John Gibson, Jul  9 1998 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

lconstant macro (
    FILE_MAGIC      = 36:SAVITS,

    T_EOF           = 0,
    T_STRING        = 1,
    T_STRING16      = 2,
    T_WORD_FIRST    = 3,
    T_WORD16_FIRST  = 4,
    T_WORD_SUBSQ    = 5,
    T_INTEGRAL_+    = 6,
    T_INTEGRAL_-    = 7,
    T_FLOAT         = 8,
    T_RATIO         = 9,
    T_COMPLEX       = 10,
);

define lconstant subscr_nbyte(sub, string, n) -> val;
    lvars n, val = 0, sub, string;
    sub fi_+ n -> sub;
    repeat
        sub fi_- 1 -> sub;
        fast_subscrs(sub, string) || val -> val;
        n fi_- 1 -> n;
        quitif(n == 0);
        val << 8 -> val
    endrepeat
enddefine;
;;;
define updaterof subscr_nbyte(val, sub, string, n);
    lvars n, val, sub, string;
    repeat
        val && 16:FF -> fast_subscrs(sub, string);
        n fi_- 1 -> n;
        quitif(n == 0);
        sub fi_+ 1 -> sub;
        val >> 8 -> val
    endrepeat
enddefine;

define item_stream_saver_in(fname);
    lvars   buf = inits(128), next_word_num = 1, wordvec,
            indev = sysopen(fname, 0, true, `N`);

    define read_into(buf, n);
        unless fast_sysread(indev, 1, buf, n) == n then
            mishap(indev, 1, 'ERROR READING DEVICE (premature eof)')
        endunless
    enddefine;

    define input_count();
        lvars n = fast_subscrs(2,buf);
        returnif(n /== 16:FF) (n);
        read_into(buf, 2);
        subscr_nbyte(1, buf, 2) -> n;
        returnif(n /== 16:FFFF) (n);
        read_into(buf, 4);
        subscr_nbyte(1, buf, 4)
    enddefine;

    define input_integral();
        lvars n;
        returnif((input_count() ->> n) == 0) (0);
        if n fi_> datalength(buf) then inits(n<<1) -> buf endif;
        read_into(buf, n);
        subscr_nbyte(1, buf, n);
    enddefine;

    define input_string(make_word);
        lvars n = input_count();
        if n fi_> datalength(buf) then inits(n<<1) -> buf endif;
        read_into(buf, n);
        if make_word then subword(1, n, buf) else substring(1, n, buf) endif
    enddefine;

    define input_string16(make_word);
        lvars i, n = input_count(), nbytes = n fi_+ n;
        if nbytes fi_> datalength(buf) then inits(nbytes<<1) -> buf endif;
        read_into(buf, nbytes);
        fast_for i by 2 to nbytes do
            (fast_subscrs(i fi_+ 1,buf) fi_<< 8) fi_+ fast_subscrs(i,buf)
        endfor;
        if make_word then consword(n) else consstring(n) endif
    enddefine;

    define get_next_item();
        lvars i, n;
        read_into(buf, 2);
        go_on fast_subscrs(1,buf) to Type:
            Type T_EOF :
                sysclose(indev);
                return(termin);

            Type T_STRING :
                return(input_string(false));

            Type T_STRING16 :
                return(input_string16(false));

            Type T_WORD_FIRST :
                input_string(true) ->> fast_subscrv(next_word_num,wordvec);
                next_word_num fi_+ 1 -> next_word_num;
                return();

            Type T_WORD16_FIRST :
                input_string16(true) ->> fast_subscrv(next_word_num,wordvec);
                next_word_num fi_+ 1 -> next_word_num;
                return();

            Type T_WORD_SUBSQ :
                return(fast_subscrv(input_count(),wordvec));

            Type T_INTEGRAL_+ :
                return(input_integral());

            Type T_INTEGRAL_- :
                return(-input_integral());

            Type T_FLOAT :
                fast_subscrs(2,buf) ->> n -> i;
                read_into(buf, n);
                lblock
            compile_mode :vm -bjmpch;
                until i == 0 do
                    fast_subscrs(i,buf);
                    i fi_- 1 -> i
                enduntil;
                endlblock;
                return(n -> float_code_bytes());

            Type T_RATIO :
                return(get_next_item() / get_next_item());

            Type T_COMPLEX :
                return(get_next_item() +: get_next_item())
        endgo_on
    enddefine;

    read_into(buf, 4);
    unless subscr_nbyte(1, buf, 4) = FILE_MAGIC then
        mishap(fname, 1, 'NOT AN ITEM STREAM SAVE FILE')
    endunless;

    read_into(buf, 4);
    initv(subscr_nbyte(1, buf, 4)) -> wordvec;
    get_next_item
enddefine;

define item_stream_saver_out(fname);
    lvars   wordtable = newanyproperty([], 2048, 1, 1600, false, false, "perm", false, false),
            wordcount = 1,
            outdev = syscreate(fname, 2, true), buf = inits(16);

    define output_byte(char);
        char -> fast_subscrs(1,buf);
        syswrite(outdev, buf, 1)
    enddefine;

    define output_count(n);
        if n < 16:FF then
            n -> fast_subscrs(1,buf);
            1
        elseif n < 16:FFFF then
            16:FF -> fast_subscrs(1,buf);
            n -> subscr_nbyte(2, buf, 2);
            3
        else
            16:FFFFFF -> subscr_nbyte(1, buf, 3);
            n -> subscr_nbyte(4, buf, 4);
            7
        endif -> n;
        syswrite(outdev, buf, n)
    enddefine;

    define output_string(string);
        lvars len = datalength(string);
        output_count(len);
        syswrite(outdev, string, len)
    enddefine;

    define output_string16(string);
        lvars n, c, len = datalength(string);
        output_count(len);
        fast_for n to len do
            fast_subscrs(n, string) -> c;
            output_byte(c fi_&& 16:FF);
            output_byte(c fi_>> 8);
        endfor;
    enddefine;

    define output_integral(int);
        output_count((integer_length(int)+7) div 8);
        until int == 0 do
            output_byte(int && 16:FF);
            int >> 8 -> int
        enduntil
    enddefine;

    define output_item(item);
        lvars n;
        if item == termin then
            output_byte(T_EOF);
            output_byte(0);
            sysseek(outdev, 4, 0);      ;;; get to wordcount position
            wordcount-1 -> subscr_nbyte(1, buf, 4);
            syswrite(outdev, buf, 4);
            sysclose(outdev);
            clearproperty(wordtable);
        elseif isstring(item) then
            if isstring16(item) then
                output_byte(T_STRING16);
                output_string16(item)
            else
                output_byte(T_STRING);
                output_string(item)
            endif
        elseif isword(item) then
            if wordtable(item) ->> n then
                output_byte(T_WORD_SUBSQ);
                output_count(n)
            else
                wordcount -> wordtable(item);
                wordcount + 1 -> wordcount;
                fast_word_string(item) -> item;
                if isstring16(item) then
                    output_byte(T_WORD16_FIRST);
                    output_string16(item)
                else
                    output_byte(T_WORD_FIRST);
                    output_string(item)
                endif
            endif
        elseif isintegral(item) then
            if item < 0 then
                output_byte(T_INTEGRAL_-);
                -item -> item
            else
                output_byte(T_INTEGRAL_+);
            endif;
            output_integral(item)
        elseif isdecimal(item) then
            output_byte(T_FLOAT);
            float_code_bytes(item) -> n;
            output_count(n);
            fast_repeat n times output_byte() endrepeat
        elseif isratio(item) then
            output_byte(T_RATIO);
            output_byte(0);
            output_item(destratio(item) -> n);
            output_item(n)
        elseif iscomplex(item) then
            output_byte(T_COMPLEX);
            output_byte(0);
            output_item(destcomplex(item) -> n);
            output_item(n)
        else
            mishap(item, 1, 'INVALID ITEM')
        endif
    enddefine;

    ;;; write magic number
    FILE_MAGIC -> subscr_nbyte(1, buf, 4);
    syswrite(outdev, buf, 4);

    ;;; skip 4 bytes for wordcount
    sysseek(outdev, 4, 1);

    output_item
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Julian Clinton, Jan 15 1999
        Changed to use a property table instead of a list to store words.
 */
