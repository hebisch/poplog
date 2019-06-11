/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_ftp.p
 > Purpose:         Read/write files with ftp
 > Author:          John Gibson, Dec  7 1995
 > Documentation:   REF * VEDCOMMS
 */
compile_mode :pop11 +strict;

include vm_flags.ph;

section;

define vars vedftpdefaults();
    vedveddefaults()
enddefine;

define vars ved_ftp;
    lvars arg, url, name, dev, a, c, slash, hlist, allow_write = false;

    define lconstant defaults();

        define lconstant write_current(explicit);
            lvars explicit, dev;
            returnunless(explicit and vedchanged);
            vedputmessage('OPENING\s' <> arg);
            sys_open_url(url, 1, false) -> dev;
            vedputmessage(nullstring);
            vedputmessage('WRITING ' <> arg);
            vedwriterange(dev, 1, max(1, vvedbuffersize));
            sysclose(dev);
            vedputmessage(arg <> ' WRITTEN');
            false -> vedchanged
        enddefine;

        'ftp\s' <> arg -> vedcurrent;
        allow_write -> vedwriteable;
        vedftpdefaults();
        dlocal pop_vm_flags = pop_vm_flags || VM_NOPROT_PVARS;
        true -> is_vedfile_local(ident ved_write_current_file,
                                                    ved_current_file);
    compile_mode :vm -prmprt;
        write_current -> ved_write_current_file
    enddefine;


    vedsetup();
    vedargument -> arg;
    if isstartstring('ftp://', arg) then
        allbutfirst(6, arg) -> arg
    elseif isstartstring('://', arg) then
        allbutfirst(3, arg) -> arg
    endif;
    unless locchar(`/`, 1, arg) ->> slash then
        arg <> '/' -> arg;
        datalength(arg) -> slash
    endunless;
    'ftp://' <> arg -> url;
    'URL\s' <> url -> name;
    if vedpresent(name) then
        vededit(name)
    else
        ;;; remove any password from display name
        if (locchar(`@`,1,arg) ->> a) and a < slash then
            ;;; has user
            true -> allow_write;
            if (locchar(`:`,1,arg) ->> c) and c < a then
                substring(1, c-1, arg) <> allbutfirst(a-1, arg) -> arg
            endif
        endif;

        vedputmessage('OPENING\s' <> arg);
        if sys_open_url(url, 0, false, `F`) ->> dev then
            dev -> name;
            subscrv(1,device_user_data(dev)) -> hlist;
            if hd(tl(lmember_=('content-type:', hlist))) = 'text/dirlist' then
                false -> allow_write
            endif
        endif;
        vedputmessage(nullstring);
        vededit([^name ftp], defaults)
    endif
enddefine;


endsection;
