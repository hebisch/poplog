/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/ved_oci.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
uses ved_oc;
uses newvedfileprop;

section $-objectclass => ved_oci;

vars ignore_buffer = '*IGNORE*';
vars procedure is_ignore_buffer = newvedfileprop();

define display_ignore();
    if is_ignore_buffer() == true then
        vedtopfile();
        unless vedatend() do
            ved_clear()
        endunless;
        applist( vved_oc_ignore_list, vedinsertstring <> vednextline );
        if vved_oc_ignore_list = [] then
            vedputmessage( 'no expressions' )
        else
            vedputmessage( 'current expressions' )
        endif;
    endif
enddefine;

define del_ignore( regexp ); lvars regexp;
    if member( regexp, vved_oc_ignore_list ) then
        delete( regexp, vved_oc_ignore_list ) -> vved_oc_ignore_list;
        vedputmessage( 'Removing ' sys_>< regexp );
    else
        vedputmessage( 'Not being ignored (' sys_>< regexp sys_>< ')' )
    endif;
enddefine;

define add_ignore( regexp ); lvars regexp;
    delete( regexp, vved_oc_ignore_list ) -> vved_oc_ignore_list;
    [^regexp ^^vved_oc_ignore_list] -> vved_oc_ignore_list;
    vedputmessage( 'Ignoring ' sys_>< regexp );
enddefine;


define show_ignore();

    if is_ignore_buffer() == true then
        vedputmessage( 'Delete or Reread? Type \'d\', \'r\' or \'c\' to continue' );
        lvars ch = rawcharin();
        if ch == `r` then
            [] -> vved_oc_ignore_list;
            vedtopfile();
            until vedatend() do
                add_ignore( vedthisline() );
                vednextline()
            enduntil;
            ;;; The above will typically reverse the ignore list.
            ;;; It is more helpful to the user if it is left alone.
            vved_oc_ignore_list.rev -> vved_oc_ignore_list
        elseif ch == `d` then
            del_ignore( vedthisstring() )
        endif;
    else
        vededitor( vedhelpdefaults, ignore_buffer );
        true -> is_ignore_buffer();
    endif
enddefine;

define global procedure ved_oci();
    if vedargument = nullstring then
        show_ignore()
    elseif vedargument( 1 ) == `+` then
        add_ignore( allbutfirst( 1, vedargument ) )
    elseif vedargument( 1 ) == `-` then
        del_ignore( allbutfirst( 1, vedargument ) )
    else
        vederror( 'Unrecognised argument' )
    endif;
    display_ignore();
enddefine;

endsection;
