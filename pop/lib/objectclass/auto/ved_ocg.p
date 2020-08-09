/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/ved_ocg.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
section $-objectclass => ved_ocg;

uses oc_utils;
uses ved_oc;

define part_props( p ); lvars p;

    define lconstant props( p ); lvars p;
        if p.isprocedure then
            p.pdprops
        else
            p
        endif
    enddefine;

    if p.iscslot then
        'SLOT ' sys_>< p.xslot_identity.props
    elseif p.isuslot then
        'UPDATE SLOT ' sys_>< p.xslot_identity.props
    elseif p.isprocedure then
        'PROC ' sys_>< p.pdprops
    else
        p
    endif;
enddefine;

define browse_generic( G ); lvars G;
    dlocal cucharout = vedcharinsert;

    define lconstant summary( G, mode ); lvars G, mode;
        dlocal pop_pr_quotes = false;
        lvars mt = G.method_table;
        lvars Arity = mode == UCALL_MODE and uArityMethodTable or cArityMethodTable;
        lvars Entries = mode == UCALL_MODE and uEntriesMethodTable or cEntriesMethodTable;
        nprintf( 'pdprops:            %p', [% G.pdprops %] );
        nprintf( 'pdnargs:            %p', [% G.pdnargs %] );
        nprintf( 'effective arity:    %p', [% mt.Arity %] );
        if mode == CALL_MODE then
            nprintf( 'linked:             %p', [% G.isunlinked_method and 'no' or 'yes' %] );
        endif;
        nprintf( 'number of methods:  %p', [% mt.Entries.length %] );
        nl(1);
        npr( 'Listing of methods' );
        lvars i;
        for i in mt.Entries do
            lvars ( tv, act ) = destEntry( i );
            unless ismeltedtmpvec( tv ) do
                lvars c;
                for c in [% explode_tmpvec( tv ) %].rev do
                    spr( c and class_name( c ) or '<any>' )
                endfor;
                appdata( ' ->  ', cucharout );
                npr( part_props( act ) );
            endunless
        endfor;
    enddefine;

    lvars s =
        sprintf(
            'Summary of Generic Procedure "%p"',
            [% G.pdprops %]
        );
    npr( s );
    repeat s.length times cucharout( `-` ) endrepeat; nl(1);
    summary( G, CALL_MODE );

    if
        G.updater and not( G.isunlinked_method ) or
        G.updater and G.method_table.uEntriesMethodTable.length > 0
    then
        nl(2);
        lvars s =
            sprintf(
                'Summary of Updater of Generic Procedure "%p"',
                [% G.updater.pdprops %]
            );
        npr( s );
        repeat s.length times cucharout( `-` ) endrepeat; nl(1);
        summary( G, UCALL_MODE );
    endif;
    nl( 2 );
enddefine;

define process_ved_ocg_args( arg ); lvars arg;
    [%
        repeat quitif( arg.length == 0 );
            if arg(1) == `\s` then
                allbutfirst( 1, arg ) -> arg
            else
                lvars c = strmember( ` `, arg );
                if c then
                    substring( 1, c-1, arg );
                    allbutfirst( c, arg ) -> arg;
                else
                    arg;
                    nullstring -> arg;
                endif;
            endif
        endrepeat;
    %]
enddefine;

define global vars ved_ocg();

    define lconstant pdstr( x ); lvars x;
        x.pdprops sys_>< nullstring
    enddefine;

    lvars regexps = process_ved_ocg_args( vedargument );
    lvars filter = compile_filter( regexps, pdstr );
    lvars ms =
        sort_by(
            [%
                procedure( M ); lvars M;
                    if filter( M ) then M endif
                endprocedure.app_all_methods
            %],
            pdstr,
            alphabefore
        );
    unless ms.null do
        lvars t = systmpfile( false, 'oc', '.tmp' );
        vededitor( vedhelpdefaults, t );
        dlocal vedlinemax = pop_max_int;
        applist( ms, browse_generic );
        vedtopfile();
    else
        vedputmessage( 'NO MATCHING GENERIC PROCEDURES' )
    endunless
enddefine;

endsection;
