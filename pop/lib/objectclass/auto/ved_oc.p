/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/ved_oc.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;
uses oc_utils;

section $-objectclass =>
    ved_oc
    vved_oc_defaults
    vved_oc_ignore
    vved_oc_ignore_list
    vved_oc_implicit_classes
;

global vars vved_oc_defaults = nullstring;          ;;; no added options
global vars vved_oc_ignore = [];                    ;;; ignore none
global vars vved_oc_ignore_list = [];               ;;; ignore none
global vars vved_oc_implicit_classes = nullvector;  ;;; accept all


;;; -- display options ------------------------------------------------------

;;; This variable determines the order in which the fields are
;;; displayed.
;;;
vars display_options = [type parents children slots];

;;; -- compile a name filter ----------------------------------------------

define compile_filter( f, get_name ); lvars f, get_name;
    if f.isstring then
        lvars ( err, procedure p ) = regexp_compile( '@^' <> f <> '@$' );
        if err then
            vederror( 'Regular expression error (' <> err <> ')' )
        else
            procedure( n ); lvars n;
                erase( p( 1, n.get_name, false.dup ) ) and true
            endprocedure
        endif
    elseif f.isword then
        compile_filter( f.word_string, get_name )
    elseif f.isprocedure then
        procedure( n ); lvars n;
            check_one(#| f( n ) |#)
        endprocedure
    elseif f = nil then
        procedure(); erase(); false endprocedure
    elseif f = nullvector then
        procedure(); erase(); true endprocedure
    elseif (f.islist or f.isvector) and f.length == 1 then
        compile_filter( f( 1 ), get_name )
    elseif f.islist then
        lvars ps = maplist( f, compile_filter(% get_name %) );
        procedure( n ); lvars n;
            lvars p;
            for p in ps do
                if p( n ) then
                    return( true )
                endif
            endfor;
            return( false )
        endprocedure
    elseif f.isvector then
        lvars ps = [% appdata( f, compile_filter(% get_name %) ) %];
        procedure( n ); lvars n;
            lvars p;
            for p in ps do
                unless p( n ) do
                    return( false )
                endunless
            endfor;
            return( true )
        endprocedure
    endif
enddefine;


;;; -- formatting procedures ------------------------------------------------

define insert_space();
    vedcharinsert( ` ` )
enddefine;

define insert_sp( n ); lvars n;
    applynum( insert_space, n )
enddefine;

define insert_sp_to( n ); lvars n;
    insert_sp( n - vedcolumn )
enddefine;


;;; -- formatting classes, slots, and lists --------------------------------

define insert_info( item ); lvars item;
    if item = [] then
        vedinsertstring( '(none)' )
    elseif item.islist then
        applist( item, insert_info <> insert_space )
    elseif item.isclass then
        insert_info( class_name( item ) )
    elseif item.isxslot then
        insert_info( xslot_name( item ) )
    elseif item.isword or item.isstring then
        vedinsertstring( item )
    endif
enddefine;

;;; -- class hierarchy accessors --------------------------------------------

define list_classes();
    [% identfn.app_all_classes %]
enddefine;

define get_class_info( c, flag ); lvars c, flag;
    if flag == "parents" then
        supers_of_class( c )
    elseif flag == "children" then
        property_domain( infs_of_class( c ) or newassoc( [] ) )
    elseif flag == "slots" then
        fields_of_class( c )
    elseif flag == "type" then
        if c.ismixinclass then
            'mixin'
        elseif c.issingletonclass then
            'singleton'
        elseif c.isobjectclass then
            'standard'
        else
            undef
        endif <>
        if c.isobsolete then
            ' (obsolete)'
        else
            ''
        endif;
    else
        undef
    endif
enddefine;


;;; -- the ved command ----------------------------------------------------

define process_args( arg ) -> ( regexps, flags );
    lvars arg, regexps, flags = newassoc( [] );
    repeat
        quitif( arg.length == 0 );
        lvars flag;
        lvars tick = arg(1);
        if tick == `-` or tick == `+` then
            lvars c = strmember( ` `, arg );
            if c then
                substring( 2, c-2, arg ) -> flag;
                allbutfirst( c, arg ) -> arg;
            else
                allbutfirst( 1, arg ) -> flag;
                nullstring -> arg;
            endif
        elseif tick == ` ` then
            allbutfirst( 1, arg ) -> arg;
            nextloop;
        else
            quitloop
        endif;
        lvars addtick = tick == `+`;
        if flag = 'p' or flag = 'parents' then
            addtick -> "parents".flags
        elseif flag = 'c' or flag = 'children' then
            addtick -> "children".flags
        elseif flag = 'A' or flag = 'Alphabetical' then
            "alphabetical" -> "sort".flags
        elseif flag = 'H' or flag = 'Hierarchical' then
            "hierarchical" -> "sort".flags
        elseif flag = 'R' or flag = 'Reverse' then
            addtick -> "rev".flags
        elseif flag = 's' or flag = 'slots' then
            addtick -> "slots".flags
        elseif flag = 't' or flag = 'type' then
            addtick -> "type".flags
        elseif flag = 'a' or flag = 'ancestors' then
            addtick -> "ancestors".flags
        elseif flag = 'd' or flag = 'descendants' then
            addtick -> "descendants".flags
        elseif flag = 'o' or flag = 'obsolete' then
            addtick -> "obsolete".flags
        elseif flag = 'l' or flag = 'leaf' then
            addtick -> "leaf".flags;
        elseif flag = 'r' or flag = 'root' then
            addtick -> "root".flags
        elseif flag = '+' or flag = '-' then
            ;;; turn on all display options.
            lvars i;
            for i in display_options do
                addtick -> i.flags;
            endfor;
        elseif flag.length >= 2 then
            consstring(#|
                tick, flag(1), ` `, tick, explode( allbutfirst( 1, flag ) ),
                ` `, explode( arg )
            |#) -> arg
        else
            vederror( 'INVALID OPTION "' sys_>< flag sys_>< '"' );
        endif;
    endrepeat;
    [] -> regexps;
    until arg.length == 0 do
        if arg(1) == ` ` then
            allbutfirst( 1, arg ) -> arg
        else
            lvars n = strmember( ` `, arg );
            if n then
                substring( 1, n-1, arg ) :: regexps -> regexps;
                allbutfirst( n, arg ) -> arg;
            else
                arg :: regexps -> regexps;
                nullstring -> arg
            endif
        endif
    enduntil;
    regexps.rev -> regexps;
enddefine;

define insert_desc( C, selection, sellen, maxlen ); lvars C, selection, sellen, maxlen;
    insert_info( C );
    if sellen == 0 then
        ;;; done.
    elseif sellen == 1 then
        lvars t = selection( 1 );
        insert_sp_to( 18 );
        insert_sp( 2 );
        vedinsertstring( t );
        vedinsertstring( ' : ' );
        insert_info( get_class_info( C, t ) )
    else
        vednextline();
        lvars t;
        for t in selection do
            lconstant indent = 4;
            insert_sp( indent );
            vedinsertstring( t );
            insert_sp_to( maxlen + indent + 2 );
            vedinsertstring( ': ' );
            insert_info( get_class_info( C, t ) );
            vednextline();
        endfor;
    endif;
    vednextline();
enddefine;

define isroot( C ); lvars C;
    null( supers_of_class( C ) )
enddefine;

define isleaf( C ); lvars C;
    lvars infs = infs_of_class( C );
    not( infs ) or null( property_domain( C ) )
enddefine;

define ancestors( C ); lvars C;
    C;
    applist( supers_of_class( C ), ancestors )
enddefine;

define descendants( C ); lvars C;
    C;
    applist( inferiors( C ), descendants )
enddefine;

define remove_duplicates( L ); lvars L;
    lvars t = newassoc( [] );
    lvars i;
    for i in L do true -> t( i ) endfor;
    property_domain( t )
enddefine;

define global ved_oc();
    dlocal pop_pr_quotes = false;
    lvars ( regexps, flags ) = process_args( vved_oc_defaults <> ' ' <> vedargument );
    lvars selection = filterin( display_options, flags );

    lvars classes = list_classes();
    if "obsolete".flags then
        filterout( classes, isobsolete ) -> classes
    endif;

    filterin(
        classes,
        compile_filter(
            regexps == [] and vved_oc_implicit_classes or regexps,
            class_name
        )
    ) -> classes;


    filterout(
        classes,
        compile_filter( [^vved_oc_ignore ^^vved_oc_ignore_list], class_name )
    ) -> classes;


    if "root".flags then
        filterin( classes, isroot ) -> classes
    endif;

    if "leaf".flags then
        filterin( classes, isleaf ) -> classes
    endif;

    lvars ancs =
        "ancestors".flags and maplist( classes, ancestors ) or [];

    lvars descs =
        "descendants".flags and maplist( classes, descendants ) or [];

    [^^ancs ^^descs ^^classes] -> classes;
    remove_duplicates( classes ) -> classes;

    sort_by(
        classes,
        if "sort".flags == "hierarchical" then
            get_hierarchy_numbering(), nonop <
        else
            ;;; default to alphabetical -- the "sort" flags may be <false>.
            class_name, alphabefore
        endif
    ) -> classes;

    apply( classes, "rev".flags and rev or identfn ) -> classes;

    lvars t = systmpfile( false, 'oc', '.tmp' );
    vededitor( vedhelpdefaults, t );

    dlocal vedlinemax = pop_max_int;

    applist(
        classes,
        insert_desc(%
            selection,
            selection.length,
            applist( 0, selection, length <> max )
        %)
    );

    vedtopfile();
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
--- Robert John Duncan, Nov 13 1995
        Exported various defaults vars documented but previously hidden.
 */
