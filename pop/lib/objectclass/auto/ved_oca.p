/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/ved_oca.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
section $-objectclass => ved_oca, ved_ocd;

uses ved_oc;

define make_tree( c, reln ); lvars c, reln;
    lvars seen = newassoc( [] );

    define lconstant make( c ); lvars c;
        c ::
        lblock
            lvars was_seen = seen( c );
            true -> seen( c );
            lvars rels = reln( c );
            [%
                if rels /= [] and was_seen then
                    [^false]
                else
                    applist( rels, make )
                endif
            %]
        endlblock
    enddefine;

    make( c )
enddefine;

define lconstant show( tree, level ); lvars tree, level;
    repeat level * 2 times cucharout( ` ` ) endrepeat;
    lvars c = hd( tree );
    if c then
        class_name( c )
    else
        '(as above)'
    endif.pr;
    nl( 1 );
    lvars i;
    for i in tl( tree ) do
        show( i, level+1 )
    endfor
enddefine;

define lconstant show_class_tree( reln ); lvars reln;
    if vedargument = nullstring then
        vederror( 'NO ARGUMENT' )
    endif;
    lvars name = vedargument.consword <> "_key";
    if name.isdefined then
        lvars val = identof( name ).idval;
        if val.isclass then
            procedure();
                dlocal cucharout = vedcharinsert;
                vededitor( vedhelpdefaults, systmpfile( false, 'tree', '' ) );
                show( make_tree( val, reln ), 0 );
                vedtopfile();
            endprocedure()
        else
            vederror( 'CLASS NAME REQUIRED' )
        endif
    else
        vederror( 'NO SUCH CLASS' )
    endif
enddefine;

define global vars procedure ved_oca();
    show_class_tree( supers_of_class )
enddefine;

define global vars procedure ved_ocd();
    show_class_tree( inferiors )
enddefine;

endsection;
