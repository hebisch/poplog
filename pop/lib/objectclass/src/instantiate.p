/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/instantiate.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Instantiate a tree ---------------------------------------------------
;;;
;;; (1) Refines the mapping from
;;;         abstract classes -> trees
;;;     into
;;;         concrete keys -> trees
;;;
;;; (2) Removes mixins.
;;;
;;; (3) Eliminates null trees.
;;;

compile_mode :pop11 +strict;

section $-objectclass;

;;; This flag is used to determine whether or not the instantiation of
;;; the tree can assume that the actions are chained into or merely
;;; applied.  For normal methods this is <true>.
;;;
vars ChainFlag;

define_lconstant_procedure Without_keys( tree, keys ) -> tree; lvars tree, keys;
    copy( tree ) -> tree;
    [%
        lvars b, k;
        for b in nextTree( tree ) do
            lvars ks = filterout_lmember( b.keysBranch, keys );
            unless ks.null then
                newBranch( ks, b.treesBranch )
            endunless;
        endfor;
    %] -> tree.nextTree;
enddefine;

define_lconstant_procedure Improve_trees( trees ); lvars trees;
    ;;; There must be more than one tree to get here!  No need
    ;;; to test again.
    [%
        lvars done = [];
        lvars tree;
        for tree in trees do
            lvars wtree = Without_keys( tree, done );
            unless nullTree( wtree ) do
                wtree
            endunless;
            quitif( tree.cantFailTree );
            lvars b, t;
            for b in tree.nextTree do
                if oneCantFail( b.treesBranch ) then
                    b.keysBranch <> done -> done;
                endif;
            endfor;
        endfor;
    %]
enddefine;

define_lconstant_procedure Classes_to_trees( cs, tree_of_class ); lvars cs, tree_of_class;
    ;;; There must be more than one tree to get here!
    if ChainFlag then
        Improve_trees
    else
        identfn
    endif( maplist( cs, tree_of_class ) );
enddefine;

;;; Now merge as many keys as possible.  This ensures the maximum
;;; amount of code sharing.  It also speeds up the code slightly, so
;;; it is important to do this.
;;;
;;; This code distinguishes between the cases of keys that map
;;; into several classes and keys that map into a single class.
;;; This significantly improves performance.
;;;
define_lconstant_procedure Find_sharing( key_to_classes, tree_of_class );
    lvars procedure( key_to_classes, tree_of_class );

    lvars procedure simple = newproperty( [], 64, [], "perm" );
    lvars procedure sharing_classes = newmapping( [], 64, [], 1 );
    lvars sharing_trees = copy( sharing_classes );

    fast_appproperty(
        key_to_classes,
        procedure( k, cs ); lvars k, cs;
            if cs == [] then
                ;;; There are always classes corresponding to keys.
                internal_error()
            elseif more_than_one( cs ) then
                ;;; This is the general case.
                ;;; lvars ts = Classes_to_trees( cs, tree_of_class );
                k :: sharing_classes( cs ) -> sharing_classes( cs );
            else
                ;;; There is exactly one tree.
                lvars t = tree_of_class( cs.front );
                k :: simple( t ) -> simple( t );
            endif
        endprocedure
    );

    fast_appproperty(
        sharing_classes,
        procedure( cs, ks ); lvars ks, cs;
            lvars ts = Classes_to_trees( cs, tree_of_class );
            sharing_trees( ts ) <> ks -> sharing_trees( ts );
        endprocedure
    );

    ;;; Finally, convert the sharing table back into a list of Branch-pairs.
    [%
        fast_appproperty(
            sharing_trees,
            procedure( ts, ks ); lvars ts, ks;
                newBranch( ks, ts )
            endprocedure
        );
        fast_appproperty(
            simple,
            procedure( t, ks ); lvars t, ks;
                unless nullTree( t ) do
                    newBranch( ks, [^t] )
                endunless
            endprocedure
        )
    %];
enddefine;



;;; Now resolve rivalry for keys.  (Only need to do this for keys with
;;; multiple classes.  So marking which keys are competed for is just
;;; an efficiency thing.)
;;;
;;; This procedure modifies "key_to_classes" so that the classes are
;;; ordered.  They are sequenced by the order in which they are
;;; found by searching through the class hierarchy from the basic key.
;;;
define_lconstant_procedure Resolve_rivalry( competed_for, key_to_classes ); lvars competed_for, key_to_classes;

    define lconstant Order_classes_of_key( classes, key );
        dlvars classes; lvars key;

        define lconstant Push_class( x ); lvars x;
            if fast_lmember( x, classes ) then x endif
        enddefine;

        [% appsupers( key, Push_class ) %]
    enddefine;

    fast_appproperty(
        competed_for,
        procedure( k, bool ); lvars k, bool;
            ;;; We just indicate the preference order.
            Order_classes_of_key( key_to_classes( k ), k ) -> key_to_classes( k )
        endprocedure
    );
enddefine;

define lconstant procedure clobber_action( t ); lvars t;
    lvars act = t.actionTree, inv;
    if
        ChainFlag and ( act.get_invokes_call_next_method ->> inv )
    then
        ;;; Because of the way we invoke call next method, we
        ;;; take this opportunity to eliminate superfluous
        ;;; distinctions.
        inv == CALL_MODE and c_invoke_call_next_method or u_invoke_call_next_method -> t.actionTree
    endif;
enddefine;

define_lconstant_procedure Make_branches( class_trees, all_keys_of_class ); lvars class_trees, procedure all_keys_of_class;

    lvars competition = false;
    lvars procedure competed_for = newproperty( [], 32, [], "perm" );
    lvars procedure key_to_classes = newproperty( [], 64, [], "perm" );
    lvars procedure tree_of_class = newproperty( [], 64, [], "perm" );

    lvars ct, k;
    for ct in class_trees do
        lvars ( c, t ) = ct.explode;
        ;;; clobber_action( t );
        t -> tree_of_class( c );
        for k in all_keys_of_class( c ) do
            lvars cs = key_to_classes( k );
            unless cs == [] do
                ;;; There's already a class that covers this key.  Better
                ;;; register it for later resolution.
                true ->> competed_for( k ) -> competition;
            endunless;
            conspair( c, cs ) -> key_to_classes( k );
        endfor;
    endfor;

    if competition then
        Resolve_rivalry( competed_for, key_to_classes );
    endif;
    Find_sharing( key_to_classes, tree_of_class );      ;;; returns branches in
enddefine;                                              ;;; new format.

define_lconstant_procedure Simple_make_branches( class_tree, all_keys_of_class );
    lvars class_tree, all_keys_of_class;
    lvars ( c, t ) = class_tree.explode;
    ;;; clobber_action( t );
    if nullTree( t ) then
        []
    else
        lvars ks = filterout( all_keys_of_class( c ), ismixinclass );
        [% unless ks == [] do newBranch( ks, [^t] ) endunless %]
    endif;
enddefine;


;;; -- Instantiate the tree representing a method: optimisation -------------

define_lconstant_procedure All_keys_of_class( C ); lvars C;
    lvars classes = [^C];
    lvars procedure isseen = newproperty( [], 20, false, "perm" );
    revconslist(#|
        until classes == [] do
            lvars c = classes.destpair -> classes;
            unless isseen( c ) do
                true -> isseen( c );
                [% dump_infs_of_class( c ) % ^^classes] -> classes;
                unless c.ismixinclass do
                    c
                endunless;
            endunless;
        enduntil;
    |#)
enddefine;

;;; Remove any branches whose only effect would be identical to
;;; the default action.
;;;
define lconstant prune_branches( bs, default_action ) -> bs; lvars bs, default_action;
    if ChainFlag then
        lvars b;
        for b in bs do
            lvars ts = b.treesBranch;
            nextunless( exactly_one( ts ) );
            lvars t = ts.hd;
            nextunless( t.simpleTree );
            nextunless( t.actionTree == default_action );
            delete( b, bs, nonop == ) -> bs;
        endfor;
    endif
enddefine;

define do_instantiate( tree, all_keys_of_class ); lvars tree, all_keys_of_class;
    if tree then
        clobber_action( tree );         ;;; Merge actions if possible.
        do_instantiate( tree.defaultTree, all_keys_of_class );
        lvars branches = tree.nextTree;
        unless branches == [] do        ;;; Efficiency hack.
            lvars b, t;
            for b in branches do
                do_instantiate( b(2), all_keys_of_class );
            endfor;
            prune_branches(
                if more_than_one( branches ) then
                    Make_branches( branches, all_keys_of_class )
                else
                    Simple_make_branches( branches(1), all_keys_of_class )
                endif,
                tree.actionTree
            ) -> tree.nextTree;
        endunless;
    endif
enddefine;

define instantiate( tree, ChainFlag ); lvars tree;
    dlocal ChainFlag;
    do_instantiate( tree, memofn( All_keys_of_class ) );
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Integral Solutions Ltd, Sep  1 1994 (Julian Clinton)
        Removed spurious assignment.
 */
