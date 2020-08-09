/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/net_to_tree.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Transform entries to a tree ------------------------------------------

compile_mode :pop11 +strict;

section $-objectclass;


define_lconstant_procedure InitTree();
    newTree( false, [], false )
enddefine;

define_lconstant_procedure Lookup_next( class, branches ); lvars class, branches;
    until branches == [] do
        lvars c = branches.destpair -> branches;
        if class == c(1) then
            return( c(2) )
        endif
    enduntil;
    return( false );
enddefine;

define_lconstant_procedure Add_tree( classes, action, tree ); lvars classes, action, tree;
    if classes == [] then
        action -> actionTree( tree )
    else
        lvars class = classes.destpair -> classes;
        if class then
            lvars next = tree.nextTree;
            lvars t = Lookup_next( class, next );
            unless t do
                InitTree() -> t;
                [ {^class ^t} ^^next ] -> tree.nextTree;
            endunless;
            Add_tree( classes, action, t );
        else
            lvars t = tree.defaultTree;
            unless t do
                InitTree() ->> t -> tree.defaultTree;
            endunless;
            Add_tree( classes, action, t );
        endif;
    endif;
enddefine;

;;; A net is simply a list of entries.
;;;
define net_to_tree( net ) -> tree; lvars net, tree;
    lvars tree = InitTree();
    lvars i;
    for i in net do
        Add_tree( [% i.classesEntry.explode_tmpvec %], i.actionEntry, tree );
    endfor;
enddefine;

endsection;
