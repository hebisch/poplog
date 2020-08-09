/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/inheritance.p
 > Purpose:         Objectclass file
 > Author:          Robert John Duncan, Oct 12 1995 (see revisions)
 > Documentation:
 > Related Files:
 */

section $-objectclass;

;;; -- Computing the class precedence list --------------------------------

    ;;; forward
constant procedure ( class_name, class_precedence_list, );

define lconstant Ambiguous_inheritance(class, supers);
    class_name(class) -> class;
    maplist(supers, class_name) -> supers;
    mishap(sprintf('Ambiguous inheritance for class %p', [^class]), supers);
enddefine;

;;; dfs:
;;;     depth-first search, as used by prototype Objectclass 1.0

define lconstant dfs(class);
    lvars procedure seen = newproperty( [], 20, false, "perm" );

    define lconstant procedure Scan( here ); lvars here;
        unless seen( here ) do
            true -> seen( here );
            here, applist( supers_of_class( here ), Scan )
        endunless
    enddefine;

    [% Scan( class ) %]
enddefine;

;;; topological_sort:
;;;     as used by CLOS and Dylan. The graph (a property) encodes an
;;;     order relation on a superclass set, where the domain is the set
;;;     itself and each class is mapped to a list of its predecessors in
;;;     the relation

define lconstant topological_sort(class, graph) -> supers;
    lvars supers = [^class], count = length(graph)-1, possibles = [];
    until count == 0 do
        ;;; delete class from the graph: the node itself...
        false -> graph(class);
        ;;; ...and any edges from it; if this leaves a class without
        ;;; predecessors, that becomes a candidate for inclusion in
        ;;; the supers list
        [%  appproperty(graph,
                procedure(succ, preds);
                    delete(class, preds, nonop==) ->> preds -> graph(succ);
                    if preds == [] then succ endif;
                endprocedure)
        %] -> possibles;
        if possibles == [] then
            ;;; loop in the graph -- inconsistent
            quitloop;
        elseif back(possibles) == [] then
            ;;; only one choice
            front(possibles) -> class;
        else
            ;;; choose the one having a direct subclass most recent
            ;;; in the list of supers computed so far
            define choose(possibles, supers) -> chosen;
                lvars c, class, chosen = false;
                for class in supers do
                    for c in supers_of_class(class) do
                        returnif(fast_lmember(c, possibles) ->> chosen)
                            (hd(chosen) -> chosen);
                    endfor;
                endfor;
            enddefine;
            quitunless(choose(possibles, supers) ->> class);
        endif;
        class :: supers -> supers;
        count - 1 -> count;
    enduntil;
    fast_ncrev(supers) -> supers;
    unless count == 0 then
        ;;; inconsistency in the graph
        if possibles == [] then property_domain(graph) -> possibles endif;
        Ambiguous_inheritance(hd(supers), possibles);
    endunless;
enddefine;

;;; clos:
;;;     topological sort algorithm used by CLOS

define lconstant clos(class) -> supers;

    define buildGraph(class) -> procedure graph;
        ;;; the order relation is computed from the direct superclasses
        ;;; only

        define lvars graph =
            newanyproperty([], 32, 1, 28, false, false, "tmparg", false,
                false);
        enddefine;

        define visit(pred);
            returnif(graph(pred));
            [] -> graph(pred);
            lvars succ;
            for succ in supers_of_class(pred) do
                visit(succ);
                pred :: graph(succ) -> graph(succ);
                succ -> pred;
            endfor;
        enddefine;

        visit(class);
    enddefine;

    topological_sort(class, buildGraph(class)) -> supers;
enddefine;

;;; dylan:
;;;     improved topological sort algorithm used by Dylan, inspired by
;;;     Ducournau et. al. "Proposal for a Monotonic Multiple Inheritance
;;;     Linearization", OOPSLA '94, published in SIGPLAN NOTICES 29(10),
;;;     Oct. 1994, pp. 164--175.

define lconstant dylan(class) -> supers;

    define buildGraph(class) -> procedure graph;
        ;;; the order relation is computed from the direct superclasses
        ;;; plus the previously-computed class precedence lists (for
        ;;; monotonicity)

        define lvars graph =
            newanyproperty([], 32, 1, 28, false, false, "tmparg", false,
                false);
        enddefine;

        define addPredecessors(pred, classes);
            lvars succ;
            for succ in classes do
                lvars preds = graph(succ) or [];
                unless fast_lmember(pred, preds) then
                    pred :: preds -> graph(succ);
                endunless;
                succ -> pred;
            endfor;
        enddefine;

        [] -> graph(class);
        addPredecessors(class, supers_of_class(class));
        for class in supers_of_class(class) do
            addPredecessors(dest(class_precedence_list(class)));
        endfor;
    enddefine;

    topological_sort(class, buildGraph(class)) -> supers;
enddefine;

;;; class_precedence_list:
;;;     returns a list of all the superclasses of a class -- including
;;;     the class itself -- in inheritance order. The algorithm used for
;;;     computing inheritance is chosen by pop_oc_inheritance_algorithm:
;;;     it's a bad idea for this to be changed once it's been used, so
;;;     we print a warning in that case. The superclass list is computed
;;;     on the first access then cached for subsequent calls; we use a
;;;     temporary property so that old keys will be cleared from the
;;;     cache. Caching means that the list of superclasses doesn't
;;;     change, even if one or more of the superclasses is modified in a
;;;     way that changes the inheritance hierarchy: this is consistent
;;;     with the fact that other properties of derived classes -- slots,
;;;     etc. -- don't change either.

define class_precedence_list =
    lblock;
    define lconstant inheritance_algorithm =
        newproperty([
            [dfs        ^dfs]
            [clos       ^clos]
            [dylan      ^dylan]
        ], 8, false, "perm");
    enddefine;
    lvars last_p = false;
    newanyproperty([], 32, 1, 28, false, false, "tmparg", false,
        procedure(class, slf);
            lvars p = inheritance_algorithm(pop_oc_inheritance_algorithm);
            if not(p) then
                mishap('Unimplemented inheritance algorithm',
                    [^pop_oc_inheritance_algorithm]);
            elseif last_p and p /= last_p then
                warning('Inheritance algorithm has changed!',
                    [^pop_oc_inheritance_algorithm]);
            endif;
            p(class) ->> slf(class);
            p -> last_p;
        endprocedure);
    endlblock;
enddefine;

;;; iterate upwards over the inheritance-net, in inheritance order, over
;;; the supers of k, applying p at each point.
define appsupers(k, p);
    applist(class_precedence_list(k), p);
enddefine;

;;; return a list of all the pure superclasses of k, i.e. excluding k
;;; itself
define all_supers_of_class(class);
    tl(class_precedence_list(class));
enddefine;


;;; -- Inferiors of a class -----------------------------------------------

define lconstant procedure New_inf_map();
    newproperty( [], 4, false, "tmpboth" )
enddefine;

define add_inf( x, y ); lvars x, y;
    lvars t = infs_of_class( y );
    unless t then
        New_inf_map() ->> t -> infs_of_class( y )
    endunless;
    true -> t( x )
enddefine;

define dump_infs_of_class( c ); lvars c;
    lvars t = infs_of_class( c );
    if t then
        fast_appproperty( t, erase )
    endif
enddefine;

;;; Returns all the inferiors of the keys (list) supplied.  All duplicates
;;; are removed.  Used by relink_method.  Leaves the results on the stack.
;;;
define all_infs( keys ); lvars keys;
    lvars isseen = newproperty( [], 20, false, "perm" );
    until null( keys ) do
        lvars k = keys.dest -> keys;
        unless isseen( k ) do
            true -> isseen( k );
            [% dump_infs_of_class( k ) % ^^keys] -> keys;
            k;
        endunless;
    enduntil;
enddefine;

;;; In the case of extantclasses, we need to be able to flush the properties
;;; for the key.  This guards against recompilation of the extantclass with
;;; altered "isa" values.  The other tables don't matter because they get
;;; directly updated.
;;;
define clear_infs( key ); lvars key;
    lvars c;
    for c in supers_of_class( key ) do
        lvars children = infs_of_class( c );
        false -> children( key );                ;;; disinherit key
    endfor;
enddefine;

;;; This is used when a class changes its superclasses: currently only
;;; for extantclasses and in the process of class adoption. Changing
;;; superclasses means that the class_precedence_list will need
;;; recomputing, but so will those for all inferiors
;;;
define clear_class_precedence_list(key);
    lvars c;
    for c in [% all_infs([^key]) %] do
        false -> class_precedence_list(c);
    endfor;
enddefine;

endsection;     /* $-objectclass */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 14 1995
        Replaced the DHHM94 algorithm with the improved Dylan version and
        factored out code common to it and CLOS
 */
