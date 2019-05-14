/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.all/lib/lib/showtree.p
 > Purpose:        Visual demonstration of lists in Ved
 > Author:         Jonathan Cunningham, June 1982 (see revisions)
 > Documentation:  HELP * SHOWTREE
 */
compile_mode :pop11 +strict;

/* ------------------------------------------------------------------------ */
/*      SOME INITIALISATIONS                                                */
/* ------------------------------------------------------------------------ */

uses-now newvedfileprop;
uses drawline;

section $-showtree =>
            showtree_node_location, showtree_node_daughters,
            showtree_node_draw_data, showtree_subtree_rootnode_name,
            showtree_name, showtree_vgap, showtree_isleaf, showtree_root,
            showtree_daughters, showtree_defaults, showtree_init,
            showtree_name_&_subtrees, showtree_width, showtree_height,
            showtree_box, showtree_draw_node, showtree_printable,
            showtree_graphical, showtree_quit, showtree, showtree_mid,
            showtree_mess;

/* ------------------------------------------------------------------------ */
/* DECLARATIONS OF THINGS THAT USERS MIGHT WANT TO ACCESS (but not redefine)*/
/* ------------------------------------------------------------------------ */

/* three vedfileprops containing properties mapping node names to: */
vars procedure (
    ;;; [startrow finishrow startcol finishcol]
    showtree_node_location   = newvedfileprop(),

    ;;; list of daughter node identifiers
    showtree_node_daughters  = newvedfileprop(),

    ;;; [?name ?startrow]
    showtree_node_draw_data  = newvedfileprop(),

    ;;; a property to give inverse
    ;;; mapping from tree fragments to node names. N.B. this will only
    ;;; work on sub-trees, not on leafs.
    showtree_subtree_rootnode_name = newvedfileprop(),
    );

/* ------------------------------------------------------------------------ */
/*       SOME USER RE-DEFINABLE PROCEDURES AND VARIABLES (see help file)    */
/* ------------------------------------------------------------------------ */

vars
    showtree_name = 'tree.t',
    showtree_vgap = 1,                   ;;; vertical spacing between nodes
  procedure(
    showtree_isleaf = atom,
    showtree_root = hd,
    showtree_daughters = tl,
    showtree_defaults = vedhelpdefaults,
    showtree_init = identfn,
)
    ;

define lconstant to_string(item);
    lvars item;
    dlocal cucharout = identfn;
    consstring(#| pr(item) |#);
enddefine;

define vars showtree_name_&_subtrees(tree) -> name -> subtrees;
    lvars tree, name, subtrees;
    ;;; tree is not a leaf, but may have no daughters
    showtree_root(tree) -> name;
    if isword(name) or isstring(name) then
        showtree_daughters(tree) -> subtrees;
    elseif showtree_isleaf(name) then
        showtree_daughters(tree) -> subtrees;
        to_string(name) -> name;
    else
        datalist(tree) -> subtrees;
        space -> name
    endif;
enddefine;

define vars showtree_width(node, name);
    lvars node, name;
    unless isword(name)
    or isstring(name) and not(pop_pr_quotes)
    then
        to_string(name) -> name;
    endunless;
    if showtree_node_daughters()(node) then
        length(name) + 2
    else
        length(name)
    endif;
enddefine;

define vars showtree_height(node, name);
    lvars node, name;
    3
enddefine;

define vars showtree_box(r1, c1, r2, c2);
    lvars r1, c1, r2, c2;
    drawline(r1, c1, r2, c1);
    drawline(r1, c1, r1, c2);
    drawline(r1, c2, r2, c2);
    drawline(r2, c1, r2, c2);
enddefine;

define vars showtree_draw_node(node, val);
    lvars node, val, name, r1, c1, r2, c2;
    ;;; val --> [?r1 ?r2 ?c1 ?c2]; but matches won't work with lvars
    dl(val) -> c2 -> c1 -> r2 -> r1;
    to_string(hd(showtree_node_draw_data()(node))) -> name;
    if showtree_node_daughters()(node) then
        showtree_box(c1, r1, c2-2, r2);
        c1 + 1 -> c1
    endif;
    vedjumpto(r1 + 1, c1);
    vedinsertstring(name);
enddefine;

/* ------------------------------------------------------------------------ */
/* PROCEDURES THAT CALCULATE THE LAYOUT OF THE TREE ON THE SCREEN           */
/* ------------------------------------------------------------------------ */

vars procedure shapetree;

define lconstant nodename = gensym(%"node"%) enddefine;

define vars transformtree(tree, depth) -> topnode;
    lvars tree, depth, topnode, name, subtrees;
    nodename() -> topnode;
    if showtree_isleaf(tree) then
        [^tree ^depth] -> showtree_node_draw_data()(topnode);
    else
        topnode -> showtree_subtree_rootnode_name()(tree);     ;;; in case other progs want to modify the tree
        showtree_name_&_subtrees(tree) -> name -> subtrees;
        [^name ^depth] -> showtree_node_draw_data()(topnode);
        maplist(subtrees,
            transformtree(% depth + showtree_vgap + showtree_height(topnode,name) %))
            -> subtrees;
        subtrees -> showtree_node_daughters()(topnode);
    endif
enddefine;

define lconstant rhsofleftneighbour(r1, r2) -> rhs;
    lvars r1, r2, rhs;
    1 -> rhs;
    appproperty(showtree_node_location(),
        procedure(x, val);
        lvars x val s1 s2 c2;
            ;;; val --> [?s1 ?s2 = ?c2];
            dl(val) -> c2 ->; -> s2 -> s1;
            if r2 >= s1 and r1 <= s2 then
                if c2 >= rhs then c2 + 1 -> rhs endif
            endif
        endprocedure);
enddefine;

define lconstant movesubtree(node, r);
    lvars node, r, subnodes, r1, r2, c1, c2;
    if (showtree_node_daughters()(node) ->> subnodes)  then
        applist(subnodes, movesubtree(% r %))
    endif;
    ;;; showtree_node_location()(node) --> [?r1 ?r2 ?c1 ?c2];
    dl(showtree_node_location()(node)) -> c2 -> c1 -> r2 -> r1;
    [%r1, r2, c1 + r, c2 + r %] -> showtree_node_location()(node);
enddefine;

define vars showtree_mid(c1, c2);
    lvars c1, c2;
    intof((c1 + c2 - 2) / 2)
enddefine;

define lconstant centreofsubnodes(node, name) -> c1 -> c2;
    lvars node, name, subnodes, r, w, c1, c2;
    showtree_width(node, name) -> w;
    if (showtree_node_daughters()(node) ->> subnodes) and subnodes /== [] then
        applist(subnodes, shapetree);
        ;;; showtree_node_location()(front(subnodes)) --> [= = ?c1 =];
        showtree_node_location()(front(subnodes))(3) -> c1;
        last(subnodes) -> node;
        ;;; showtree_node_location()(node) -->  [= = = ?r];
        showtree_node_location()(node)(4) -> r;
        showtree_mid(r, c1) - intof((w - 1)/2) -> c1
    else
        1 -> c1;
    endif;
    c1 + w + 1 -> c2
enddefine;

define vars shapetree(node);
    lvars node, r1, r2, c1, c2, rhs, name;
    ;;; showtree_node_draw_data()(node) -->  [?name ?r1];
    dl(showtree_node_draw_data()(node)) -> r1 -> name;
    r1 + showtree_height(node, name) - 1 -> r2;
    centreofsubnodes(node, name) -> c1 -> c2;
    rhsofleftneighbour(r1 - round(showtree_vgap/2), r2 + round(showtree_vgap/2)) -> rhs;
    [^r1 ^r2 ^c1 ^c2] -> showtree_node_location()(node);
    if rhs > c1 then movesubtree(node, rhs-c1) endif;
enddefine;


/* ------------------------------------------------------------------------ */
/* PROCEDURES TO DRAW THE TREE ACCORDING TO THE CALCULATED LAYOUT           */
/* ------------------------------------------------------------------------ */

define lconstant straightjoin(node, unode);
    lvars node, unode, r1, r2, c1, c2;
    ;;; showtree_node_location()(node) --> [= ?r2 ?c1 ?c2];
    dl(tl(showtree_node_location()(node))) -> c2 -> c1 -> r2;
    showtree_mid(c1, c2) -> c1;
    ;;; showtree_node_location()(unode) --> [?r1 = = =];
    hd(showtree_node_location()(unode)) -> r1;
    drawline(c1, r2, c1, r1)
enddefine;

define lconstant sidejoin(node, unode, left);
    lvars node, unode, left, r1, r2, c1, c2, ct, rmid;
    ;;; showtree_node_location()(node) --> [= ?r2 ?c1 ?c2];
    dl(tl(showtree_node_location()(node))) -> c2 -> c1 -> r2;
    showtree_mid(c1,c2) -> ct;
    ;;; showtree_node_location()(unode) --> [?r1 = ?c1 ?c2];
    dl(showtree_node_location()(unode)) -> c2 -> c1 ->; -> r1;
    showtree_mid(c1, c2) -> c1;
    round((r1 + r2) / 2) -> rmid;
    if left then drawline(ct, r2, ct, rmid) endif;
    drawline(ct, rmid, c1, rmid);
    drawline(c1, rmid, c1, r1);
enddefine;

define lconstant leftjoin = sidejoin(%true%) enddefine;
define lconstant rightjoin = sidejoin(%false%) enddefine;

define lconstant underjoin(node, subnodes);
    lvars node, unode, subnodes, r1, c1, c2, r2, rmid;
    ;;; showtree_node_location()(node) --> [= ?r2 ==];
    showtree_node_location()(node)(2) -> r2;
    for unode in subnodes do
        ;;; showtree_node_location()(unode) --> [?r1 = ?c1 ?c2];
        dl(showtree_node_location()(unode)) -> c2 -> c1 ->; -> r1;
        round((r1 + r2) / 2) -> rmid;
        showtree_mid(c1, c2) -> c1;
        drawline(c1, r1, c1, rmid)
    endfor
enddefine;

define vars connectup();
    appproperty(showtree_node_daughters(),
        procedure(node, val);
            lvars val, node, unode, subnodes;
            ;;; if val matches [?unode ??subnodes] then
            if listlength(val) > 0 then
                dest(val) -> subnodes -> unode;
                if subnodes == [] then
                    straightjoin(node, unode)
                else
                    leftjoin(node, unode);
                    ;;; subnodes --> [??subnodes ?unode];
                    last(subnodes) -> unode;
                    allbutlast(1, subnodes) -> subnodes;
                    rightjoin(node, unode);
                    underjoin(node, subnodes)
                endif
            endif
        endprocedure);
enddefine;

/* ------------------------------------------------------------------------ */
/* MISCELLANEOUS UTILITY PROCEDURES AND MAIN PROCEDURES                      */
/* ------------------------------------------------------------------------ */

define vars showtree_printable();
    5 -> showtree_vgap
enddefine;

define vars showtree_graphical();
    4 -> showtree_vgap
enddefine;

define vars showtree_quit();
    ;;; not clear what (if anything) this should do
    ;;; leave it doing nothing for now -- JG
enddefine;

lvars showmess = false;

define vars showtree(tree);
    lvars   n, used, tree, filename, oldwindow = vedstartwindow,
            showms = showmess;
    dlocal  vedstartwindow = vedscreenlength;

    ;;; records tree number of file
    lconstant file_tree_number = newproperty([], 8, false, "tmparg");

    define lconstant numbers_used();
        lvars file, p;
        [%  for file in vedbufferlist do
                if ispair(file_tree_number(file) ->> p)
                and front(p) = showtree_name then
                    back(p)
                endif
            endfor
        %]
    enddefine;

    define lconstant draw_tree();
        lvars wasediting = vedediting;
        dlocal vedediting, vedautowrite = false, vedstatic;
        conspair(showtree_name, n) -> file_tree_number(ved_current_file);
        vedputmessage('Please Wait - Planning the layout');
        showtree_init();
        newproperty([], 59, false, true) -> showtree_node_location();
        newproperty([], 59, false, true) -> showtree_node_daughters();
        newproperty([], 59, false, true) -> showtree_node_draw_data();
        newproperty([], 59, false, true) -> showtree_subtree_rootnode_name();
        oldwindow -> vedstartwindow;
        true -> vedstatic;
        false ->> vedchanged -> vedediting;
        shapetree(transformtree(tree,1));
        procedure();
            dlocal vedediting = true;
            vedputmessage('Please Wait - Drawing the graph')
        endprocedure();
        appproperty(showtree_node_location(), showtree_draw_node);
        connectup();
        ;;; showtree_node_location()("node1") --> [= = ?col =];
        vedjumpto(1, showtree_node_location()("node1")(3));

        if showms then
            vedlineabove();
            vedcheck();
            vedinsertstring('USE ARROW KEYS and \'KEYPAD\' NUMBER KEYS TO EXPLORE DIAGRAM'); vedlinebelow();
            vedinsertstring('WHEN FINISHED PRESS \'ESC\' BUTTON THEN PRESS Q');
            vedjumpto(1,1);
        endif;

        nullstring -> vedmessage;
        if wasediting then
            true -> vedediting;
            vedrefresh();
        endif;
        1 -> vedchanged;
    enddefine;

    vedsetup();

    1 -> gensym("node");

    numbers_used() -> used;
    for n from 0 to 1000 do quitunless(fast_lmember(n, used)) endfor;
    if n == 0 then
        showtree_name
    else
        (sys_fname(showtree_name,1,4) sys_>< n) <> sys_fname_extn(showtree_name)
    endif -> filename;

    unless vedinvedprocess then vedinput(draw_tree) endunless;
    vededit(filename, showtree_defaults);
    if vedinvedprocess then draw_tree() endif
enddefine;

define vars showtree_mess(tree);
    lvars tree;
    dlocal showmess = true;
    showtree(tree)
enddefine;

endsection;     ;;; $-showtree


/*  --- Revision History ---------------------------------------------------
--- Simon Nichols, Sep  1 1993
        Replaced inconsistent uses of sprintf and >< with new procedure
        to_string. Now works tolerably with pop_pr_quotes set true, and once
        more respects redefinitions of pr (thus Prolog showtree now works
        again).
--- John Gibson, Aug 24 1993
        Now lib showtree instead of new_sh*owtree (old one is
        lib/obsolete/old_showtree.p)
--- John Gibson, Nov 12 1992
        Now lib new_sh*owtree with all exported identifiers prefixed with
        "showtree_". Made all perm ids vars, got rid of global (now
        unnecessary with strict).
--- Adrian Howard, Oct 13 1992
        Fixed bug with the new -pop_pr_quotes- stuff
--- John Williams, Sep 29 1992
        Fixed BR christ.7 by making transformtree variable
--- Adrian Howard, Aug 20 1992
        Made it act sanely when -pop_pr_quotes- is -true-.
--- John Williams, Aug  5 1992
        Fixed BR ianr.33, by making -shapetree- and -connectup- global
        variables instead of lconstants.
--- John Williams, Aug  5 1992
        Fixed BR ianr.32 & BR isl-fr.4460, by adding explicit vars
        declarations for the identifiers documented as variables
        in HELP SHOWTREE
--- John Gibson, Jun 22 1992
        Made sure it draws tree immediately if already in vedprocess
--- John Gibson, Apr  9 1992
        Changed to generate different filenames for each tree, and
        generally cleaned up
--- John Gibson, Feb 13 1992
        Removed all references to old lib g*raphcharsetup
--- John Williams, Jul 28 1989
    Changed 'vedstartwindow = 24' to 'vedstartwindow = vedscreenlength'
    Changed -vars- inside procedures to -dlocal-
--- Aaron Sloman, Oct 27 1988
    Stopped call of vedsetup in background processes
--- John Gibson, Feb 24 1988
        Removed use of /==_nil
--- Mark Rubinstein, Jun  6 1986 - altered sys_>< to ><.
--- Mark Rubinstein, May 15 1986 - sectionised, lvarsed and made some of the
    code more efficient.  Moved into the public library, beefed up the HELP
    file, cleaned up -showtree_printable- and introduced -showtree_graphical-.
--- Mark Rubinstein, Nov 26 1985 - Made several of the identifiers global.
--- Mark Rubinstein, Dec 1984 -fix so that it doesn't mishap
    when first called from pop
--- Jonathan Cunningham, Mar 1984 Re-written and extended.  Main reason was
    to allow arbitray sized rectangular nodes.
--- Roger Evans, Sep 1983 - modified to VEDFILEPROP and user procs
    SHOWTREEDEFAULTS and SHOWTREEINIT added.  user definable procedure:
        showtree_name_&_subtrees(tree) -> name -> subtrees;
    this procedure is given a subtree, and should return the node name
    to be used for that subtree, and the list of sub-trees
    the default assumes that a list in the node name positon is a subtree
--- Aaron Sloman, Nov 1982 modified to accommodate terminals other than Visual
    200
 */
