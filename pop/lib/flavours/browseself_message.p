/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/flavours/browseself_message.p
 > Purpose:        a browser for the flavours package
 > Author:         Mark Rubinstein, Apr 18 1986 (see revisions)
 > Documentation:  HELP * BROWSESELF, TEACH * FLAVOURS
 > Related Files:  LIB * FLAVOURS
 */

;;; -- DISCLAIMER ----------------------------------------------------------
;;; THIS FILE IS EXPERIMENTAL AND IS NOT SUPPORED.  USE IT AT YOUR OWN RISK.

uses seetree;

;;; --- UTILITY ------------------------------------------------------------
define vedwarning;
    vedputmessage();
    vedscreenbell();
enddefine;

;;; --- SEETREE MANIPULATORS -----------------------------------------------

;;; get the subtree for the current node
define lconstant node_subtree(cunode);
lvars cunode;
    fast_appproperty($-showtree_subtree_rootnode_name(),
        procedure(t, n);
            if n == cunode then t; exitfrom(node_subtree) endif;
        endprocedure);
    false;
enddefine;

;;; get the subtree appropriate for the current cursor location
define lconstant current_subtree() -> subtree -> index;
lvars subtree = node_subtree(seetree_root), i mlist index = false;
    unless islist(subtree) do
        seetree_snum + 1 -> index;
        seetree_mlist -> mlist;
        hd(mlist) -> mlist; dl(mlist) -> i -> mlist;
        node_subtree(mlist(i)) -> subtree;
    endunless;
enddefine;

;;; get the current object the cursor is sitting on and the subtree and
;;; appropriate index into it (so that the tree can be destructivel altered)
define lconstant current_object_ref -> subtree -> index -> object;
lvars subtree index object;
    current_subtree() -> subtree -> index;
    unless index do
        if isinstance(hd(subtree), false) then 1 else
            if listlength(subtree) == 2 then 2 else 1 endif
        endif -> index;
    endunless;
    subtree(index) -> object;
    unless isinstance(object, false) do
        if ispair(object) and isinstance(hd(object), false) then
            object -> subtree; 1 -> index;
            hd(object) -> object;
        elseif listlength(subtree) == 2 then
            false -> object;
            vedwarning('the value for ' >< hd(subtree) >< ' is not an object');
        elseif isword(object) then
            vedwarning('which ' >< object >< ' do you mean?');
            false -> object;
        else
            false -> object;
            vedwarning('not appropriate!!!');
        endif
    endunless;
enddefine;

define lconstant current_object();
    current_object_ref() -> ->;
enddefine;

;;; find the current INSTANCE_VARIABLE for the current cursor location
;;; and the object which it relates to and includes the flavour in the list
;;; for the ivars in the donotbrowse property.
define lconstant donotbrowsethisivar();
lvars name = node_subtree(seetree_root), mlist index = false, f;
    unless ispair(name) do
        vedwarning('you are not an appropriate node'); return
    endunless;
    unless isword(hd(name) ->> name) do
        if isinstance(name, metaflavour_flavour) then
            'you cannot remove browsable instance variables from a flavour'
        else
            name >< ' is not an instance variable'
        endif.vedwarning;
        return;
    endunless;
    seetree_snum + 1 -> index;
    seetree_mlist -> mlist;
    hd(mlist) -> mlist; dl(mlist) -> f -> mlist;
    hd(node_subtree(mlist(f))) -> f;
    unless isinstance(f, vanilla_flavour) do
        if isinstance(f, metaflavour_flavour) then
            'you cannot remove browsable instance variables from a flavour'
        else
            'you are not an appropriate node'
        endif.vedwarning;
        return;
    endunless;
    f <- donotbrowse(name);
enddefine;

define lconstant browsemyflavourrecord;
lvars object;
    unless current_object() ->> object do return endunless;
    object <- browsemyflavour;
enddefine;

define lconstant focus_on_this_object;
lvars object tf;
dlocal cucharout;
    unless current_object() ->> object do return endunless;
    if isinstance(object, metaflavour_flavour) then
        systmpfile(false, ivalof(object, "name") >< '_flavour', '.p') -> tf;
        discout(tf) -> cucharout;
        object <- dodisplayself;
        cucharout(termin);
        vededitor(vedhelpdefaults, tf);
        vedswapfiles();
        vedwarning('THE FLAVOUR IS IN THE BUFFER LIST');
    else
        chainfrom(object <- browsetree, seetree, seetree);
    endif
enddefine;

focus_on_this_object -> seetree_command(veddocr);
browsemyflavourrecord -> seetree_command(vedredocommand);
donotbrowsethisivar -> seetree_command(vedchardelete);

;;; --- FLAVOUR STUFF ------------------------------------------------------

lconstant hidden_ivars = newproperty([], 64, [], true);

flavour vanilla novanilla;
    defmethod browsetree;
    ;;; return a tree for the browser
    lvars v f variables;
        sort(myflavour<-instance_variables) -> variables;
        [%  self;
            for v in variables do
                for f in hidden_ivars(v) do
                    if isinstance(self, f) then nextloop(2) endif;
                endfor;
                [%  v;
                   if ispair(ivalof(self, v) ->> v) then dl(v) else v endif
                %]
            endfor; %]
    enddefmethod;
    defmethod donotbrowse(v);
    ;;; if you get the message not to browse a particular instance pass it
    ;;; on to the flavour
    lvars v;
        unless lmember(myflavour, hidden_ivars(v)) do
            myflavour :: hidden_ivars(v) -> hidden_ivars(v);
            vedputmessage(v >< ' will no longer be shown for instances of ' ><
                myflavour<-name);
        endunless;
    enddefmethod;
    defmethod browseself;
    lvars obj root_tree;
    dlocal showtree_name = 'instance_browser.t';
        procedure;
        lvars subtree index object;
            current_object_ref() -> subtree -> index -> object;
            unless object do return endunless;
            if isinstance(object, metaflavour_flavour) then
                vedwarning('you cannot splice in when browsing a flavour')
            elseif index == 1 then
                vedwarning('you have already expanded ' >< object);
            else
                object <- browsetree -> subtree(index);
                chainfrom(root_tree, seetree, seetree);
            endif;
        endprocedure -> seetree_command(vedenter);

        self<-browsetree -> root_tree;
        seetree(root_tree);
    enddefmethod;
    defmethod browsemyflavour;
        myflavour <- browseself;
    enddefmethod;
endflavour;

flavour metaflavour a metaflavour;
    defmethod browsetree;   /* special kind of browsetree */
    lvars c components;
        self <- components -> components;
        [% self; for c in components do c<-browsetree endfor; %]
    enddefmethod;
    defmethod browseself;
    lvars fl;
    dlocal showtree_name;
        if isinstance(self, flavour_flavour) then
            'flavour_browser.t' -> showtree_name
        else
            'metaflavour_browser.t' -> showtree_name;
        endif;
        seetree(self<-browsetree);
    enddefmethod;
    defmethod browsemyflavour;
        myflavour <- browseself;
    enddefmethod;
    defmethod dodisplayself;
    lvars list metaflavour each pdr before after printed_comment;
    lconstant displaydaemon = printf(%'\t\tdefmethod %p %p;\n\t\tenddefmethod;\n'%);

        ;;; DISPLAYMETHOD displays a method in a formatted fashion.
        define lconstant displaymethod(methname, pdr, updflag);
        lvars methname updflag pdr i pdcount = pdnargs(pdr);
            unless isclosure(pdr) and isclosure(pdpart(pdr))
            and pdpart(pdpart(pdr)) == mishap do
                printf(methname, if updflag then '\tdefmethod updaterof %p'
                                else '\tdefmethod %p' endif);
                if pdcount == 0 then
                    pr(';\n')
                else
                    pr('(');
                    for i from 1 to pdcount - 1 do printf(i, 'Arg%p, '); endfor;
                    printf(pdcount, 'Arg%p);\n')
                endif;
                pr('\tenddefmethod;\n');
            endunless;
            if not(updflag) and updater(pdr) then
                displaymethod(methname, updater(pdr), true)
            endif;
        enddefine;

        printf(name, 'flavour %p');
        myflavour<-name -> metaflavour;
        unless metaflavour == "flavour" do
            printf(' a %p', [^metaflavour])
        endunless;
        maplist(self<-components, ivalof(% "name" %)) -> list;
        if lmember("vanilla", list) then
            delete("vanilla", list, 1) -> list;
        else
            pr(' novanilla');
        endif;
        unless list == [] do
            pr(' isa');
            until list == [] do printf(dest(list) -> list, ' %p') enduntil;
        endunless;
        pr(';\n');
        unless (self<-lexical_instance_variables ->> list) == [] do
            pr('ivars'); applist(list, printf(% ' %p' %)); pr(';\n');
        endunless;
        unless (self<-dynamic_instance_variables ->> list) == [] do
            pr('divars'); applist(list, printf(% ' %p' %)); pr(';\n');
        endunless;
        sort(self<-beforedaemons) -> before;
        sort(self<-afterdaemons)  -> after;
        sort(self<-methods) -> list;
        false -> printed_comment;
        for each in before do
            unless lmember(each, list) do
                unless printed_comment do
                    pr('\t/* --- BEFORE DAEMONS --- */\n');
                    true -> printed_comment;
                endunless;
                displaydaemon(each, "before");
                delete(each, before, 1) -> before;
                if lmember(each, after) then
                    displaydaemon(each, "after");
                    delete(each, after, 1) -> after;
                endif;
            endunless;
        endfor;
        false -> printed_comment;
        for each in after do
            unless lmember(each, list) do
                unless printed_comment do
                    pr('\t/* --- AFTER DAEMONS --- */\n');
                    true -> printed_comment;
                endunless;
                displaydaemon(each, "after");
                delete(each, after, 1) -> after;
            endunless;
        endfor;
        false -> printed_comment;
        for each in list do
            unless printed_comment do
                pr('\t/* --- PRIMARY METHODS --- */');
                true -> printed_comment;
            endunless;
            pr(newline);
            self <-methodfor(each) -> pdr;
            if lmember(each, before) then
                displaydaemon(each, "before");
                delete(each, before, 1) -> before;
            endif;
            displaymethod(each, pdr, false);
            if lmember(each, after) then
                displaydaemon(each, "after");
                delete(each, after, 1) -> after;
            endif;
        endfor;
        pr('endflavour;\n');
    enddefmethod;
    defmethod displayself;
    lvars tf;
    dlocal cucharout;
        if vedediting then
            self <- dodisplayself;
        else
            systmpfile(false, name >< '_flavour', '.p') -> tf;
            discout(tf) -> cucharout;
            self <- dodisplayself;
            cucharout(termin);
            chain(vedhelpdefaults, tf, vededitor);
        endif;
    enddefmethod;
endflavour;

flavour flavour a metaflavour;
    defmethod donotbrowse(i);
    lvars i each;
        if islist(i) then
            for each in i do self <- donotbrowse(each) endfor; return;
        endif;
        unless lmember(self, hidden_ivars(i)) do
            self :: hidden_ivars(i) -> hidden_ivars(i);
            ;;; inform
            vedputmessage(i >< ' will no longer be browsed for ' >< name);
        endunless;
    enddefmethod;
    defmethod dobrowse(i);
    lvars i each;
        if islist(i) then
            for each in i do self <- dobrowse(each) endfor; return;
        endif;
        if lmember(self, hidden_ivars(i)) then
            delete(self, hidden_ivars(i)) -> hidden_ivars(i);
            ;;; inform
            vedputmessage(i >< ' will be shown when browsing ' >< name);
        endif;
    enddefmethod;
endflavour;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 23 1992
        Replaced $-seetree vars with renamed seetree_ vars
 */
