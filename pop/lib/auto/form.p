/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/auto/form.p
 >  Purpose:        define new syntactic forms
 >  Author:         Allan Ramsay, Nov  1 1983 (see revisions)
 >  Documentation:  HELP * FORMS
 >  Related Files:  LIB * EXPRREAD, LIB * IMPREAD, LIB * IMPSEQREAD * VARREAD
 */
compile_mode :pop11 +strict;

#_INCLUDE '$usepop/pop/lib/include/ved_declare.ph'

section;

;;; Add to ved_tidy incremental lists
    [^^(weakref vedopeners) form] -> weakref vedopeners;
    [^^(weakref vedclosers) endform] -> weakref vedclosers;

global vars syntax endform = pop_undef;

define lconstant failsafe(fn);
    lvars fn;

    define dlocal prmishap(n, string);
        lvars n, string;
        if isstring(string) then erasenum(n) endif;
        exitfrom(false, failsafe);
    enddefine;

    fn();
enddefine;

define lconstant lookfor(x,alist) -> y;
    lvars x, alist, y;
    until alist == nil do
        if  (dest(dest(alist)) -> alist -> y) == x then return endif;
    enduntil;
    false -> y;
enddefine;

define lconstant embedlistread();
    [% listread() %]
enddefine;

define lconstant next_var = gensym(%"_form_var"%) enddefine;

define lconstant plant_new_var(x);
    lvars x;
    x, next_var();
enddefine;

define lconstant substitute(fillers, list);
    lvars fillers, list, x, y;
    [%for x in list
         do
             if      lookfor(x,fillers) ->> y
             then    if islist(y) then dl(y) else y endif
             else    x
             endif
         endfor%]
enddefine;

define lconstant expand_form(fillers,form_tree) -> code;
    lvars fillers, form_tree, code, x, entry, filler, filler_name, svproglist;
    if      form_tree == nil
    then    false -> code
    else    hd(form_tree) -> entry;
        if      isword(entry)
        then    proglist -> svproglist;
            itemread() -> x;
            if      x == entry
            and
                expand_form(fillers, hd(tl(form_tree))) ->> code
            then
            else    svproglist -> proglist;
                expand_form(fillers, tl(tl(form_tree))) -> code
            endif;
        elseif  isvector(entry)
        then    subscrv(1,entry) -> filler_name; subscrv(2,entry) -> filler;
            proglist -> svproglist;
            if      (failsafe(filler) ->> x)
            and
                (expand_form(filler_name :: (x :: fillers),
                        hd(tl(form_tree)))
                        ->> code)
            then
            else    svproglist -> proglist;
                expand_form(fillers, tl(tl(form_tree))) -> code
            endif
        else    fillers <> maplist(hd(entry),plant_new_var) -> fillers;
            substitute(fillers,hd(tl(entry))) -> code
        endif
    endif
enddefine;

define lconstant plant_form(form_arg,old_fn);
    lvars form_arg, old_fn, code, svproglist;
    if      expand_form(nil,form_arg) ->> code
    then    code <> proglist -> proglist
    else    proglist -> svproglist; old_fn()
    endif
enddefine;

define lconstant 1 read_field;
    lvars name, type, x;
    unless  isword(itemread() ->> type)
    then    mishap('Illegal item for field type in form declaration',[%type%])
    endunless;
    returnif(type == ";") (false);
    unless  (itemread() ->> x) == ":"
    then    x :: proglist -> proglist;
            /* Don't redeclare existing syntax words */
            unless  is_syntax_word(type)
            then    sysunprotect(type);
                    sysSYNTAX(type,"syntax",false);
                    sysprotect(type);
            endunless;
            return(type);
    endunless;
    if      type == "list"
    then    embedlistread -> type
    else    consword(#| explode(type), explode('read') |#) -> type;
            unless  isprocedure(valof(type)->>type)
            then    mishap('Undefined reader in form declaration', [%type%])
            endunless;
    endif;
    unless  isword(itemread() ->> name)
    then    mishap('Illegal item for field name in form declaration',[%name%])
    endunless;
    {% name, type %}
enddefine;

define lconstant 1 read_form_body;
    lvars x, y, f_vars ;
    nil -> f_vars;
    if      (itemread()->>x) == "formvars"
    then    [% until   (itemread()->>x) == ";" then x enduntil %] -> f_vars;
    else    x :: proglist -> proglist
    endif;
    [%  f_vars,
         [%  ";",
              unless  f_vars == nil
              then    "vars", applist(f_vars,identfn), ";"
              endunless,
              until  (itemread()->>x) == "endform"
              do
                  if      x == "formvars"
                  then    mishap('formvars read when not expected', nil)
                  elseif  x == termin
                  then    mishap('termin read when not expected', nil)
                  else    x
                  endif;
              enduntil%] %]
enddefine;

define lconstant merge_trees(pattern,tree);
    lvars pattern, tree;
    if      pattern == nil
    then    tree
    elseif  tree == nil
    then    [% hd(pattern), merge_trees(tl(pattern), tree) %]
    elseif  hd(pattern) = hd(tree)
    then    hd(tree) :: (merge_trees(tl(pattern), hd(tl(tree))) :: tl(tl(tree)))
    elseif  islist(hd(pattern)) and islist(hd(tree))
    then    hd(pattern) -> hd(tree); tree
    else    hd(tree) :: (hd(tl(tree)) :: merge_trees(pattern, tl(tl(tree))))
    endif;
enddefine;

define global vars syntax form;
    lvars name, pattern, form_tree, fn, x;
    sysunprotect(readitem() ->> name);
    if is_syntax_word(name) then
        if      isclosure(valof(name)->>fn)
        and
            pdprops(pdpart(fn)) == "plant_form"
        then    frozval(1,fn) -> form_tree;
        else    nil -> form_tree;
            plant_form(% nil, fn %) -> valof(name);
        endif;
    else    sysSYNTAX(name,"syntax",false);
        nil -> form_tree;
        plant_form(% nil, sysprmishap(% 'Unmatched form', [% name %] %) %)
            -> valof(name);
    endif;
    [% while read_field() ->> x do x endwhile, read_form_body %] -> pattern;
    merge_trees(pattern,form_tree) -> frozval(1,valof(name));
    sysprotect(name);
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Oct 17 1992
        Cleaned up
--- John Williams, Aug  7 1992
        Made -form- and -endform- global
--- Rob Duncan, Mar 17 1988 - changed tests on -identprops- to use
    -issubstring_lim- to catch redefinitions of infix syntax words.
    See bug report terryd@unx1.uucp.1
--- Mark Rubinstein, Oct 28 1985 - POP-2isms removed.
--- Allan Ramsay, Apr  6 1984 - Altered to check that significant syntax
    words such as "and" don't get redefined
 */
