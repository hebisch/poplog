/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/lib/crossref.p
 >  Purpose:        prototype cross-referencer for POP programs
 >  Author:         Jonathan Cunningham, May 31 1985 (see revisions)
 >  Documentation:  HELP * CROSSREF
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +oldvar;

;;; LIB CROSSREF - now considerably out of date, but still
;;; useful sometimes (I use it!). JL Cunningham.
;;; (last changed May 1985)

;;; procedures that the user can re-define include
;;;     unimportant     -   words which are unimportant can be used nonlocally
;;;                         without being noted.
;;;     ignoreable      -   words which are ignoreable are not included in
;;;                         the crossreferencing at all
;;; various other options that should be under user control, but not yet
;;;     implemented: turning off some forms of output, re-directing output, ...
;;; bugs (features):
;;;     doesn't check for re-declaration of a procedure

constant WORDQUOTE; front(["]) -> WORDQUOTE;

define ignoreable(); not(isassignable()) enddefine;

vars list_depth frozzing nonlocals cref define_stack;
define in_a_list(); list_depth > 0 enddefine;

vars item;
define getitem();
        readitem() -> item
enddefine;

define nexitem() -> item;
    vars proglist;
    getitem()
enddefine;

define in_a_definition(); length(define_stack) > 1 enddefine;

define islocal(scope);
    member(scope,[local input output inputoutput MULTIPLY_DECLARED])
enddefine;

define zero_usage(word,deforvar,scope);
vars oldscope usecount database;
    if word.ignoreable then return endif;
    cref(word) -> database;
    if present([?usecount ?oldscope ^^define_stack]) then
        if oldscope.islocal then
            if scope == "output" and oldscope == "input" then
                "inputoutput" -> scope
            else
                ppr([%';;; WARNING Multiple declaration:\n;;;   '%
                        ^word is ^scope and ^oldscope in ^^define_stack]);
                nl(1);
                "MULTIPLY_DECLARED" -> scope
            endif
        endif;
        remove(it)
    else
        0 -> usecount
    endif;
    add([^usecount ^scope ^^define_stack]);
    if scope == "nonlocal" then
        add([^deforvar ^^define_stack])
    endif;
    database -> cref(word)
enddefine;

vars localise; zero_usage(% "local" %) -> localise;

vars nonlocalise; zero_usage(% "nonlocal" %) -> nonlocalise;

vars inputise; zero_usage(% "declared", "input" %) -> inputise;
vars outputise; zero_usage(% "declared", "output" %) -> outputise;

define declaration(word,deforvar);
    if in_a_definition() then
        localise(word,deforvar)
    else
        nonlocalise(word,deforvar)
    endif
enddefine;

define readvarlist();
vars item;
    [%
         repeat
             getitem();
             if item.isinteger or lmember(item,[macro procedure]) then
                 ;;; ignore it
             elseif item == "(" then
                 repeat
                     getitem();
                 quitif(item == ")");
                     if item == ";" then
                         mishap('Seems to be an extra ; in vars statement',[])
                     endif;
                     item
                 endrepeat
             elseif item == ";" then
                 quitloop
             else
                 item
             endif
         endrepeat
         %]
enddefine;

define readvars();
vars bra;
    false -> bra;
    [% until (getitem(), item == ";") do
             if member(item,[, ( )]) then
                 true -> bra
             else
                 item
             endif
         enduntil %];
    if bra then
            -> bra;
        if front(bra).isinteger then
            back(bra)
        else
            bra
        endif
    endif
enddefine;

define usage(word);
vars usecount scope database;
    if word.ignoreable then return endif;
    cref(word) -> database;
    if present([?usecount ?scope ^^define_stack]) then
        remove(it)
    else
        0 -> usecount;
        "nonlocal" -> scope
    endif;
    add([%usecount+1,scope% ^^define_stack]);
    database -> cref(word)
enddefine;

define listorder(list1,list2) -> o;
    alphabefore(front(list1),front(list2)) -> o;
    if o == 1 then
        if back(list1) == [] then
            unless back(list2) == [] then true -> o endunless
        elseif back(list2) == [] then
            false -> o
        else
            listorder(back(list1),back(list2)) -> o
        endif
    endif
enddefine;

define unimportant(word);
    identprops(word) /== undef and isprocedure(valof(word))
enddefine;

define notenonlocal(word);
    unless lmember(word,nonlocals) or unimportant(word) then
        conspair(word,nonlocals) -> nonlocals
    endunless
enddefine;

define Report(files,outfile);
vars item varname usecount scope define_stack cucharout database undeclared;
vars deforvar;
    pr(';;; Writing crossref.log\n');
    if outfile then discout(outfile) -> cucharout endif;
    unless files == [] then
        pr('CROSS REFERENCE LISTING OF:\n');
        for item in files do
            pr('\n\t\t\t');
            pr(item)
        endfor;
        nl(2)
    endunless;
    for varname in sort([% appproperty(cref,erase) %]) do
        nl(1);
        pr(varname);
        cref(varname) -> database;
        syssort(database,procedure x y; listorder(back(x),back(y)) endprocedure)
            -> database;
        true -> undeclared;
        for item in database do
            if item matches [?deforvar:%member(% [defined declared] %)% ??define_stack] then
                false -> undeclared;
                nl(1); tabs(1);
                ppr([*** ^deforvar in ^^define_stack])
            else
                item --> [?usecount ?scope ??define_stack];
                if usecount == 0 then
                    unless scope == "nonlocal"
                            and present([?usecount:%nonop/==(%0%)%nonlocal ==]) then
                        nl(1); tabs(1);
                        ppr([*** DECLARED ^scope BUT NOT USED]);
                        for item in define_stack do
                            ppr([IN ^item])
                        endfor
                    endunless
                else
                    nl(1); tabs(1);
                    ppr([^usecount uses as ^scope]);
                    if scope == "nonlocal" and undeclared then
                        notenonlocal(varname)
                    endif;
                    for item in define_stack do
                        ppr([in ^item])
                    endfor
                endif
            endif
        endfor;
        nl(1)
    endfor;
    unless nonlocals == [] then
        pr('\n=======================================\n');
        pr('\'IMPORTANT\' VARIABLES USED AS NONLOCALS');
        pr('\n=======================================\n\n');
        syssort(nonlocals,alphabefore) -> nonlocals;
        for varname in nonlocals do
            tabs(1);
            pr(varname);
            nl(1)
        endfor
    endunless;
    cucharout(termin)
enddefine;

define isspecial(word);
    word.isinteger
        or member(word,[macro syntax updaterof])
enddefine;

define stripoutvars(list) -> outputs -> inputs;
vars temp;
    ;;; shouldnt be any commas in list
    [] -> outputs;
    while list matches [??inputs -> ?temp] do
        temp :: outputs -> outputs;
        inputs -> list
    endwhile;
    list -> inputs;
    if inputs matches [??list -> ??temp] then
        temp <> outputs -> outputs;
        list -> inputs
    endif
enddefine;

define modify(list);
vars bra item;
    1 -> bra;
    tl(list) -> list;
    [% "%",
         until bra == 0 do
             dest(list) -> list -> item;
             if item == "(" then
                 1 + bra -> bra
             elseif item == ")" then
                 bra - 1 -> bra
             endif;
             item
         enduntil,
         erase(),
         "%" %]
enddefine;

define Cref(myfile);
vars proglist list_depth getitem heading inputs outputs;
vars define_stack pcent_stack;
    [] ->> pcent_stack -> define_stack;
    0 -> list_depth;
    0 -> frozzing;
    pdtolist(incharitem(discin(myfile))) -> proglist;
    pr(';;; Looking at ' >< myfile >< '\n');
    [^myfile] -> define_stack;
    until (getitem(), item == termin) do
        if in_a_list() then
            if item == "[" or item == "{" then
                list_depth + 1 -> list_depth
            elseif item == "]" or item == "}" then
                list_depth - 1 -> list_depth
            elseif item == "%" then
                frozzing :: (list_depth :: pcent_stack) -> pcent_stack;
                0 -> list_depth;
                0 -> frozzing
            elseif item == "^"
                    or item == "^^"
                    or item == "?"
                    or item == "??"
            then
                if nexitem() == "(" then
                    modify(proglist) -> proglist;
                    nextloop
                endif;
                getitem();
                usage(item);
                if nexitem() == ":" then
                    getitem();
                    unless lmember(nexitem(),[% "^", "^^", "%" %]) then
                        getitem();
                        if item.isword then usage(item) endif
                    endunless
                endif
            endif
        elseif item.isword then
            if item == "vars" then
                applist(readvarlist(),declaration(% "declared" %))
            elseif item == "procedure" then
                "procedure" :: readvars() -> inputs;
                goto defining
            elseif item == "define" then
                ;;; the next line is a temporary fix
                if nextitem() == "constant" then .readitem.erase endif;
                readvars() -> inputs;
defining:       if inputs matches [?item:isinteger ??heading] then
                    ;;; is an operator declaration
                    stripoutvars(heading) -> outputs -> heading;
                    if heading /== [] and length(heading) < 3 then
                        destpair(heading) -> inputs -> item
                    elseif heading matches [?heading ?item ??inputs] then
                        heading :: inputs -> inputs
                    else mishap('Unknown DEFINE syntax',[])
                    endif;
                    declaration(item,"defined");
                    item :: define_stack -> define_stack;
                    applist(inputs,inputise);
                    applist(outputs,outputise)
                elseif inputs matches [?item:isspecial ??heading]
                        or (inputs ->> heading) then
                    stripoutvars(heading) -> outputs -> heading;
                    destpair(heading) -> inputs;
                    if item == "updaterof" then
                        consword(><'-updater')
                    endif;
                    -> item;
                    declaration(item,"defined");
                    item :: define_stack -> define_stack;
                    applist(inputs,inputise);
                    applist(outputs,outputise)
                endif
            elseif item == "enddefine" or item == "endprocedure" then
                if in_a_definition() then
                    back(define_stack) -> define_stack
                else mishap('Too many enddefine',[])
                endif
            elseif item == "[" or item == "{" then
                list_depth + 1 -> list_depth
            elseif item == WORDQUOTE then
                getitem();
                getitem();
            elseif item == "(" then
                if nexitem() == "%" then
                    frozzing + 1 -> frozzing;
                    getitem()
                endif
            elseif item == "%" then
                if frozzing /== 0 and nexitem() == ")" then
                    frozzing - 1 -> frozzing
                elseif pcent_stack == [] then
                    mishap('Unexpected percent symbol',[])
                else
                    destpair(pcent_stack) -> pcent_stack -> frozzing;
                    destpair(pcent_stack) -> pcent_stack -> list_depth
                endif
            elseif item.identprops == "syntax" then
            else
                usage(item)
            endif
        endif
    enduntil
enddefine;

define myread() -> item;
    readitem() -> item;
    if member(item,[^termin ^newline]) then ";" -> item endif
enddefine;

define macro crossref;
    vars name item popnewline files;
    true -> popnewline;
    dl([ newproperty([],50,[],true) -> cref; [] -> nonlocals;]);
    [] -> files;
    myread() -> name;
    until name == ";" do
        myread() -> item;
        if item == "." then
            name >< '.' -> name;
            myread() -> item;
            unless item == ";" then
                name >< item -> name;
                myread() -> item
            endunless
        endif;
        name :: files -> files;
        dl([Cref(^name);]);
        item -> name
    enduntil;
    dl([Report(sort(^files),'crossref.log');])
enddefine;
