/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/ved/src/vdsetkey.p
 > Purpose:
 > Author:          John Gibson & Aaron Sloman (see revisions)
 */

;;;-------------------- VEDSETKEY ------------------------------------------

#_INCLUDE 'vddeclare.ph'

constant
        procedure (vedinsertstring, Sys$-Ved$-Do_char_action)
    ;

vars
        vednormaltable, ved_last_char
    ;


    ;;; string is a string. new_entry is a procedure, string, ident
    ;;; or word (including "undef"). Change VED's character tables to map the
    ;;; character sequence string onto new_entry.

define vedsetkey(string, new_entry);
    lvars string, new_entry, slen;

    define lconstant Table_recurse(index, table) -> table;
        lvars table, old, new, pair, temp, index, char;

        define lconstant Get_vec(char, vec, def) -> vec;
            lvars vec, def, char;
            if char fi_> 127 and datalength(vec) == 127 then
                if vec == vednormaltable then vedinsertvedchar -> def endif;
                consvector(explode(vec), repeat 128 times def endrepeat, 255)
                                            -> vec
            endif
        enddefine;

        if isword(table) and table /== "undef" and isdefined(table) then
            valof(table) -> temp;
            Table_recurse(index, temp)
                        ->  if islist(temp) or isvector(temp) then
                                valof(table)
                            else
                                table
                            endif
        elseif isident(table) then
            idval(table) -> temp;
            Table_recurse(index, temp)
                        ->  if islist(temp) or isvector(temp) then
                                idval(table)
                            else
                                table
                            endif
        elseif (index fi_+ 1 ->> index) fi_> slen then
            new_entry -> table
        elseif (fast_subscrs(index,string) -> char; isvector(table)) then
            Get_vec(char, table, "undef") -> table;
            table(char) -> old;
            Table_recurse(index, old) -> new;
            unless new == old then
                unless isinheap(table) then copy(table) -> table endunless;
                new -> table(char)
            endunless
        elseif ispair(table) then
            lmember(char, table) -> old;
            if old then
                front(back(old) ->> pair) -> old;
                Table_recurse(index, old) -> new;
                unless new == old then
                    unless isinheap(table) then
                        copylist(table) -> table;
                        back(lmember(char, table)) -> pair;
                    endunless;
                    new -> front(pair)
                endunless
            elseif listlength(table) > 30 then
                ;;; too long  -  replace it with a vector
                ;;; find if there's a default value at the end
                for old on table do
                    if isinteger(front(old)) then
                        back(old) -> old
                    else
                        quitloop
                    endif
                endfor;
                if old == [] then "undef" else front(old) endif -> old;
                consvector(repeat 127 times old endrepeat, 127) -> temp;
                while table /== [] and isinteger(front(table)) do
                    Get_vec(front(table), temp, old) -> temp;
                    front(back(table)) -> temp(front(table));
                    back(back(table)) -> table
                endwhile;
                temp -> table;
                Get_vec(char, table, old) -> table;
                Table_recurse(index, table(char)) -> table(char)
            else
                char :: (Table_recurse(index, false) :: table) -> table
            endif
        elseif index == 1 then
            mishap(0, 'vedsetkey: NO CHARACTER TABLE IN vednormaltable')
        else
            procedure() with_nargs 2;
                ;;; put the current character back on the input stream
                ved_last_char :: ved_char_in_stream -> ved_char_in_stream;
                ;;; 2nd arg is the previous character - make it current
                -> ved_last_char;
                ;;; 1st arg is action for sequence ending with previous char
                ;;; -- do it
                chain(Sys$-Ved$-Do_char_action)
            endprocedure(% table, string(index fi_- 1) %) -> table;
            [% char, Table_recurse(index, table), table %] -> table
        endif;
    enddefine;      /* Table_recurse */

    unless isstring(string) and (datalength(string) ->> slen) /== 0 then
        mishap(string, 1, 'vedsetkey: BAD FIRST ARGUMENT')
    endunless;
    unless isstring(new_entry) or isprocedure(new_entry)
    or isword(new_entry) or isident(new_entry) then
        mishap(new_entry, 1, 'vedsetkey: BAD SECOND ARGUMENT')
    endunless;
    if isstring(new_entry) and issubstring(string, 1, new_entry) then
        mishap(new_entry, 1, 'vedsetkey: FIRST ARGUMENT SUBSTRING OF SECOND')
    endif;

    Table_recurse(0, ident vednormaltable) ->
enddefine;      /* vedsetkey */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 13 1993
        Changed Get_vec to set vednormaltable action for 16:80 - 16:FF to
        be vedinsertvedchar
--- John Gibson, Dec 16 1992
        Allowed to take idents for table or procedures
--- John Gibson, Aug  2 1991
        Added test for isdefined(table) before using valof on it.
--- John Gibson, Aug 16 1987
        Tidied up
 */
