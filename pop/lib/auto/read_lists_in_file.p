/* --- Copyright University of Birmingham 2004. All rights reserved. ------
 > File:            $poplocal/local/auto/read_lists_in_file.p
 > Purpose:         Read a collection of lists from a file
 > Author:          Aaron Sloman, Oct 23 2004
 > Documentation:   Below, in and HELP READ_LISTS_IN_FILE
 > Related Files:
 */

/*

LIB read_lists_in_file

This is an autoloadable library that provides a procedure for reading
a list of text items from a file containing things like

    [ the cat sat ]
    [ on every mat]
    [ 1 2 3 4 ]
    [ 'fifty five' mice]
    [ ]

and returning a list of lists. E.g. if the file mydata.p contains
the above text, then the library can be used thus:

    ;;; make pop11 print string quotes
    true -> pop_pr_quotes;
    ;;; read the list of lists and print it out:
    read_lists_in_file('mydata.p') ==>
    ** [[the cat sat] [on every mat] [1 2 3 4] ['fifty five' mice] []]

How it works.

The procedure read_lists_in_file uses discin to create a character repeater
for the file.

It uses incharitem to create an item repeater from the character
repeater.

It uses pdtolist to create a dynamic list from the item repeater.

It uses the following procedure defined below
    make_all_lists(items) -> lists;

to create a list of lists from a list of text items containing pairs
of matched square brackets.

Note that make_all_lists is defined so as NOT to cope with nested
lists.

This limitation can easily be removed by making it recursive, but
that will make it harder to check for errors in programs that create
files representing lists of non-nested lists but sometimes fail
to write the closing list bracket "]" for one or more lists.
This version is for novice-users doing simple things.


Procedures provided:
    define make_all_lists(items) -> lists;
        ;;; given a list of items including occurrences of
        ;;; matched pairs "[" ... "]" make a list of lists.


    define read_lists_in_file(filename) -> lists;
        ;;; given the name of a text file in approprpiate format
        ;;; return a list of lists read from the file.
        ;;; the lists must not be nested. They can contain words,
        ;;; strings or numbers.


I don't know if discin works on windows poplog. If not this will
not work on Windows.

Then make a suitable file containing list expressions and test it
    read_lists_in_file('mydata.p') ==>


*/


/*
First a procedure

    make_all_lists(items) -> lists

to chop a list of text items into a list of lists.

Test it

    make_all_lists(
        [%
            "[", "the", "cat", "]",
            "[", "ate", "all", "]",
            "[", 99, "mice", "]",
        %]) ==>
    ** [[the cat] [ate all] [99 mice]]

;;; check some of its error handling
    make_all_lists(
        [%
            "[", "the", "cat", "]",
            "ate", "all", "]",
            "[", 99, "mice", "]",
        %]) ==>

;;; MISHAP - "[" not in list when expected
;;; INVOLVING:  ate

    make_all_lists(
        [%
            "[", "the", "cat", "[", "]",
            "ate", "all", "]",
            "[", 99, "mice", "]",
        %]) ==>
;;; MISHAP - New list starts before old one finished
;;; INVOLVING:  [ ] ate all ] [ 99 mice ]

    make_all_lists(
        [%
            "[", "the", "cat",  "]",
            "[", "ate", "all", "]",
            "[", 99, "mice"
        %]) ==>

;;; MISHAP - Closing "]" not found
;;; DOING    : make_all_lists ....

*/

define make_all_lists(items) -> lists;
    ;;; given a list of items including occurrences of
    ;;; matched pairs "[" ... "]" make a list of lists.

    [%
        repeat
            if null(items) then
                ;;; finished
                quitloop()

            elseif hd(items) == "[" then
                ;;; make next list
                tl(items) -> items;
                [%
                    repeat
                        if null(items) then
                            mishap('Closing "]" not found', [])
                        elseif hd(items) == "[" then
                            mishap('New list starts before old one finished', [^^items])
                        elseif hd(items) == "]" then
                            ;;; finish this list
                            tl(items) -> items;
                            quitloop()
                        else
                            ;;; next list element left on stack
                            hd(items);
                            tl(items) -> items;
                        endif;
                    endrepeat;
                %]
            else
                ;;; no opening list bracket, so complain
                mishap('"[" not in list when expected', [%hd(items)%])
            endif;
        endrepeat
    %] -> lists;
enddefine;


/*

Now use that to define a procedure to read a list of lists from a file

*/

define read_lists_in_file(filename) -> lists;
    ;;; given the name of a text file in approprpiate format
    ;;; return a list of lists read from the file.
    ;;; the lists must not be nested. They can contain words,
        ;;; strings or numbers.

    lvars file_items;

    ;;; create a dynamic list of text items in the file
    pdtolist(incharitem(discin(filename))) -> file_items;

    ;;; create all the lists from the text items, and
        ;;; make a list of all of them

    make_all_lists(file_items) -> lists;

enddefine;
