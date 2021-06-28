/*
TEACH TEACHNUMS                                 Aaron Sloman 10 Aug 2009

This is a DRAFT teach file. Comments and suggestions welcome.

It may be too difficult for some younger children, though some of the
early ideas could be presented by a teacher using a computer, with
pupils being asked to make suggestions about how things should work.

More advanced learners (especially potential mathematical high fliers)
could work through this file themselves, trying out the suggested
procedure definitions then exploring alternatives, and finally trying to
meet the challenges presented at the end, e.g. extending these ideas to
negative numbers, and other extensions.

Regarding negative numbers, teachers may find it useful to read:

Pamela Liebeck,
    Scores and Forfeits: An Intuitive Model for Integer Arithmetic,
    Educational Studies in Mathematics,
    Vol 21, No 3, June, 1990, pp. 221--239,

[Available via JSTOR to subscribing institutions]

CONTENTS - (Use <ENTER> g to access required sections)

 -- Introduction
 -- A myth about computers
 -- Using bit-patterns in two-element links chained to form lists
 -- Doing arithmetic on lists of x's.
 -- First procedure: a number recognizer
 -- A procedure for adding two nums
 -- A procedure to subtract a number from another
 -- Another version, subnums2 using the pattern matcher
 -- Multiplication as repeated addition
 -- Some predicates for comparing numbers
 -- -- samenum checks whether two numbers are the same
 -- -- bigger checks whether a number is bigger than another
 -- -- between checks whether a number is between two others
 -- A procedure to divide one number into another
 -- A hard problem: how to represent and operate on negative numbers
 -- Another hard problem: how to divide 4 by 7 and produce a fraction
 -- Introducing 'place' notation to compress number representations
 -- Can you use numbers for counting?
 -- -- Try problem (a)
 -- -- Try problem (b)
 -- Introducing numerals
 -- How about using place notation with number names?
 -- More information

-- Introduction

In this teach file we introduce the idea of representing numbers as
lists of 'x's and operations on numbers such as addition, subtraction
and multiplication as operations on lists. The file gets to the point
where new puzzles arise regarding how to deal with negative numbers.

Working out how to do that is left as an exercise (for teachers and
learners). There is no one right answer: learners should explore
different types of solution.

-- A myth about computers ---------------------------------------------

It is often said that computers operate primarily on numbers and that
everything else computers do is built on top of those operations.

This is not correct. Computers operate on 'bit-patterns', i.e. patterns
made of collections of switches that can be on or off. These can be
represented by strings made up of '1's and '0's, e.g.

    000111000111000111
    000010011000111001101111

The fact that we use '0' and '1', is misleading. We could have used 'X'
and 'Y', e.g.

    XXXYYYXXXYYYXXXYYY
    XXXXYXXYYXXXYYYXXYYXYYYY

Any two symbols can be used in bit-patterns.

One of many things that can be done using bit patterns is representing
locations in memory in the computer, and also representing printable
symbols.

For now the details are not important.

-- Using bit-patterns in two-element links chained to form lists ------

It is very often useful to store information in structures that are all
built out of elementary structures containing only two adjacent
locations in memory. These are called 'pairs' in Pop-11.

The first location of a pair may store some bit pattern that represents
a letter for example 'x' or 'y', or a word, e.g. 'the' or 'cat', or
anything else (e.g. a number 3769).

The second location can store a bit pattern that specifies the location
of another pair. When several pairs are 'joined up' in a series to form
a chain, because each pair points to the next pair, then they are
sometimes referred to as 'links', making up a chain, or list.

For example the list of words

    [how now brown cow]

could be represented as chained links, as follows, where each asterisk
'*' represents a bit-pattern that specifies the location of the next
link, and 'NIL' at the end means there is not another link:

     .---.   .---.---.   .---.---.   .-----.---.   .---.---.
     | *---->|how| *---->|now| *---->|brown| *---->|cow|NIL|
     .---.   .---.---.   .---.---.   .-----.---.   .---.---.

The first asterisk represents a bit-pattern that could be stored in
various places, specifying where to find this list.

This diagram suggests that the links are all stored close together, and
in the order shown, but that is not required for the system to work.

There are many more details concerning how lists are represented in
computers, but we shall ignore them as we want to explore ways of using
lists to represent numbers.

Anyone wanting to know more about what lists are, how they are
implemented in Pop-11, and which notations and commands are available to
construct and operate on them, can look at

    TEACH LISTS
    TEACH WAL
    TEACH MATCHES

and many 'teach' files that use lists and the pattern matcher, e.g.

    TEACH RIVER
    TEACH RESPOND

Below we introduce the idea of using a list of 'x's to represent a
number. We then have to define arithmetical operations, like addition,
multiplication, subtraction and division using operations on lists.

This file assumes that you have already had some practice writing simple
Pop-11 programs and know, for example, that if L is a list, then hd(L)
is the first element of L (its 'head') and tl(L) is a list (the 'tail'
of L) containing everything in L except the first element.

So
    hd([how now brown cow]) is the word "how"

    tl([how now brown cow]) is the list [now brown cow]

The operator "<>" can be used to join two lists together, or more
precisely to produce a new list that starts with the elements of the
first list and then has a tail containing the second list.

For example:

    [how now] <> [brown cow] produces [how now brown cow]

-- Doing arithmetic on lists of x's. ----------------------------------

In what follows we present some pop-11 procedures that can operate on
lists of x's in order to perform arithmetic operations. You will find
each procedure presented with some documentation preceding it, saying
what its name is, what inputs it takes, what output it takes, what it is
for and giving some test examples.

The tests can be run after the procedures are compiled. If you change
any of the procedure definitions you should run the tests again.

The procedure headers are produced using a command in the poplog editor
Ved, namely ENTER procheader, whose use is explained in
    HELP * VED_PROCHEADER

the procedures defined below are

    define isnum(item) -> boole;
        check whether item is a number and return true or false

    define addnums(num1, num2) -> sum;

        check whether num1 and num2 are numbers and if so return a
        new number which is their sum, otherwise mishap

    define subnums(num1, num2) -> diff;

        if num1 and num2 are numbers return a number that is
        their difference

    define subnums2(num1, num2) -> diff;
        another way of defining subnums

    define mult(num1, num2) -> product;

        if num1 and num2 are numbers return a number that is
        the result of multiplying num1 by num2

The next three are predicates for testing a relation between two or
three numbers. They all return a boolean value, i.e. true or false.

    define samenum(num1, num2) -> boole;
        true if the numbers are the same

    define bigger(num1, num2) -> boole;
        true if num1 is bigger than num2

    define between(num1, num2, num3) -> boole;
        true if num1 is between num2 and num3

Returning to arithmetical operators:

    define divide(num1, num2) -> (remainder, quotient);

        divide num1 by num2 and return the remainder and the quotient
        (num1 is the dividend, num2 the divisor)

The file ends with a challenge to extend these ideas to negative numbers
and some other challenges.


-- First procedure: a number recognizer -------------------------------

We need a number recognizer, which will be used in many other procedures
to check that they have been given a list, and failing that to produce
an error message (or MISHAP message in Pop-11).

We can define a number recognizer procedure as follows. It checks that
it is given a list, and that the list contains only occurrences of the
word "x".

It returns the boolean true or the boolean false, depending on what it
finds.

A first problem is whether the empty list should be used to represent a
number. An obvious answer is that it should represent the number 0.

We shall take that option below, but you could consider whether the
program could be changed so as not to allow the empty list.
*/

/*
PROCEDURE: isnum (item) -> boole
INPUTS   : item is anything
OUTPUTS  : boole is a truth-value (boolean)
USED IN  : Many procedures below.
CREATED  : 30 May 2009
PURPOSE  : Recognize a numeral in the form of a
           list that is either empty, i.e. [] or contains only
           occurrences of the word "x", e.g. [x] [x x x]

;;; specify some tests that will be used to check that the procedure
;;; works
TESTS:

isnum([x x x]) =>
** <true>

isnum([]) =>
** <true>

isnum([y y y]) =>
** <false>

isnum([xx]) =>
** <false>

isnum(99) =>
** <false>

*/

define isnum(item) -> boole;

    lvars L;

    ;;; if the list is empty
    if islist(item) and item matches []
    ;;; or the list starts with "x" followed by a number list
    or (islist(item) and item matches ![x ??L:isnum])
    then
        true
    else
        false
    endif ->  boole;
enddefine;


/*
-- A procedure for adding two nums ------------------------------------
*/

/*
PROCEDURE: addnums (num1, num2) -> sum
INPUTS   : num1, num2
  Where  :
    num1 is a num
    num2 is a num
OUTPUTS  : sum is a num
USED IN  : several procedures below
CREATED  : 30 May 2009
PURPOSE  : add two numbers (as lists) returning the sum (as a list).
           It should produce a mishap if given non-numbers.

TESTS:

addnums([], []) =>
** []

addnums([x], []) =>
** [x]

addnums([], [x]) =>
** [x]

addnums([x x], [x x x]) =>
** [x x x x x]

addnums([x x], [y y]) =>
;;; MISHAP - Nums needed for addnums
;;; INVOLVING:  [y y]
;;; FILE     :  /home/axs/popd/teachnums.p
;;; DOING    :  addnums runproc


;;; this should also produce a mishap (why?)

addnums([xx], [xxx]) =>
;;; MISHAP - Nums needed for addnums
;;; INVOLVING:  [xx]
;;; FILE     :  /home/axs/popd/teachnums.p
;;; DOING    :  addnums runproc

*/

define addnums(num1, num2) -> sum;

    ;;; first check that num1 and num2 satisfy isnum
    unless isnum(num1) then
        mishap('Nums needed for addnums', [^num1]);
    endunless;

    unless isnum(num2) then
        mishap('Nums needed for addnums', [^num2]);
    endunless;

    ;;; now we know they are both nums, so we can add them

    ;;; if either num1 or num2 is empty use the other as the result

    if num1 matches [] then
        num2 -> sum
    elseif num2 matches [] then
        num1 -> sum
    else
        ;;; add an "x" to num1 and reduce one from num2
        ;;; and add the two resulting lists
        addnums([ x ^^num1], tl(num2)) -> sum
    endif
enddefine;

;;; exercise: try defining a version of this using the Pop-11 concatenator
;;; operator <>

/*
-- A procedure to subtract a number from another ----------------------
*/

/*
PROCEDURE: subnums (num1, num2) -> diff
INPUTS   : num1, num2
  Where  :
    num1 is a number
    num2 is a number
OUTPUTS  : diff is a number
USED IN  : ???
CREATED  : 21 Jun 2009
PURPOSE  : ???
    if num1 is bigger than or equal to num2 then return a number which
    when added to num2 gives num1

TESTS:

subnums( [], [] ) =>
** []

subnums( [x], [] ) =>
** [x]

subnums( [], [x] ) =>

;;; MISHAP - subnums cannot subtract from zero
;;; INVOLVING:  [] [x]
;;; FILE     :  /home/axs/popd/teachnums.p
;;; DOING    :  subnums runproc

subnums( [x x x], [x x x x] ) =>

;;; MISHAP - subnums cannot subtract from zero
;;; INVOLVING:  [] [x]
;;; FILE     :  /home/axs/popd/teachnums.p
;;; DOING    :  subnums(*4) runproc


subnums( [x x x x], [x x x x] ) =>
** []
*/

define subnums(num1, num2) -> diff;

    if isnum(num1) and isnum(num2) then
        if num2 matches [] then
            num1 -> diff
        elseif num1 matches [] then
        mishap('subnums cannot subtract from zero', [^num1 ^num2]);
        else
            ;;; subtract an "x" from both and return the difference
            subnums(tl(num1), tl(num2)) -> diff
        endif
    else
        mishap('Nums needed for subnums', [^num1 ^num2]);
    endif;
enddefine;

/*
-- Another version, subnums2 using the pattern matcher ----------------
*/

/*
PROCEDURE: subnums2 (num1, num2) -> diff
INPUTS   : num1, num2
  Where  :
    num1 is a number
    num2 is a number
OUTPUTS  : diff is a number
USED IN  : ???
CREATED  : 21 Jun 2009
PURPOSE  : ???
    if num1 is bigger than or equal to num2 then return a number which
    when added to num2 gives num1

TESTS:

subnums2( [], [] ) =>
** []

subnums2( [x], [] ) =>
** [x]

subnums2( [], [x] ) =>
;;; MISHAP - Cannot subtract bigger number from smaller
;;; INVOLVING:  [] [x]
;;; FILE     :  /home/axs/popd/teachnums.p
;;; DOING    :  subnums2 runproc

subnums2( [x x x], [x] ) =>
** [x x]

subnums2( [x x x], [x x x x] ) =>
;;; MISHAP - Cannot subtract bigger number from smaller
;;; INVOLVING:  [x x x] [x x x x]
;;; FILE     :  /home/axs/popd/teachnums.p
;;; DOING    :  subnums2 runproc

subnums2( [x x x x], [x x x x] ) =>
** []

subnums2( [x x x x x x x x], [x x x x] ) =>
** [x x x x]
*/


define subnums2(num1, num2) -> diff;

    if isnum(num1) and isnum(num2) then
        if num1 matches ![??diff ^^num2] then
            ;;; nothing else to do
        else
            mishap('Cannot subtract bigger number from smaller', [^num1 ^num2])
        endif
    else
        mishap('Nums needed for subnums', [^num1 ^num2]);
    endif;
enddefine;



/*
-- Multiplication as repeated addition --------------------------------
*/

/*
PROCEDURE: mult (num1, num2) -> product
INPUTS   : num1, num2
  Where  :
    num1 is a num
    num2 is a num
OUTPUTS  : product is a num
USED IN  : ???
CREATED  : 30 May 2009
PURPOSE  : multiply two numbers (as lists) returning the product

TESTS:

mult([], []) =>
** []

mult([], [x x x x]) =>
** []

mult([x x x x], []) =>
** []

mult([x x x x], [x]) =>
** [x x x x]

mult([x], [x x x x]) =>
** [x x x x]

mult([x x], [x x x x]) =>
** [x x x x x x x x]

*/

define mult(num1, num2) -> product;
    if isnum(num1) and isnum(num2) then

        if num1 matches [] then
            [] -> product
        elseif num2 matches [] then
            [] -> product
        else

            [] -> product;
            until num2 matches [] do
                addnums(num1, product) -> product;
                tl(num2) -> num2;
            enduntil;
        endif
    else
        mishap('Nums needed for product', [^num1 ^num2]);
    endif;
enddefine;

;;; NOTE:
;;; A substantial chunk of the code in this procedure is redundant
;;; and can be removed, because of unobvious duplication.
;;; Which lines can be removed without changing the behaviour?
;;; Explain why?


/*
-- Some predicates for comparing numbers ------------------------------

-- -- samenum checks whether two numbers are the same
*/

/*
PROCEDURE: samenum (num1, num2) -> boole
INPUTS   : num1, num2
  Where  :
    num1 is a number
    num2 is a number
OUTPUTS  : boole is a boolean
USED IN  : divide
CREATED  : 10 Aug 2009
PURPOSE  : Testing numbers for equality

        true if the numbers are the same

TESTS:

samenum([], []) =>
** <true>

samenum([x], [x]) =>
** <true>

samenum([xx], [xx]) =>

;;; MISHAP - Nums needed for samenum
;;; INVOLVING:  [xx] [xx]
;;; FILE     :  /home/axs/popd/teachnums.p
;;; DOING    :  samenum runproc

samenum([x x], [x x]) =>
** <true>

samenum([x x x], [x]) =>
** <false>

samenum([], [x x]) =>
** <false>

*/

define samenum(num1, num2) -> boole;

    if isnum(num1) and isnum(num2) then

        num1 matches num2 -> boole

    else

        mishap('Nums needed for samenum', [^num1 ^num2]);

    endif
enddefine;


/*
-- -- bigger checks whether a number is bigger than another
*/

/*
PROCEDURE: bigger (num1, num2) -> boole
INPUTS   : num1, num2
  Where  :
    num1 is a num
    num2 is a num
OUTPUTS  : boole is a boolean
USED IN  : divide
CREATED  : 30 May 2009
PURPOSE  : determine whether num1 is bigger than num2

TESTS:

bigger([], []) =>
** <false>

bigger([x], []) =>
** <true>
bigger([], [x]) =>
** <false>

bigger([x x], [x x]) =>
** <false>

bigger([x x x], [x x]) =>
** <true>

bigger([x x x x x], [x x]) =>
** <true>

bigger([x x], [x x x x]) =>
** <false>


bigger([xx], [xxxx]) =>

;;; MISHAP - Nums needed for samenum
;;; INVOLVING:  [xx] [xxxx]
;;; FILE     :  /home/axs/popd/teachnums.p
;;; DOING    :  samenum bigger runproc


*/

define bigger(num1, num2) -> boole;
    ;;; note: we don't need to test that num1 and num2 are
    ;;; numbers, ad samenum does that

    if samenum(num1, num2) then
        false -> boole
    elseif num1 matches []  then
        false -> boole
    elseif num2 matches [] then
        true -> boole
    else bigger(tl(num1), tl(num2)) -> boole
    endif

enddefine;


/*
-- -- between checks whether a number is between two others
*/

/*
PROCEDURE: between (num1, num2, num3) -> boole
INPUTS   : num1, num2, num3
  Where  :
    num1 is a number
    num2 is a number
    num3 is a number
OUTPUTS  : boole is a boolean
USED IN  :
CREATED  : 21 Jun 2009
PURPOSE  : Check whether num1 is between num2 and num3
           in ascending or descending order

TESTS:

between([x], [x], [x]) =>
** <false>

between([x x], [x], []) =>
** <false>

between([x], [x x], []) =>
** <true>

between([x x], [x], [x x x]) =>
** <true>

between([x x], [x x], [x x x]) =>
** <false>
*/

define between(num1, num2, num3) -> boole;

    (bigger(num3, num1) and bigger(num1, num2))
    or
    (bigger(num2, num1) and bigger(num1, num3))
        -> boole

enddefine;


/*
-- A procedure to divide one number into another ----------------------
*/

/*
PROCEDURE: divide (num1, num2) -> (remainder, quotient);
INPUTS   : num1, num2
  Where  :
    num1 is a number (the dividend)
    num2 is a number (the divisor)
OUTPUTS  : remainder, quotient
  Where  :
    remainder is a number
    quotient is a number
USED IN  : ???
CREATED  : 21 Jun 2009
PURPOSE  : given two numbers num1, num2, divide num1 by num2 and
    return quotient and remainder

    quotient is the number of times num2 goes into num1

    remainder is the remainder after dividing num2 into num1

    (The remainder should be less than num2).

TESTS:

divide([], []) =>
;;; MISHAP - Cannot divide [] into []
;;; FILE     :  /home/axs/popd/teachnums.p
;;; DOING    :  divide runproc

;;; do the next two give reasonable answers?
divide([], [x]) =>
** [] []

divide([], [x x]) =>
** [] []

divide([x], []) =>
;;; MISHAP - Cannot divide [] into [x]
;;; FILE     :  /home/axs/popd/teachnums.p
;;; DOING    :  divide runproc

divide([x], [x]) =>
** [] [x]

divide([x x x x], [x x x x]) =>
** [] [x]

divide([x x x x], [x x]) =>
** [] [x x]

divide([x x x x x x x], [x x x]) =>
** [x] [x x]

divide([x x x x x x x x x], [x x x]) =>
** [] [x x x]

*/


define divide(num1, num2) -> (remainder, quotient);

    ;;; we don't need to check that they are numbers as
    ;;; bigger and samenum will do that

    if num2 == [] then
        mishap('Cannot divide [] into ' >< num1, []);
    elseif bigger(num2, num1) then
        [] -> quotient; num1 -> remainder
    elseif samenum(num1, num2) then
        [x] -> quotient; [] -> remainder
    else
        divide(subnums(num1, num2), num2) -> (remainder, quotient);
        addnums([x], quotient) -> quotient;
    endif;

enddefine;

/*
-- A hard problem: how to represent and operate on negative numbers ---
*/

/*
What should subnums do if the second number is larger than the first?

subnums([x x], [x x x]) =>

At present it gives this mysterious error message (which could be
modified of course):

    ;;; MISHAP - subnums cannot subtract from zero
    ;;; INVOLVING:  [] [x]
    ;;; FILE     :  /home/axs/popd/teachnums.p
    ;;; DOING    :  subnums(*3) runproc


You have been taught that the result of subtracting 3 from 2 should be a
negative number.

But what is a negative number?

How can you represent negative numbers using lists?

How should they be added, subtracted or multiplied?

Why should multiplying two negative numbers give a positive number?

Try to find ways of extending this package that are not totally
arbitrary and give a sensible interpretation of negative numbers.

If you come up with a suggestion please write to me about it (email
address at end of this file).

THERE IS NO UNIQUE RIGHT ANSWER
If you find an answer feel free to email me about it (address at bottom
of file), or post it on the comp.lang.pop forum.

You may find this paper by Pamela Liebeck relevant, if you can get hold
of it.

Pamela Liebeck,
    Scores and Forfeits: An Intuitive Model for Integer Arithmetic,
    Educational Studies in Mathematics,
    Vol 21, No 3, June, 1990, pp. 221--239,

Available on JSTOR here from subscribing academic institutions:
http://links.jstor.org/sici?sici=0013-1954%28199006%2921%3A3%3C221%3ASAFAIM%3E2.0.CO%3B2-N

*/

/*
-- Another hard problem: how to divide 4 by 7 and produce a fraction --
*/

/*
At present dividing a number by another number using the procedure
divide, defined above, gives a remainder and a quotient, as in these
examples:

    divide([x x x x x x x x], [x x x])=>
    ** [x x] [x x]

    divide([x x x x x x x x], [x x x x])=>
    ** [] [x x]

    divide([x x x x x x x x], [x x x x x])=>
    ** [x x x] [x]

    divide([x x], [x x x])=>
    ** [x x] []

    divide([x x x], [x x x x x])=>
    ** [x x x] []

Can you think of a way of changing the procedure divide(num1, num2), so
that instead of producing two numbers as its result it produces a single
object that can be thought of as a ratio (num1/num2).

The way you represent ratios should have the property that if you divide
num1 by num2 to get a ratio, then if you multiply that ratio by num2
you should get back to num1.

Can you make this work with negative as well as positive numbers.

The key idea is to think of a good way to represent ratios so that they
preserve all the required information, but have no redundant
information.

If you come up with a suggestion please write to me about it (email
address at end of this file).

THERE IS NO UNIQUE RIGHT ANSWER
If you find an answer feel free to email me about it (address at bottom
of file), or post it on the comp.lang.pop forum.
*/

/*
-- Introducing 'place' notation to compress number representations ----
*/

/*
If you use the notation developed here to represent numbers you will
soon realise that it is analogous to representing numbers by sequences
of strokes:

    | || ||| |||| ||||| ||||||

In principle any number, no matter how large, could be represented
either as a sequence of strokes, or as a list of x's. However this can
get very unwieldy for very large numbers. E.g. this is how the number
217 could look (with the list 'folded' to fit within a reasonable width:

   [x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
    x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
    x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
    x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
    x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
    x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x]

And this is how it could look using the stroke notation without spaces:

    |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    ||||||||||||||||

Compare that with the brevity of using three digits in  "217". This
works because the numbers are interpreted as

    keep the right most number
    add 10 times the number next left
    add 100 times the number next left
    .....

giving 7 + 10x1 + 100x2, or, if you prefer 2x100 + 10x1 + 7

Try working out a way to get a comparable compression using lists of x's
so that instead of a number like 217 being represented by one long list
of x's it is represented by three much shorter lists.

If you can develop such a notation try designing and testing some
procedures to convert between the above simple-minded list notation and
a place-based notation.

Also modify the procedures for adding, subtracting, multiplying and
dividing so that they can handle numbers represented in the new way.

THERE IS NO UNIQUE RIGHT ANSWER
If you find an answer feel free to email me about it (address at bottom
of file), or post it on the comp.lang.pop forum.
*/

/*
-- Can you use numbers for counting? ----------------------------------
*/

/*
(a) If I give you a list of words, and ask you how many there are, you
can point at them in turn as you recite number names, and when you get
to the end you can announce the final name as the answer to the 'how
many' question.

(b) Alternatively if I give you a list of words and ask you to give me a
list containing the first three words, or the first nine words, you can
count along the list until you get to the number specified, and make a
list of all the words you have counted.
*/

/*
-- -- Try problem (a)
*/

/*
Can you define a procedure how_many that performs the first task?

It is useful to use the fact that in Pop-11, code run in brackets like
this will create a list of the values produced in the code:

    [% .... %]

That construct could be used in a how_many procedure:
*/

define how_many(list) -> number;

    lvars item;

    ;;; go through the list and create a new one representing a number
    ;;; of the types used above.

    [%
    for item in list do

        ;;;; what exactly ???

    endfor
    %] -> number;

enddefine;

/*
;;; Test it thus (and define some more tests of your own)

    how_many([ the hat ]) =>

;;; should produce

    ** [x x]

whereas

    how_many([the man with a hat is blind as a bat ]) =>

;;; should produce

    ** [x x x x x x x x x x]

;;; What should this do?

    how_many([ ]) =>

*/

/*
-- -- Try problem (b)
*/

/*
Can you define a procedure count_out that performs the second task,
namely to produce a specified number of elements from a list.
*/

define count_out(items, number) -> list;

    ;;; what should go in here?

    ;;; you need a look to generate the numbers, one at a time,
    ;;; and as you do that you save an item from the list items.

    ;;; when you generate the target number given, the program can stop
    ;;; counting and return the list of saved items.

enddefine;


/*
;;; Test it thus (and define some more tests of your own)

    ;;; save a list of words for re-use
    vars words = [the man with a hat is blind as a bat];

    count_out(words, [x x]) =>

should produce

    ** [the man]

    count_out(words, [x x x x x]) =>

should produce

    ** [the man with a hat]

What should this do ?

    count_out(words, []) =>
*/

/*
-- Introducing numerals -----------------------------------------------
*/

/*
There are many things we use numbers for which would be very clumsy if
we were to use lists of 'x's as names. So it would be useful to have
Pop-11 words as names, providing a short hand notation. E.g.

    "one"   could be a name for [ x ]
    "two"   could be a name for [ x x ]
    "three" could be a name for [ x x x]
    "four"  could be a name for [ x x x x ]

and so on.

Can you find a way of setting up a mapping between the words and the
numbers, where the numbers are represented as lists of 'x's?

If you do that, how could you modify the previous two definitions so
that

    how_many([ the hat ]) =>

produces
    ** two

and

    count_out(words, "five") =>

produces

    ** [the man with a hat]
*/

/*
-- How about using place notation with number names? ------------------
*/

/*
Could you produce a program that understands the words

    nought one two .... nine

and then can interpret lists of those words as representing numbers in
the usual decimal way, e.g.

    what_number([three]) =>

should produce

    [x x x]

    what_number([three five]) =>

should produce a list of 35 'x's

    [x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x]

    what_number([three five nine]) =>

should produce a list of 359 'x's, and so on.

What should these produce

    what_number([three five nought]) =>

    what_number([three nought five]) =>


    what_number([nought three five]) =>

    what_number([nought nought three five]) =>
*/


/*
-- More information ---------------------------------------------------


For more information on poplog see
    http://www.cs.bham.ac.uk/research/projects/poplog/freepoplog.html

There is a section on teaching using Pop-11
    http://www.cs.bham.ac.uk/research/projects/poplog/freepoplog.html#teaching

There is a Pop-11 primer here:
    http://www.cs.bham.ac.uk/research/projects/poplog/primer/

========
Aaron Sloman
Email: A.Sloman@cs.bham.ac.uk
http://www.cs.bham.ac.uk/~axs

--- ?????/teachnums.p
--- Copyright University of Birmingham 2009. All rights reserved.
--- See http://www.cs.bham.ac.uk/research/projects/poplog/copyright.html

*/
