TEACH SUPER_EXAMPLE.P                              Aaron Sloman Aug 2009

EXAMPLES OF THE USE OF THE POP-11 "SUPERDATABASE": LIB * SUPER


This library extends the Pop-11 database with mechanisms that match much
of the power of Prolog. This teach file provides examples, supplementing
the previously available help file, HELP * SUPER

To make LIB * SUPER available do

    uses super


         CONTENTS - (Use <ENTER> g to access required sections)

 -- LIB SUPER
 -- -- Some examples of the sorts of things SUPER can do
 -- Printing out the database
 -- Using 'which' to get complete information
 -- Exercise
 -- Printing out the database
 -- -- Exercise: fruits and colours
 -- Removing things from the database
 -- Handling relations in SUPER
 -- Saving a database using copydata
 -- Atomic facts and generalisations
 -- -- Atomic facts (Recapitulation)
 -- -- Generalisations
 -- -- First example
 -- -- Second example
 -- -- EXERCISES
 -- Doing all that in SUPER
 -- -- Using 'foreach' and 'which' to get multiple answers
 -- -- Using 'or' to compress two rules into one
 -- -- Using 'segment variables' with '??'
 -- -- -- EXERCISES
 -- SEE ALSO

 --
 -- -- -- EXERCISES

-- LIB SUPER ----------------------------------------------------------

The Pop-11 SUPER library, created around 1982 by Steve Hardy, adds  many
of the features  of Prolog to  Pop-11, in  the form of  an extension  to
Pop-11's database (see TEACH * DATABASE).

A full description of  the library is in  HELP * SUPER, with  additional
examples in HELP * WHICH

The purpose of this file is simply to provide a few examples showing how
the LIB * SUPER facilities can be used as an alternative to Prolog.

In order to understand this teach file you do not need to know  anything
about Prolog, but it helps if you understand family relationships!


-- -- Some examples of the sorts of things SUPER can do

;;; You can use 'mark and load range' commands in Ved or XVed
;;; to run these examples.

uses super

;;; startup an empty database
newdatabase([]);

;;; add some facts about who is  big.

add([big john]);
add([big fred]);
add([big sally]);

;;; we can search for a person who is big

;;; declare a variable person
vars person;

;;; is there anyone small?
present([small ?person]) =>
** <false>


;;; anyone big?
present([big ?person]) =>
** <true>

;;; who was found?

person =>
** john

-- Printing out the database ------------------------------------------

At this stage, several things have been added to the database.

At any time you can print out the database, using Pop-11's  pretty-print
arrow (==>), like this

database ==>
** [[big [big john] [big fred] [big sally]]]


If you add some other facts the facts will be grouped according to their
first word.

add([round moon]);
add([round sun]);
add([round saturn]);

add([planet earth]);
add([planet saturn]);
add([star sun]);
add([satellite moon]);

;;; now look at the database
database ==>
** [[satellite [satellite moon]]
    [star [star sun]]
    [planet [planet earth] [planet saturn]]
    [round [round moon] [round sun] [round saturn]]
    [big [big john] [big fred] [big sally]]]


-- Using 'which' to get complete information --------------------------

;;; Previously used present to check one thing at a time

;;; we can find all the big persons using which?

vars person;

which("person", [[big ?person]]) =>
** [john fred sally]

;;; you can also use 'foreach' to find all the heavy things, and do
;;; something them one at a time.

;;; We use ?person as a variable to be set in a list by matching.
;;; We use ^person as a variable whose already set value is to be
;;; put in the list. We can't use "^" like that outside a list.

vars person;
foreach [big ?person] do
    [I know that ^person is big] =>
endforeach;
** [I know that john is big]
** [I know that fred is big]
** [I know that sally is big]


;;; is anyone heavy?
which("person", [[heavy ?person]]) =>
** []

;;; the empty list is returned, so nobody is heavy.

;;; But we can add an inference rule saying, that if you need
;;; to find someone heavy, check if there's someone big

add([ifneeded [heavy ?person] [big ?person]]);

;;; now is anyone heavy?

which("person", [[heavy ?person]]) =>
** [john fred sally]

;;; So SUPER has *inferred* that each of john, fred and sally is heavy.
;;; but it has not stored that information in the database, as you
;;; can tell by printing out the database.

;;; we can add the information that jack is small and heavy

alladd([[small jack] [heavy jack]]);

;;; who is small?
which("person", [[small ?person]]) =>
** [jack]

;;; who is heavy now?
which("person", [[heavy ?person]]) =>
** [john fred sally jack]

;;; So there's one more heavy person

-- Exercise -----------------------------------------------------------

Try adding a set of 'ifneeded' rules that say that something is
big if it a planet, or if it is a star, or if it is a satellite.

Then use 'which' to find all the big things.

Then use 'which' to find all the heavy things.

See if super can do two steps of inference, e.g. from planet to big, and
from big to heavy, to infer [heavy earth]. The inference from big to
heavy is already handled by an ifneeded rule above.

After you have added an appropriate ifneeded rule, check out which
things SUPER thinks are heavy.


-- Printing out the database ------------------------------------------

Now see how the database has grown:

database ==>
** [[heavy [ifneeded [heavy <ref person>] [big <ref person>]] [heavy jack]]
    [small [small jack]]
    [satellite [satellite moon]]
    [star [star sun]]
    [planet [planet earth] [planet saturn]]
    [round [round moon] [round sun] [round saturn]]
    [big [big john] [big fred] [big sally]]]

You may get something different printed out, depending on what
experimenting you have been doing.

The database includes not only the facts that were asserted but also the
rules using 'ifneeded'.

Notice how variables appear in the form <ref person> rather than ?person
when the database is printed out.

(Experienced programmers will guess that that means variables are stored
as references in the database. But you can ignore that if you don't know
what references are.)

-- -- Exercise: fruits and colours

Add some facts about colours of different kinds of fruit, e.g.

    tomatoes are red

    cherries are red

    bananas are yellow

Then add facts about pieces of fruit, e.g. called f1 f2 f3 f4 f5...

e.g.
    [tomato f1]
    [banana f2]

(Here there are no variables involved, because "?" is not used: so those
facts just include the Pop-11 words "f1", "f2", in addition to the words
"tomato" and "banana").

Then check which things are tomatoes, which are bananas, etc.

Then add rules about colours of different kinds of fruit, e.g.

   [ifneeded [red ?thing] [tomato ?thing]]

Then use a formula of the form which("thing", [....]) to find out which
things are red, which are yellow, etc.

-- Removing things from the database ----------------------------------

It is possible to remove items from the database using 'remove'.

There are two formats, depending on whether you are removing something
with or without variables.

;;; Let's use 'alladd' to add several things at once:

alladd([ [city london] [city paris] [city berlin] ]);

vars place;
which("place", [[city ?place]]) =>
** [london paris berlin]

;;; now remove one of those:
remove([city paris]);

;;; see what's left

vars place;
which("place", [[city ?place]]) =>
** [london berlin]

;;; If we wanted to use a pattern to find several items in the database
;;; and remove them all we can do this, using 'it' to refer to the last
;;; thing found.

foreach [city ?place] do remove(it) endforeach;

;;; Have they all gone?

which("place", [[city ?place]]) =>
** []


-- Handling relations in SUPER ----------------------------------------

So far, all  the elementary facts  added, involving one  pair of  square
brackets ([...]) have use a property  name (a 'predicate') and a  single
thing, e.g. [star sun] [city london], etc.

However many  (perhaps most)  of the  important things  about the  world
involve two or  more things  that stand  in some  relation. Examples  of
relational facts might be:

    [bigger saturn earth]
    [bigger fred joan]
    [inside tom kitchen]
    [inside joan bathroom]
    [heavier saturn earth]
    [inside kitchen house]
    [invented newton calculus]

(or Leibniz -- take your pick).

There are also relations between relations, which can be expressed in
SUPER using ifneeded rules, for example:

    [ifneeded [heavier ?thing1 ?thing2] [bigger ?thing1 thing2]]

So if you want something heavier than a particular object (e.g. earth)
you can look for something that is bigger than it (e.g. saturn).

In fact that rule is not true in our universe since things of very low
density can be big, but SUPER is not concerned with anything but
hypothetical reasoning
    (IF ... so and so ... THEN ... such and such ...)

Try using SUPER to  store some facts about  things that have  properties
and relationships, then add some  'ifneeded' rules that allow new  facts
about properties and  relationships to  be inferred from  old ones,  and
test that your rules work.

You  could  start  by  simply  using  household  objects,  and   spatial
relationships such as size, containment, being adjacent, etc.

Use both 'foreach'  and 'which' to  get SUPER to  answer questions,  and
decide which is best to use.

There is also  forevery, which  is more powerful  than foreach,  because
foreach takes  only one  pattern to  repeatedly match  against  database
items,  whereas  forevery  takes  a  list  of  patterns  and  looks  for
consistent ways of matching all of them.

This is illustrated below, and in HELP * SUPER


-- Saving a database using copydata -----------------------------------

In the example below we shall use the newdatabase procedure, which is
part of LIB SUPER. It is given a list of things to add, which it does
after creating a new empty database.

When it is used, all the previous database contents will be forgotten.

If you wish to save the  current database before using newdatabase,  you
can use the Pop-11 procedure copydata, thus:

vars olddata;

copydata(database) -> olddata;

Or in a single command

vars olddata = copydata(database);

Later you can go back to using the olddata database, by simply assigning
it:

    olddata -> database;

Thereafter database operations will operate on database, which initially
is the same thing as olddata. However if you add or remove things that
can alter olddata, which you may not want.

If you prefer to save olddata without allowing database operations
to change it, instead of the above simple assignment you can copy then
assign it:

    copydata(olddata) -> database;

Lib super, unlike the ordinary Pop-11 database does not provide a
mechanism for saving a SUPER database in the file.

This could be added, however.


-- Atomic facts and generalisations -----------------------------------

The examples so far have included both atomic facts and generalisations
(or rules) of various kinds.

-- -- Atomic facts (Recapitulation)

Atomic facts can be expressed as simple assertions that something  has a
property, or that some things stand in a relationship.

Examples:

    Everest is big
    Everest is heavy
    Susan is clever

which in the notation of SUPER could be

    [big Everest]
    [heavy Everest]
    [clever Susan]

Atomic facts can also use relations applied to two or more things, e.g.

    [bigger saturn earth]       saturn is bigger than earth

    [isin fred kitchen]         fred is in kitchen

    [between tom dick harry]    tom is between dick and harry

    [invented leibniz calculus] leibniz invented calculus

Note that we always need a convention about how the ordering of items
after the relation name, represents roles in that relation.

-- -- Generalisations

Generalisations are what logicians refer to as "universally  quantified"
propositions, which are  about all things  of a certain  sort, or  about
everything or  about  all  pairs  or  groups  of  things  in  a  certain
relationship.

Generalisations include things like:

    "Everything that is big is heavy"

    "Everyone who is the mother of someone who is the father of someone
        else is the grandmother of the someone else"

In  the  SUPER  notation,  those  generalisations  are  expressed  using
'ifneeded' database entries.  As in Prolog,  we express  generalisations
"back to front", with 'consequents' first followed by 'antecedents'.

-- -- First example

So the first example ("Everything that is big is heavy") becomes:

    "Everything is heavy, if it is big"

or in the  notation of  SUPER (where  ?x and  ?y should  be declared  as
Pop-11 variables).

    [ifneeded [heavy ?x] [big ?x]]

Which means:

    If you need to know that
        ?x is heavy,
    see if you can establish that
        ?x is big.

When something like that is attempted, and fails, no result is returned.
The program  just  indicates to  whatever  calls  it, that  it  has  not
succeeded. Sometimes such a failure can be taken to mean that the  thing
not proved is  false ('negation  as failure').  But more  often it  just
means that  the consequent  cannot be  derived from  information in  the
database.


-- -- Second example

The second example was:

    "Everyone who is the mother of someone who is the father of someone
        else is the grandmother of the someone else"

(Of course, this is not the only way to be a grandmother. See below).

This also gets turned back-to-front in SUPER (as in Prolog) thus:

    "If you need to know that
        ?x is the grandmother of ?y
    see if you can establish that
        ?x is the mother of ?z
        ?z is the mother of ?y

(This works only for maternal grandmothers. We'll deal with paternal
grandmothers later.)

In SUPER that could be expressed more concisely as:

     [ifneeded [grandma ?x ?y] [ma ?x ?z] [ma ?z ?y]]

Notice that this  mean that  in proving  that a  certain relation  holds
between two things you sometimes have to  find a third thing that is  in
the right relationships.

In Prolog it would be

    grandma(X, Y) :- ma(X, Z), ma(Z, Y).

But we shall not bother with Prolog syntax here.

(See HELP * PROLOG if you want to know more about the version of Prolog
that is available in Poplog.)

Remember: what comes immediately after  'ifneeded' is the conclusion  or
consequent. The ifneeded rule states that that consequent follows  from
the antecedents that form the rest of the ifneeded rule. E.g. the above
ifneeded rule states that:

the consequent

    [grandma ?x ?y]

follows from these two antecedents

    [ma ?x ?z] [ma ?z ?y]

However it follows only when  the variables have been given  appropriate
(and consistent) values. E.g. the value for ?x must be the same in  both
places where it occurs, and likewise for ?y and ?z.


-- -- EXERCISES

Write down both in English and in SUPER

    1. an additional rule for being a grandma

    2. two rules for being a grandpa

    3. a rule for being a grandparent

        [ifneeded [grandparent ?x ?y] .........]


-- Doing all that in SUPER --------------------------------------------

;;; make sure the library is compiled.
uses super


;;; Set up a NEW database with some atomic facts and some rules.
;;; As shown in HELP * SUPER and above, we could use newdatabase(),
;;; to start an empty database, and use 'add' many times, to put
;;; information into the SUPER database, but instead we use a single
;;; command to insert many things in a new database:

newdatabase([
              ;;; Atomic facts
              [ pa tom mary]
              [ pa tom dick]
              [ ma mary sue]
              [ ma mary fred]
              [ pa dick jane]

              ;;; Generalisations
              ;;; How can x be grandpa of y?
              [ifneeded [grandpa ?x ?y] [pa ?x ?z][pa ?z ?y]]
              [ifneeded [grandpa ?x ?y] [pa ?x ?z][ma ?z ?y]]

              ;;; How can x be grandma of y?
              [ifneeded [grandma ?x ?y] [ma ?x ?z][pa ?z ?y]]
              [ifneeded [grandma ?x ?y] [ma ?x ?z][ma ?z ?y]]

              ;;; How can x be grandparent of y?
              [ifneeded [grandparent ?x ?y] [grandpa ?x ?y]]
              [ifneeded [grandparent ?x ?y] [grandma ?x ?y]]
         ]);

NB: notice the outermost pair of square brackets inside the round
brackets.

;;; Some variables used later
vars person, other;

;;; As illustrated previously, we can use present
;;; (= "recorded in the database") to check individual facts,
;;; including partially specified facts:

present([pa ?person dick]) =>
** <true>

person =>
** tom

present([ma dick ?person]) =>
** <false>

;;; Try some others that return false!

;;; We can use present with more than one variable:

present([ma ?person ?other]) =>
** <true>

person, other =>
** mary sue

But you can't use a variable in the first position, e.g.

vars relation;

present([?relation mary sue]) =>
** <false>


-- -- Using 'foreach' and 'which' to get multiple answers

;;; Unfortunately, present can only find one thing at a time.

;;; We can use 'foreach' to report all the children of mary

foreach [ma mary ?person] do person => endforeach;
** sue
** fred

;;; As shown previously, we can also use the 'which' library to do this,
;;; returning a ;;; list of results. 'which' is given a variable name
;;; (quoted), and a list of patterns to be satisfied by the variable.

;;; It can also be given a list of variables, as shown later.

which("person", [[ma mary ?person]]) =>
** [sue fred]

;;; running 'which' is equivalent to running a loop and collecting
;;; the things found in the loop into a list.

;;; But mary is not the father of anyone

which("person", [[pa mary ?person]]) =>
** []

;;; Using a list of variables

vars person, other;

which([person other], [[pa ?person ?other]]) =>
** [[tom mary] [tom dick] [dick jane]]


;;; You can also give 'which' a list of patterns (which is why we had to
;;; use double brackets even when there was only one pattern.
;;; It treats one pattern as a special case.

;;; Find out which person(s) and other(s) stand in the grandpa
;;; relationship

which([person other], [[grandpa ?person ?other]]) =>
** [[tom jane] [tom sue] [tom fred]]

-- -- Using 'or' to compress two rules into one

In the example above we had two ifneeded rules for being a grandma

     [ifneeded [grandma ?x ?y] [ma ?x ?z][pa ?z ?y]]
     [ifneeded [grandma ?x ?y] [ma ?x ?z][ma ?z ?y]]

Notice how they are the same except for the last pattern.

How would you express that as a single generalisation in English?

It is possible to compress this into one rule using 'or', as follows
using 'gmother' as a new relation name for testing purposes.

    add([ifneeded [gmother ?x ?y] [ma ?x ?z] [or [pa ?z ?y] [ma ?z ?y]] ]);

Perhaps the following is clearer?

    add([ifneeded
            [gmother ?x ?y]
            [ma ?x ?z]
            [or [pa ?z ?y] [ma ?z ?y]]
        ]);

In English that is roughly, to check if x is the grandmother of y, check
if x is the mother of someone who is either the father of y or the
mother of y.

Lets add some more children, so that we can test that ifneeded rule:

    alladd([[pa fred tim][ma sue sally]]);

Test that now:

which([person other], [[gmother ?person ?other]]) =>
** [[mary sally] [mary tim]]


-- -- Using 'segment variables' with '??'

A 'segment variable' is one that can match any number of components.
For example, using the Pop-11 matcher we could use 'persons' as a
segment variable in this example, indicated by '??':

vars persons;

[ tom dick harry fred joe mary ] matches [== dick ??persons mary ==] =>
** <true>

;;; who were the persons?

persons =>
** [harry fred joe]

So the segment variable matched a list of three elements in the middle
of the first list.

(For more on the Pop-11 matcher see TEACH * MATCHES, HELP * MATCHES)

In the SUPER library it is possible to use '??' as a variable prefix in
a pattern, to match an arbitrary number of items in a list (a segment of
the list), PROVIDED that it is the LAST variable in the pattern.

This restriction on the use of 'segment variables' is also in prolog.

-- -- -- EXERCISES

1. Construct and run an example using 'which' to find all the
mother and child pairs.

2. Construct and run an example to find all the individuals of whom
tom is a grandparent. (Look back at the ifneeded rule for grandparent.)

3. Try completing these in various ways, by replacing the dots, and then
run the resulting commands (remembering that variables in the
second argument of 'which' need '?' or '??'

    which("person", [[grandparent ... jane]]) =>
    which("person", [[grandparent tom ... ]]) =>

    which("persons", [[grandpa tom ?persons]]) =>

which([person persons], [[grandpa ?person ?persons]]) =>


3. Using add in the format

    add([ifneeded [greatgrandma ...] [...] [...]]);

insert a rule for someone to be a someone's great grandmother.

Can you use 'or' to express that as a single ifneeded rule?


-- SEE ALSO -----------------------------------------------------------

HELP * SUPER

HELP * WHICH

TEACH * MATCHES

HELP * MATCHES


See HELP * SUPER, * FOREVERY, * FOREACH

--- $usepop/pop/teach/super_example.p
--- Copyright University of Birmingham 2009. All rights reserved.
