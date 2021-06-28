/*
TEACH NUMBER-RULEBASE.P                                  25 Nov 2011
Modified: 28 Nov 2011

Setting up mappings between numbers and their verbal descriptions
http://www.cs.bham.ac.uk/research/projects/poplog/teach/number-rulebase.p

This teach file is the subject of a video tutorial available on the CogAff
web site and on YouTube

    http://www.cs.bham.ac.uk/research/projects/cogaff/tutorials/number-rulebase.ogv 

    http://www.youtube.com/aaronsloman#p/a/u/0/9TAQoRpvLFE

In the video presentation, which was unscripted, I made a couple of minor mistakes.
If you notice them try to work out what I should have said.

Some of the rules displayed during the video have now had substantial comments
added in this file, especially the rules countnumbers_from and countto

Aaron Sloman
School of Computer Science University of Birmingham

===============================================================================

This is one of a number of tutorials on how the benefits of programming and
computer science go beyond providing lots of useful, exciting, entertaining
practical applications.

In addition, acquiring a deep understanding of ways of designing, building and
testing complex information processing systems can give us new ways of thinking
about how NATURAL information processing systems, produced by biological
evolution, such as animal minds and human minds work.

One important aspect of human minds is their ability to learn about mathematics,
and how to make mathematical discoveries. That includes learning about numbers.
This tutorial shows how to use a rule-based system to model, in a simplified
way, some aspects of what humans learn.

The system used here is Poprulebase, an extension of Pop11, a powerful language
developed for teaching and research in AI (Artificial Intelligence). Pop11 and
its extension, Poprulebase, are part of Poplog, described here.

    http://www.cs.bham.ac.uk/research/projects/poplog/freepoplog.html

This file is a mixture of comments and executable Pop11 and Poprulebase code.

It can be compiled using the editor command <ENTER> l1, to make the program
available for testing or demonstration.


CONTENTS - (Use <ENTER> g to access required sections)

 -- Introduction
 -- Learning about numbers vs learning about numerosity
 -- The language used here: Pop11, part of Poplog
 -- The current set of rules
 -- Compiling the program
 -- Running the ruleset
 -- Load prerequisite libraries
 -- Define the demo ruleset concerned with counting
 -- The control procedure run_counting(data);
 -- Exercises
 -- Further reading

-- Introduction ---------------------------------------------------

This is one of a collection of tutorials illustrating the notion of "thinky"
programming, explained in

    http://www.cs.bham.ac.uk/research/projects/poplog/examples

The importance of computing and knowledge of computing is very widely
acknowledged in many of not all countries.

Yet, of all the reasons for learning about computing, and for teaching it in
school, the one that is of the deepest significance in the long term goes
largely unrecognised and untaught, though it was recognised by some people
several decades ago.

I tried to explain some of it in my 1978 book (now freely available online):
    The Computer Revolution in Philosophy: Philosophy, Science and
    Models of Mind
    http://www.cs.bham.ac.uk/research/projects/cogaff/crp/

This tutorial focuses on a subset of the ideas in Chapter 8, online here:
    http://www.cs.bham.ac.uk/research/projects/cogaff/crp/chap8.html
    On Learning About Numbers: Problems And Speculations

This is one of many illustrations (albeit enormously simplified for tutorial
purposes) of ways in which learning about computing provides us not only with
new means of creating and using many tools, machines, and services based on
computers but also provides us with powerful new tools for thinking and
reasoning about complex structures and processes, including processes that go on
in products of biological evolution: seeing, understanding, learning, planning,
acting, controlling, communicating, discovering, proving, wanting, wondering
whether and many more.

Those are all natural (biological) forms of information processing, many of
which are still not understood and cannot yet be replicated in computers and
robots. But even when we don't yet know how the natural example works in any
detail, we can attempt to clarify aspects of what it can do, what its
competences are, what the uses of those competences are, and the construction of
such simplified models can be useful stepping stones towards modelling the
processes in full.

This tutorial focuses on some aspects of what goes on when a young human learns
about numbers.

-- Learning about numbers vs learning about numerosity ----------------

The concepts of "number" and "numerosity" are often confused by psychologists
and biologists studying animal cognition. Learning about numbers is not the same
as learning about "numerosity", e.g. being able to look at a group of objects
and say "two", "three", or "four", etc. depending on the visual appearance of
the group.

That's because numbers are essentially concerned with sequential discrete
operations, especially operations making use of or setting up one-to-one
mappings between two or more sets of objects, whereas the visual labels need not
be: they can involve classification of patterns or templates -- a very different
process. Some of these ideas are explained in more detail in this chapter:

    http://www.cs.bham.ac.uk/research/projects/cogaff/crp/chap8.html
    On Learning About Numbers: Problems And Speculations

However, even there, the topic is considerably simplified. But I hope that a few
young learners can be inspired to think about and later develop and deploy
computational theories about some of the kinds of learning that go on in young
children and about some of the uses to which we put the results of that
learning.

This tutorial is an incomplete introduction to some of the techniques that might
be used. We shall not attempt to model neurons, brain chemicals or any of the
physical processes that go on in human brains.

But we can hope to model (in simplified form) some of the thinking and discovery
processes that go on in young learners after they have begun to learn about
numbers.

-- The language used here: Pop11, part of Poplog ----------------------

For this purpose, I have chosen a language, Pop11, developed for teaching and
research in Artificial Intelligence, based on an older language Pop2 developed
at Edinburgh University. Some of the features of Pop11 are summarised here:
    http://en.wikipedia.org/wiki/Pop11

It is part of the Poplog development environment, containing Pop11 and three
other languages implemented in Pop11.
    http://en.wikipedia.org/wiki/Poplog

One of the features of Pop11 that makes it very useful for teaching is that it
has a relatively simple core shared with many other languages but has additional
features that support a wide range of programming tasks some of which would be
very difficult in most other languages (even Lisp, which is probably the
language closest to Pop11).

In particular, Pop11 is extendable in ways that allow the development of new
special purpose programming languages that can be combined with Pop11. The
particular extension used here is called Poprulebase, a language built around a
collection of rules that can "fire" in an order that depends on other things
going on in the computer, instead of being linearly ordered like the
instructions in many more conventional programs. For experts, some more
information about Poprulebase and its applications can be found in

    http://www.cs.bham.ac.uk/research/projects/poplog/teach/rulebase
    http://www.cs.bham.ac.uk/research/projects/poplog/help/poprulebase
    http://www.cs.bham.ac.uk/research/projects/poplog/packages/simagent.html

This tutorial makes use of only a tiny subset of those extensions, including the
use of Pop11's list "pattern matcher" which enormously simplifies many forms of
symbol manipulation.

We'll use a simple collection of rules for modelling a subset of a child's
number competences, leaving it to learners to think of additional number
competences or other competences that could be modelled. Some may even want to
learn more about the poprulebase system in order to go on to do Artificial
Intelligence research, or development.

Initially I shall merely demonstrate the rules running, showing how some of the
rules specified below are related to the competences demonstrated.

Later, more ambitious teachers and students may decide to use these ideas for a
different type of learning.

*/

/*

-- The current set of rules -------------------------------------------

On 28 Nov 2011, the following rules were in the ruleset
(this may change in a later version).

    RULE start
    RULE getquestion
    RULE bye
    RULE transform1
    RULE transform2
    RULE transform3
    RULE unknown
    RULE new_number
    RULE new_numbers
    RULE nothingafter
    RULE whatafter
    RULE nothingbefore
    RULE whatbefore
    RULE countnumbers
    RULE countnumbers_from
    RULE countnumbers_fail
    RULE cannot_countfrom
    RULE countfrom
    RULE count_between_down
    RULE count_between_up
    RULE cannot_countto
    RULE countto
    RULE countitems
    RULE cannotcount
    RULE count
    RULE whatsfirst
    RULE whatslast
    RULE precedes
    RULE precedes_new
    RULE follows
    RULE follows_new
    RULE precedes
    RULE greater_unknown
    RULE greater
    RULE storedata
    RULE loadfile
    RULE dont_understand
    RULE cleanup


-- Compiling the program ----------------------------------------------

This file is a mixture of program code and commented out text, using Pop11's
comment brackets: /*  */

The program below defines a ruleset in this format:

    define :ruleset counting_rules;
        ...

    RULE <name>
        <conditions>
            ==>
        <actions>

    RULE <name>
        <conditions>
            ==>
        <actions>

    RULE <name>
        <conditions>
            ==>
        <actions>

        ...

    enddefine;

expressed in the notation of the Poprulebase extension of Pop11, which is part
of the SimAgent toolkit described here:

    http://www.cs.bham.ac.uk/research/projects/poplog/packages/simagent.html

If you have Poplog installed you can find more using these editor commands

Make the package available:

    uses newkit

    ENTER teach rulebase
        read a tutorial introduction to rule-based programming

    ENTER help poprulebase
        read a long, extensive documentation file on poprulebase

The ruleset below starts:

    define :ruleset counting_rules;

-- Running the ruleset ------------------------------------------------

Once the ruleset definition has been compiled, e.g. using

    ENTER l1

the ruleset is invoked via this procedure defined near the end of this file:

    define run_counting(data);

whch invokes a command of the form:

    prb_run("counting_rules", <initial database>);

The initial database can be empty or contain some "innate" list of number names,
e.g.
    [numwords one two three]

    ;;; Commands to start the program
    run_counting([]);

    ;;; or, for example
    run_counting([one two three]);

*/

/*
-- Load prerequisite libraries ----------------------------------------

The 'uses' command loads the library only if it has not yet been compiled.
*/

;;; Load the main package
uses newkit
uses poprulebase

;;; some extensions required for this demo
uses prb_interact
uses rdl
uses list_to_string


/*

-- Define the demo ruleset concerned with counting --------------------

RULESET  : counting_rules
CREATED  : 25 Nov 2011
PURPOSE  : Demonstrate the use of poprulebase for modelling a changing
competence.

The command

    prb_run("counting_rules", []);

will start the ruleset with an empty database, which can be extended during the
ensuing interaction, with commands like

    one is a number

    two is a number

    three four and five are numbers

and interrogated in various ways

    count up to six

*/

define :ruleset counting_rules;

    [DLOCAL [prb_allrules = true]];

RULE start

    [NOT started]
        ==>
    ;;; [ADD numwords one two three four five six seven eight nine]
    ;;; [ADD numwords]
    [ADD started]

RULE getquestion
    [NOT sentence ==]
        ==>
    [READ 'What would you like me to do next?' [sentence ANSWER]]

RULE bye
    [sentence bye]
        ==>
    [SAY 'I think this lesson was worth' [$$ random(20)] 'pounds. Thank you.']
    [STOP]

RULE transform1
    ;;; capitalise LOAD for easier recognition
    [sentence load ??rest]
        ==>
    [NOT sentence ==]
    [sentence LOAD ??rest]

RULE transform2
    ;;; capitalise STORE for easier recognition
    [sentence store ??rest]
        ==>
    [NOT sentence ==]
    [sentence STORE ??rest]

RULE transform3
    ;;; replace bigger or larger or higher with greater, to simplify rules
    [OR
        [sentence ??first bigger ??rest]
        [sentence ??first larger ??rest]
        [sentence ??first higher ??rest]
    ]
        ==>
    [NOT sentence ==]
    [sentence ??first greater ??rest]

RULE unknown
    [OR [sentence == whats after ?numname ==]
        [sentence == comes after ?numname ==]
        [sentence == whats before ?numname ==]
        [sentence == comes before ?numname ==]
        [sentence == count from ?numname ==]
        [sentence == count == to ?numname ==]
        [sentence is ?numname ==]
    ]
    [NOT numwords == ?numname ==]
        ==>
    [SAY ?numname is not a known word]
    [TESTADD answered]

RULE new_number
    [sentence == ?numname is a number ==]
    [NOT numwords == ?numname ==]
    [numwords ??numwords]
        ==>
    [NOT numwords ??numwords]
    [numwords ??numwords ?numname]
    [SAY I have added ?numname as my last number word]
    [answered]

RULE new_numbers
    [OR
        [sentence == ??numnames are == numbers ==]
        [sentence additional numbers are ??numnames]
        [sentence the next numbers are ??numnames]
        [sentence the numbers ??numnames come next]
    ]
    [NOT numwords == ??numnames ==]
    [numwords ??numwords]
        ==>
    [POP11 lvars item;
        [%
            for item in numnames do
                unless member(item, [and also , .]) then item endunless
            endfor
        %] -> numnames;
    ]
    [NOT numwords ??numwords]
    [numwords ??numwords ??numnames]
    [SAY I have added ?numnames as new number words]
    [answered]

RULE nothingafter
    [OR
        [sentence == whats after ?numname ==]
        [sentence == what comes after ?numname ==]
    ]
    [numwords == ?numname]
        ==>
    [SAY Nothing comes after ?numname]
    [answered]

RULE whatafter
    [OR
        [sentence == whats after ?numname ==]
        [sentence == what comes after ?numname ==]
    ]
    [numwords == ?numname ?nextname ==]
        ==>
    [SAY ?nextname comes after ?numname]
    [answered]

RULE nothingbefore
    [OR
        [sentence == whats before ?numname ==]
        [sentence == what comes before ?numname ==]
    ]
    [numwords ?numname ==]
        ==>
    [SAY Nothing comes before ?numname]
    [answered]

RULE whatbefore
    [OR
        [sentence == whats before ?numname ==]
        [sentence == what comes before ?numname ==]
    ]
    [numwords == ?prevname ?numname ==]
        ==>
    [SAY ?prevname comes before ?numname]
    [answered]

RULE countnumbers
    ;;; this rule has to come earlier than some of the others
    [NOT answered]
    [OR
        [sentence count the numbers from ?name1 to ?name2]
        [sentence how many numbers from ?name1 to ?name2]
    ]
    [numwords == ?name1 ??rest ?name2 ==]
    [numwords ??numwords]
        ==>
    [NOT sentence ==]
    [POP11
        lvars items = [^name1 ^^ rest ^name2], name, num,
            lastname, lastnum;

        for name, num in items, numwords do
            [^num ^name] ==>
            num -> lastnum;
            name -> lastname;
        endfor;
        if lastname = name2 then
            [there are ^lastnum numbers from ^name1 to ^name2 inclusive] =>
        else
            ;;; should not happen
            [Sorry I lost track] =>
        endif;
    ]
    [answered]

RULE countnumbers_from
    [NOT answered]
    [sentence count the numbers from ?name1]
    [numwords == ?name1 ??rest]
    [numwords ??numwords]
        ==>
    [NOT sentence ==]
    [POP11
        ;;; variables local to this action
        lvars items = [^name1 ^^rest], num, name, lastnum;

        ;;; Note the parallel iteration over two lists:
        ;;; 'num' iterates over the contents of numwords, used for counting
        ;;; 'name' iterates over the contents of items, being counted

        ;;; The iteration here uses 'for ... in ... do ... endfor' whereas the
        ;;; iteration in countitems, below, uses 'for ... on ... do ... endfor'

        for num, name in numwords, items do
            [^num ^name] ==>
            ;;; keep a memory of the last number used for counting, as it
            ;;; will be needed in the summary printed after the loop
            num -> lastnum;
        endfor;

        [there are ^lastnum numbers from ^name1 onwards] =>
    ]
    [answered]

RULE countnumbers_fail
    [NOT answered]
    [sentence count the numbers from ?name1 to ?name2]
    [NOT numwords == ?name1 ??rest ?name2]
        ==>
    [SAY Sorry - I do not seem to have numbers from ?name1 to ?name2]
    [answered]


RULE cannot_countfrom
    [sentence == count from ?numname ==]
    [numwords == ?numname]
        ==>
    [SAY Sorry I dont know any numbers after ?numname]
    [answered]

RULE countfrom
    [NOT answered]
    [sentence == count from ?numname]
    [numwords == ?numname ??rest]
        ==>
    [SAY  ?numname ??rest]
    [answered]

RULE count_between_down
    [NOT answered]
    [OR
        [sentence count == from ?name1 == to ?name2 ==]
        [sentence count == to ?name2 == from ?name1 ==]
    ]
    [numwords == ?name2 ??rest ?name1 ==]
        ==>
    [POP11 rev(rest) -> rest]
    [SAY ?name1 ??rest ?name2]
    [answered]

RULE count_between_up
     [NOT answered]
    [OR
        [sentence count == from ?name1 == to ?name2 ==]
        [sentence count == to ?name2 == from ?name1 ==]
    ]
    [numwords == ?name1 ??rest ?name2 ==]
        ==>
    [SAY ?name1 ??rest ?name2]
    [answered]

RULE cannot_countto
    [sentence == count == to ?numname ==]
    [numwords ?numname ==]
        ==>
    [SAY Sorry I dont know any numbers before ?numname]
    [answered]

RULE countto
    ;;; needs to come after count_between_up and count_between_down
    [NOT answered]
    [sentence == count == to ?numname ==]
    [numwords ??first ?numname ==]
        ==>
    [SAY ??first ?numname]
    [answered]

RULE countitems
    ;;; modified 28 Nov 2011
    [NOT answered]
    [OR
        [sentence count == : ??items]
        [sentence how many == : ??items]
    ]
    [numwords ??numwords]
        ==>
    [POP11

        ;;; like the action in rule countnumbers_from above, this action
        ;;; iterates over two lists in parallel: the variable 'numwords' is
        ;;; used to move along the list links in the list numwords (found in
        ;;; the last condition before "==>", and the variable 'items' moves
        ;;; along the list items, found in the OR condition above.

        ;;; But there is an important difference. In the previous rule, the
        ;;; loop variables took as successive values items IN each of the rules
        ;;; (e.g. the number names 'one', 'two', ... in the list numwords), whereas
        ;;; in this loop the variables take as successive values the TAILS of the two
        ;;; lists, which is why the word 'on' is used instead of 'in, i.e.
        ;;;     for numwords, items *on* numwords, items do
        ;;; instead of
        ;;;     for numwords, items *in* numwords, items do
        ;;;
        ;;; This means that instead of the successive values of 'numwords' being
        ;;; 'one', 'two', 'three', they are the lists
        ;;; [one two three four ...], then [two three four ...] then [three four ...]
        ;;; and so on.

        ;;; The 'on' form of loop is useful when the program code after the loop needs
        ;;; to know how far the loop got in case it did not get to the end as can
        ;;; happen in this case if there are not enough names in numwords to count
        ;;; everything in items.

        lvars lastnum; ;;; Remember last number used for counting.

        for numwords, items on numwords, items do
            [% hd(numwords), hd(items) %] ==>
            hd(numwords) -> lastnum;
        endfor;

        ;;; Did we get to the end of items? Not if length(items) > 0
        ;;; or equivalently items /= [] (items is not the empty list).

        if items /= [] then
            
            [I counted ^lastnum items -- but] =>
            [I dont know enough number words to continue] =>
        else
            [There were ^lastnum items] =>
        endif;
    ]
    [answered]

RULE cannotcount
    [NOT answered]
    [sentence count ==]
    [numwords]
        ==>
    [SAY 'Sorry: I don\'t know any numbers']
    [answered]

RULE count
    [NOT answered]
    [OR
        [sentence count ==]
        [sentence show == numbers ==]
        [sentence recite == numbers ==]
    ]
    [NOT sentence count == from ==]
    [NOT sentence count == to ==]
    [numwords ??numwords]
    [WHERE numwords /= []]
        ==>
    [SAY ??numwords]
    [answered]

RULE whatsfirst
    [sentence == the first number ==]
    [numwords ?first ==]
        ==>
    [SAY the first number is ?first]
    [answered]

RULE whatslast
    [sentence == the last number ==]
    [numwords == ?last]
        ==>
    [SAY the last number is ?last]
    [answered]

RULE precedes
    [sentence == ?name1 precedes ?name2 ==]
    [numwords == ?name1 ?name2 ==]
        ==>
    [SAY I already know that ?name1 precedes ?name2]
    [answered]

RULE precedes_new
    [sentence == ?name1 precedes ?name2 ==]
    ;;; is name2 the first item in numwords?
    [numwords ?name2 ??rest]
        ==>
    ;;; delete current list
    [NOT numwords ==]
    ;;; add new list of words
    [numwords ?name1 ?name2 ??rest]
    [SAY  I now know that ?name1 is now the first number. Thanks.]
    [answered]

RULE follows
    ;;; i.e. follows
    [OR
        [sentence == ?name2 follows ?name1 ==]
        [sentence == ?name2 succeeds ?name1 ==]
        [sentence == ?name2 comes after ?name1 ==]
        [sentence == ?name2 goes after ?name1 ==]
    ]
    [numwords == ?name1 ?name2 ==]
        ==>
    [SAY I already know that ?name2 comes after ?name1]
    [answered]

RULE follows_new
    [OR
        [sentence == ?name2 follows ?name1 ==]
        [sentence == ?name2 succeeds ?name1 ==]
        [sentence == ?name2 comes after ?name1 ==]
        [sentence == ?name2 goes after ?name1 ==]
    ]
    [numwords ??first ?name1]
        ==>
    [NOT numwords ==]
    [numwords ??first ?name1 ?name2]
    [SAY I now know that ?name2 is the last number. Thanks.]
    [answered]

RULE precedes
    ;;; i.e. follows
    [OR
        [sentence == ?name2 precedes ?name2 ==]
        [sentence == ?name2 comes before ?name2 ==]
        [sentence == ?name2 goes before ?name2 ==]
        [sentence == ?name2 = earlier than ?name2 ==]
    ]
    [numwords == ?name1 ?name2 ==]
        ==>
    [SAY I already know that ?name2 comes after ?name1]
    [answered]


RULE greater_unknown
    [NOT answered]
    [sentence is ?name1 greater than ?name2]
    [NOT numwords == ?name2 == ]
        ==>
    [SAY sorry ?name2 is not recognized]
    [answered]

RULE greater
    [NOT answered]
    [OR
        [sentence is ?name1 greater than ?name2]
        [sentence does ?name1 exceed ?name2]
        [sentence is ?name2 smaller than ?name1]
        [sentence does ?name2 precede ?name1]
        [sentence does ?name2 come before ?name1]
    ]
    [numwords ??firsts1 ?name1 ==]
    [numwords ??firsts2 ?name2 ==]
        ==>
    [POP11
        lvars
        l1 = length(firsts1),
        l2 = length(firsts2);

        if name1 = name2 then
        'No they are the same number ' =>

        elseif l1 > l2 then
        'Yes ' >< name1 >< ' is bigger than ' >< name2 =>
        else
        'No ' >< name2 >< ' is bigger' =>
        endif
    ]
    [answered]

RULE storedata
    ;;; store database in named file
    [NOT answered]
    [sentence STORE ??filename]
        ==>
    [NOT sentence ==]
    ;;; prevent this getting stored
    [NOT started]
    [POP11
        list_to_string(filename) -> filename;
        if length(filename) > 0 then
        'Storing database in ' >< filename =>
        prb_storedata(filename);

        if sys_file_exists(filename) then
        'Successfully created ' >< filename =>
        else
        'Something went wrong: please try again' =>
        endif
        else
        '\nPlease type filename in string quotes \'....\'\n' =>
        endif
    ]
    ;;; reinsert
    [started]
    [answered]


RULE loadfile
    ;;; store database in named file
    [NOT answered]
    [sentence LOAD ??filename]
        ==>
    [NOT sentence ==]
    ;;; prevent this getting stored
    [NOT started]
    [POP11
        list_to_string(filename) -> filename;
        if length(filename) > 0 then

            if sys_file_exists(filename) then
                'Compiling ' >< filename =>
                pop11_compile(filename);
                'Compiled ' >< filename =>
            else
                'File does not exist: please try again' =>
            endif
        else
            '\nPlease type filename in string quotes \'....\'\n' =>
        endif
    ]

RULE dont_understand
    ;;; no pattern has matched
    [NOT answered]
    [sentence ??sentence]
        ==>
    [SAY Sorry I did not understand [??sentence]]
    [answered]

    ;;; Final rule gets rid of previously typed in sentence.
    ;;; A more sophisticated program could save previous sentences
    ;;; and sometimes refer back to them.
RULE cleanup
        ==>
    ;;; remove stuff from database
    [NOT sentence ==]
    [NOT answered]
/*
    ;;; for debugging
    [POP11 prb_print_database(); pr(newline)]
    */

enddefine;

/*

-- The control procedure run_counting(data); --------------------------

*/


/*
PROCEDURE: run_counting (data)
INPUTS   : data is a list of number names
OUTPUTS  : NONE
USED IN  : Running the counting_rules ruleset
CREATED  : 25 Nov 2011
PURPOSE  : Demonstration

TESTS:

;;; Start the program with this command
run_counting([]);

;;; or, for example, with some known number names
run_counting([one two three]);

;;; interact with it one line at a time, e.g.

    one is a number

    two three and four are numbers

    count from two to six

etc.

Finish with

    bye

*/

define run_counting(data);

    ;;; First set some global variables which control the behaviour
    ;;; of the interpreter.

    dlocal
        ;;; Prevent prb_run continually pausing. Make it true
        ;;; to slow things down and show what is going on
        prb_walk = false,

        ;;; Make this true for more tracing
        prb_chatty = false;

    ;;; now run the interpreter
    ;;; give it the the ruleset name, a word, not the ruleset itself,
    ;;; so that the new ruleset will be used if recompiled during execution.
    prb_run("counting_rules", [[numwords ^^data]]);

enddefine;

/*
;;; The command to run the rules.

run_counting([]);
run_counting([numwords one two three]);

*/
pr('\
;;; Commands to start the program\
run_counting([]);\
;;; or, for example, with some known number names\
run_counting([one two three]);\
');

/*
-- Exercises ----------------------------------------------------------

1. Try modifying some of the formats in which the program can be given questions
or instructions.

2. Try modifying the program so that it can do simple arithmetic

    what's two plus three

    what's three minus two

It could answer that by counting from the the successor of the lower number up
to the higher number.

What should it reply in answer to

    what's two minus three

Allow the questions to be asked in different formats:

    add two to three
    add two and three

    subtract two from four
    what's four take away two

etc.

3. What are the advantages of adding zero as a number?
Try doing that.

4. Try extending the program so that it can learn about

    teenwords: ten eleven twelve ... nineteen

    tentywords: twenty thirty forty fifty ... ninenty

Then allow it to count up to nineteen.

Then allow it to count beyond twenty (up to ninety nine)

5. Now extend the ability to addition and subtraction

6. Try adding multiplication, based on making copies of a list and counting the
result.

7. Try adding division.

*/

/*
-- Further reading ------------------------------------------------

Background pop11

    TEACH LISTS
    TEACH LISTSUMMARY
    TEACH MATCHES
    HELP  MATCHES
    TEACH ARITH
    HELP  ARITH
    HELP  NUMBERS
    TEACH TEACHNUMS
    HELP  LOOPS

    TEACH STACK
    TEACH DEFINE

    TEACH VARS_AND_LVARS

    TEACH GRAMMAR
    TEACH STORYGRAMMAR

Poprulebase and SimAgent

After the command:

    uses newkit

these should work:

    TEACH RULEBASE
    TEACH sim_agent
    TEACH sim_feelings
    TEACH sim_sheepdog.p

    TEACH number-rulebase.p
        (this file)

    HELP POPRULEBASE

    http://www.cs.bham.ac.uk/research/projects/poplog/packages/simagent.html

*/

/*
Relocated: 28 Nov 2011

--- $usepop/pop/teach/number-rulebase.p
--- Copyright University of Birmingham 2011. All rights reserved.

*/
