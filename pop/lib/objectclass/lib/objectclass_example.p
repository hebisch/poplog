/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/lib/objectclass_example.p
 > Purpose:         Code from the objectclass tutorial
 > Author:          Robert John Duncan, Dec  1 1995
 > Documentation:   TEACH * OBJECTCLASS_EXAMPLE
 */

uses objectclass;

define :class person;
    slot person_name = undef;
    slot person_age  = 0;
    slot person_sex  = undef;
enddefine;

;;; create a new person
vars fred = consperson("fred", 5, "male");
fred =>

;;; create another person, with default attributes
vars anonymous = newperson();
anonymous =>

destperson(anonymous) =>

vars (freds_name, freds_age, freds_sex) = destperson(fred);
freds_name =>
freds_age =>

isperson(fred) =>

isperson(anonymous) =>

isperson("fred") =>

person_name(fred) =>

person_age(fred) =>

6 -> person_age(fred);  ;;; it must be his birthday
person_age(fred) =>

consperson("mary", 32, "female") =>

newperson() =>

vars mary = newperson();
"mary" -> person_age(mary);
"female" -> person_sex(mary);

instance person
    person_name = "mary";
    person_sex  = "female";
endinstance =>

define :instance mary:person;
    person_name = "mary";
    person_sex  = "female";
enddefine;
mary =>

define :method birthday(p:person);
    lvars age = person_age(p) + 1;
    age -> person_age(p);
    [Happy birthday ^(person_name(p)) - now aged ^age] =>
enddefine;

person_age(fred) =>

;;; it's his birthday again
birthday(fred);

person_age(fred) =>

/* MISHAP - Method "birthday" failed
birthday("fred");
*/

/* MISHAP - Method "birthday" failed
birthday([^fred ^mary]);
*/

define :class adult is person;      ;;; specify inheritance
    slot person_spouse = false;     ;;; and an additional slot
enddefine;

define :instance adam:adult;
    person_name = "adam";
    person_sex = "male";
    person_age = 33;
enddefine;
adam =>

isperson(adam) =>

person_spouse(adam) =>

birthday(adam);

isadult(adam) =>

isadult(fred) =>

/* MISHAP - Method "person_spouse" failed
person_spouse(fred) =>
*/

define :method marry(p1:adult, p2:adult);
    ;;; check to see if the marriage is legal
    define check_bigamy(p1, p2);
        if person_spouse(p1)            ;;; already married
        and person_spouse(p1) /== p2    ;;; ... to someone else!
        then
            mishap('BIGAMY', [^p1 ^p2]);
        endif
    enddefine;
    check_bigamy(p1, p2);
    check_bigamy(p2, p1);
    ;;; check for same sex
    if person_sex(p1) == person_sex(p2) then
        [hmm - very modern] =>
    endif;
    ;;; marry them
    define take_spouse(p1, p2);
        ;;; take the vows
        [I ^(person_name(p1))
            take thee ^(person_name(p2))
            to be my lawfully wedded
            other] =>
        ;;; update the spouse slot
        p2 -> person_spouse(p1);
    enddefine;
    take_spouse(p1, p2);
    take_spouse(p2, p1);
enddefine;  /* marry */

define :instance eve:adult;
    person_name = "eve";
    person_sex = "female";
    person_age = 35;
enddefine;

marry(adam, eve);

person_name(person_spouse(adam)) =>

person_name(person_spouse(eve)) =>

person_spouse(person_spouse(adam)) == adam =>

/* MISHAP - BIGAMY
marry(adam,
    instance adult
        person_name = "jezebel";
        person_sex  = "female";
    endinstance);
*/

/* MISHAP - Method "marry" failed
marry(fred, mary);
*/

adam =>

define :method print_instance(a:adult);
    printf('<adult name:%P sex:%P age:%P', [%
        person_name(a),
        person_sex(a),
        person_age(a),
    %]);
    if person_spouse(a) then
        printf(' spouse:%P', [%
            person_name(person_spouse(a))
        %]);
    endif;
    printf('>');
enddefine;

adam =>

newadult() =>

define :class professor is adult;
    slot telephone_number;
    slot discipline;
enddefine;

define :instance roger:professor;
    person_name = "penrose";
enddefine;

telephone_number(roger) =>

discipline(roger) =>

roger =>

define :method print_instance(p:professor);
    printf('<professor %P>', [% person_name(p) %]);
enddefine;

roger =>

define :class subject;
    slot subject_name = 'undecided';
enddefine;

define :method write_paper(p:professor, s:subject);
    [^(person_name(p))
        is writing a paper on ^(subject_name(s))] =>
    /* now insert code for a professor to write a paper */
enddefine;

define :instance vars rel:subject;
    subject_name = "relativity";
enddefine;
rel =>

write_paper(roger, rel);

define :method write_paper(p:professor, s:professor);
    [^(person_name(p))
        is writing a paper criticising ^(person_name(s))] =>
    /* now insert code for a professor to write a paper */
enddefine;

write_paper(roger,
    instance professor
        person_name = "jones"
    endinstance);

define :instance marvin:professor;
    person_name = "marvin";
    person_sex = "male";
    person_age = 53;
    telephone_number = '(691) 455 554';
    discipline = "computers_and_philosophy";
enddefine;
marvin =>

discipline(marvin) =>

marry(marvin,
    instance adult
        person_name = "nina";
        person_sex = "female";
    endinstance);

birthday(marvin);

person_age(marvin) =>

write_paper(marvin, conssubject("ai"));

write_paper(marvin, roger);

define :method birthday(prof:professor);
    ;;; a professor's birthday is like anybody else's birthday
    call_next_method(prof);
    ;;; ...but with the possibility of retirement
    lvars age = person_age(prof);
    if age >= 65 then
        [^(person_name(prof))
         is now retired] =>
    elseif 65 - age < 5 then
        [^(person_name(prof))
         has only ^(65 - age) years before retiring] =>
    endif;
enddefine;

define :instance albert:professor;
    person_age = 63;
    person_name = "einstein";
    discipline = "relativity";
enddefine;
albert =>

birthday(albert);

birthday(albert);

define :instance vars margaret:adult;
    person_name = "margaret";
    person_age = 63
enddefine;

birthday(margaret);

birthday(margaret);

define :method write_paper(p:professor, title);
    [^(person_name(p))
     is writing a paper called ^title] =>
    /* now insert code for a professor to write a paper */
enddefine;

write_paper(marvin, 'The Future of AI');

write_paper(marvin, roger);

write_paper(marvin, 0);

person_age(adam) =>         ;;; access the slot

35 -> person_age(adam);     ;;; update the slot
person_age(adam) =>

define :method years_to_retirement(prof:professor) -> years;
    ;;; base method computes how many years a professor has left
    ;;; before retirement
    max(65 - person_age(prof), 0) -> years;
enddefine;

define :method updaterof years_to_retirement(years, prof:professor);
    ;;; the updater does the inverse: given the number of years
    ;;; before retirement, we can deduce the professor's age
    65 - years -> person_age(prof);
enddefine;

years_to_retirement(albert) =>

years_to_retirement(marvin) =>

;;; we may not know roger's age...
person_age(roger) =>

;;; but we can work it out if we know when he'll retire
12 -> years_to_retirement(roger);
person_age(roger) =>

person_age(
    instance professor
        years_to_retirement = 6;
    endinstance) =>

;;; create a property for holding phone numbers
define telephone_book =
    newassoc([]);
enddefine;

;;; ordinary mortals have their telephone numbers in the book
define :method telephone_number(p:adult);
    telephone_book(p);
enddefine;

define :method updaterof telephone_number(n, p:adult);
    n -> telephone_book(p);
enddefine;

telephone_number(marvin) =>

telephone_number(adam) =>

'(0171) 232 3000' -> telephone_number(adam);
telephone_number(adam) =>

define :class student is adult;
    slot student_course;
    ;;; print a message for each new student
    on new(student) do
        printf('One more student to teach!\n');
enddefine;

newstudent() =>

define :wrapper updaterof student_course(course, s:student, slot_p);
    slot_p(course, s);
    printf('One more student enrolled for %P\n', [^course]);
enddefine;

define :instance bill:student;
    person_name = "bill";
    person_age = 19;
    student_course = "csai";
enddefine;
