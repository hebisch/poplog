/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/data/lexicon.p
 *  Purpose:        Lexicon for use with LIB * GRAMMAR
 *  Author:         Aaron Sloman, Jan 1982
 *  Documentation:  TEACH * GRAMMAR
 *  Related Files:  LIB * GRAMMAR, *GRAMMAR1, *GRAMMAR2
 */

section;

global vars lexicon;
[
;;; copula
[cop is was were]
;;; determiners
[det a the another every each some her]
;;; adjectives
[adj big sad happy bald small blue old young clean new first third long short]
;;; proper names and pronouns
[pn steve she you i I me we they he him it her them us julie john mary jon
    gerald judith]
;;; common nouns
[noun car door man president girl room tree chair table cat dog]
;;; prepositions
[prep on in by from to for with under below above inside into of
    up upon within outside beside]
;;; locative prepositions
[locprep on upon in under below above inside within outside beside into]
;;; transitive verbs
[vnp owned liked loved hated bought hit shot sold
    solved reversed used joined]
;;; transitive verbs followed by from
[vnpfromnp fetched copied borrowed stole took lifted bought removed ]
;;; transitive verbs followed by for
[vnpfornp got brought wrote made bought]
;;; transitive verbs followed by to
[vnptonp gave reported sent sold joined]
[vnpbynp grabbed pulled lifted]
;;; intransitive verbs
[v smiled laughed wrote danced fell jumped sang]
;;; intransitive verbs followed by at
[vatnp smiled laughed looked shot winked gazed grinned]
;;; intransitive verbs followed by over
[vovernp jumped fell wept stood]
;;;  --- followed by "on"
[vonnp  jumped fell climbed]
;;; --- followed by "to"
[vtonp went walked crawled ran jumped talked belonged]
[vfornp looked searched sent]
[vupnp picked rang lifted looked]
[vnpup picked rang lifted looked]
;;; transitive verbs followed by a locative preposition
[vnplocnp put inserted stored]
[vnpatnp threw shot]
;;; relative pronouns
[rel that who which]
;;; propositional verbs
[pv thought believed hoped feared]
[adv quickly happily slowly stupidly cleverly boldly quietly]
[sadv probably certainly maybe suppose]
;;; conjunctions
[conj and or because but while although]
]-> lexicon;

endsection;
