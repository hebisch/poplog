/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/data/grammar1.p
 *  Purpose:        Data for use with LIB * GRAMMAR
 *  Author:         Aaron Sloman, Jan 1983
 *  Documentation:  HELP * GRAMMAR
 *  Related Files:  LIB * GRAMMAR, *LEXICON, *GRAMMAR2
 */

;;; for use with LIB GRAMMAR                        A. Sloman Jan 1982
section;

global vars grammar1 ;

;;; **********THE FIRST GRAMMAR*********
[
;;; sentence formats
[s
    [np vp]
    ]
;;; noun phrases
[np
    [pn]
    [snp]
    [snp pp]
    ]
;;; simple noun phrase
[snp
    [det qn]
    ]
;;; qualified noun
[qn
    [noun]
    [adj qn]
    ]
;;; prepositional phrases
[pp
    [prep np]
    ]
;;; verb phrases
[vp
    [vatnp at np]
    [vtonp to np]
    [vfornp  for np]
    [vonnp  on np]
    [vovernp over np]
    [vupnp up np]
    [vupnp np up]
    [vnptonp np to np]
    [vnpatnp np at np]
    [vnpbynp np by np]
    [vnpfromnp np from np]
    [vnpfornp np for np]
    [vnplocnp np locprep np]
    [vnp np]
    [v]
    ]
] -> grammar1;

endsection;
