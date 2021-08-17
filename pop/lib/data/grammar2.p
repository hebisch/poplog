/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/data/grammar2.p
 *  Purpose:        A more complex grammer for use with LIB * GRAMMAR
 *  Author:         Aaron Sloman, ???
 *  Documentation:  TEACH * GRAMMAR
 *  Related Files:  LIB * GRAMMAR, *GRAMMAR1.
 */

;;; For use with LIB GRAMMAR; Compare GRAMMAR1.P
section;

global vars grammar2;

[
;;; sentence formats
[s
    [if s then s]
    [np vp]
    [sadv np vp]
    [s conj s]
    ]
;;; noun phrases
[np
    [pn]
    [snp]
    [snp pp]
    [snp relp]
    [np and np]
    ]
;;; relative clause phrases
[relp
    [rel vp]
    [rel np vnp]
    ]
;;; predicates
[pred
    [adj]
    [a qn]
    [np]
    ]
;;; prepositional phrases
[ppatnp     [at np]]
[pptonp     [to np]]
[ppfornp    [for np]]
[pponnp     [on np]]
[ppovernp   [over np]]
[ppupnp     [up np]]
[ppnpup     [np up]]
[ppnptonp   [np to np]]
[ppnpatnp   [np at np]]
[ppnpbynp   [np by np]]
[ppnpfromnp [np from np]]
[ppnpfornp  [np for np]]
[ppnplocnp  [np locprep np]]
[pp     [prep np]]
;;; verb phrases
[vp
    [cop pred]
    [vatnp ppatnp]
    [vtonp  pptonp]
    [vfornp  ppfornp]
    [vonnp  pponnp]
    [vovernp  ppovernp]
    [vupnp  ppupnp]
    [vnpup  ppnpup]
    [vnptonp  ppnptonp]
    [vnpatnp  ppnpatnp]
    [vnpbynp  ppnpbynp]
    [vnpfromnp  ppnpfromnp]
    [vnpfornp  ppnpfornp]
    [vnplocnp  ppnplocnp]
    [pv that s]
    [vnp np]
    [v]
    [vp and vp]
    [vp adv]
    [adv vp]
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
] -> grammar2;

endsection;
