/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/gamma.p
 > Purpose:         Gamma function for complex arguments
 > Author:          David Young, Jul  9 1987 (see revisions)
 > Documentation:   HELP *GAMMA
 */

compile_mode :pop11 +strict;

section;

global constant procedure (gamma, gammaln);

define gammaln(xx) -> result;
    lvars xx, result;
    ;;; Returns an approximation to the logarithm of the gamma function,
    ;;; using the algorithm given in Numerical Recipes in C by Press et al.
    lconstant cof = {  76.18009173     -86.50532033   24.01409822
        -1.231739516   0.120858003e-2  -0.536382e-5},
        logpi = log(pi);
    lvars j, x, tmp, ser, flipped = false;

    dlocal popradians = true;       ;;; for sin function below

    if realpart(xx) < 0.5 then
        ;;; get into accurate half-plane
        1 - xx -> xx;
        true -> flipped
    endif;

    ;;; Main calculation
    xx - 1.0 -> x;
    x + 5.5 -> tmp;
    tmp - (x + 0.5) * log(tmp) -> tmp;
    1.0 -> ser;

    fast_for j from 1 to 6 do
        x + 1.0 -> x;
        ser + cof(j)/x -> ser
    endfor;

    -tmp + log(2.50662827465 * ser) -> result;

    if flipped then
        ;;; apply reflection formula
        logpi - result - log(sin(pi * xx)) -> result;
    endif;
enddefine;

define gamma(/* x */) /* -> result */ with_nargs 1;
    exp(gammaln())
enddefine;

define gammainv(/* x */) /* -> result */ with_nargs 1;
    exp(-gammaln())
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- David S Young, Apr 15 1993
        Completely revised to use the algorithm from Numerical Recipes in C
 */
