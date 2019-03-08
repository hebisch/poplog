/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:            C.all/extern/unicode/encoding_ucs_2.c
 * Purpose:         Unicode <-> UCS-2
 * Author:          Robert John Duncan, Feb 27 1997
 * Documentation:
 * Related Files:
 */

/*
 *
 *  UCS-2
 *  =====
 *  Two-byte encoding for Unicode, with a byte-order mark prefix to
 *  distinguish big/little endian
 *
 *  State:
 *      0   initial call
 *      1   natural byte order
 *      2   reverse byte order
 *
 */

#include "encoding.h"

static UNICODE_TO_X unicode_to_ucs_2;
static X_TO_UNICODE ucs_2_to_unicode;

CODING_PAIR encoding_ucs_2 = { unicode_to_ucs_2, ucs_2_to_unicode };

static void
unicode_to_ucs_2(
    const CHAR16*   input,
    unsigned*       inputLength,
    CHAR8*          output,
    unsigned*       outputLength,
    unsigned*       state,
    unsigned        flags
) {
    unsigned ilen, olen;

    olen = *outputLength;
    if (olen <= 1) {
        /* insufficient output */
        *outputLength = 0;
        return;
    }

    if (*state == 0) {
        /* first call: output byte-order mark */
        static const CHAR16 bom[] = {U_BYTE_ORDER_MARK};
        output[0] = ((CHAR8*)bom)[0];
        output[1] = ((CHAR8*)bom)[1];
        *state = 1;
        output += 2, olen -= 2;
    }

    ilen = *inputLength;
    if (flags & CHAR8_INPUT) {
        /* input is 8-bit */
        CHAR16 c[1];
        const CHAR8* charInput = (const CHAR8*)input;
        while (ilen > 0 && olen > 1) {
            c[0] = charInput[0];
            output[0] = ((CHAR8*)c)[0];
            output[1] = ((CHAR8*)c)[1];
            ++charInput, --ilen, output += 2, olen -= 2;
            if (c[0] == '\n' && (flags & STOP_AT_NEWLINE)) break;
        }
    }
    else
        while (ilen > 0 && olen > 1) {
            output[0] = ((CHAR8*)input)[0];
            output[1] = ((CHAR8*)input)[1];
            ++input, --ilen, output += 2, olen -= 2;
            if (input[-1] == '\n' && (flags & STOP_AT_NEWLINE)) break;
        }

    *inputLength = ilen;    /* no. chars not copied */
    *outputLength -= olen;  /* no. bytes copied */

}

static void
ucs_2_to_unicode(
    const CHAR8*    input,
    unsigned*       inputLength,
    CHAR16*         output,
    unsigned*       outputLength,
    unsigned*       state,
    unsigned        flags
) {
    unsigned ilen, olen;

    ilen = *inputLength;
    if (ilen <= 1) {
        /* insufficient input */
        *outputLength = 0;
        return;
    }

    if (*state == 0) {
        /* first call: check for byte-order mark */
        CHAR16 bom[1];
        ((CHAR8*)bom)[0] = input[0];
        ((CHAR8*)bom)[1] = input[1];
        if (bom[0] == U_REVERSE_BYTE_ORDER_MARK) {
            /* reverse byte order */
            *state = 2;
            input += 2, ilen -= 2;
        }
        else {
            /* assume natural byte order; skip the b.o.m. if present */
            *state = 1;
            if (bom[0] == U_BYTE_ORDER_MARK)
                input += 2, ilen -= 2;
        }
    }

    olen = *outputLength;
    if (*state == 2)
        /* reverse byte order */
        while (ilen > 1 && olen > 0) {
            ((CHAR8*)output)[0] = input[1];
            ((CHAR8*)output)[1] = input[0];
            input += 2, ilen -= 2, ++output, --olen;
            if (output[-1] == '\n' && (flags & STOP_AT_NEWLINE)) break;
        }
    else
        /* natural byte order */
        while (ilen > 1 && olen > 0) {
            ((CHAR8*)output)[0] = input[0];
            ((CHAR8*)output)[1] = input[1];
            input += 2, ilen -= 2; ++output, --olen;
            if (output[-1] == '\n' && (flags & STOP_AT_NEWLINE)) break;
        }

    if (ilen == 1 && olen > 0 && !(flags & INPUT_INCOMPLETE)) {
        /* untranslatable single byte at end of input */
        output[0] = U_REPLACEMENT_CHARACTER;
        ilen = 0, --olen;
    }

    *inputLength = ilen;    /* no. bytes not copied */
    *outputLength -= olen;  /* no. characters copied */

}
