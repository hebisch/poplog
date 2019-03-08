/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 * File:            C.all/extern/unicode/encoding_iso8859_1.c
 * Purpose:         Unicode <-> ISO 8859-1
 * Author:          Robert Duncan, Aug  7 1997 (see revisions)
 */

/*
 *
 *  ISO 8859-1
 *  ==========
 *  The lower 8 bits of Unicode
 *
 */

#include "encoding.h"

static UNICODE_TO_X unicode_to_iso8859_1;
static X_TO_UNICODE iso8859_1_to_unicode;

CODING_PAIR encoding_iso8859_1 = { unicode_to_iso8859_1, iso8859_1_to_unicode };

static void
unicode_to_iso8859_1(
    const CHAR16*   input,
    unsigned*       inputLength,
    CHAR8*          output,
    unsigned*       outputLength,
    unsigned*       state,
    unsigned        flags
) {
    unsigned ilen = *inputLength;
    unsigned olen = *outputLength;
    unsigned i = 0;

    if (ilen > olen) ilen = olen;

    if (flags & CHAR8_INPUT) {
        /* input is 8-bit -- needs nothing doing */
        const CHAR8* charInput = (CHAR8*)input;
        if (flags & STOP_AT_NEWLINE) {
            while (i < ilen) {
                CHAR8 cin = charInput[i];
                output[i] = cin;
                ++i;
                if (cin == '\n') break;
            }
        }
        else {
            while (i < ilen) {
                output[i] = charInput[i];
                ++i;
            }
        }
    }
    else {
        /* replace 16-bit chars with '?' */
        while (i < ilen) {
            CHAR16 cin = input[i];
            if (cin > 0xff) {
                output[i] = '?';
                ++i;
            }
            else {
                output[i] = (CHAR8)cin;
                ++i;
                if (cin == '\n' && (flags & STOP_AT_NEWLINE)) break;
            }
        }
    }

    *inputLength -= i;
    *outputLength = i;
}

static void
iso8859_1_to_unicode(
    const CHAR8*    input,
    unsigned*       inputLength,
    CHAR16*         output,
    unsigned*       outputLength,
    unsigned*       state,
    unsigned        flags
) {
    unsigned ilen = *inputLength;
    unsigned olen = *outputLength;
    unsigned i = 0;

    if (ilen > olen) ilen = olen;

    /* extend 8-bit to 16-bit */
    if (flags & STOP_AT_NEWLINE) {
        while (i < ilen) {
            CHAR8 cin = input[i];
            output[i] = cin;
            ++i;
            if (cin == '\n') break;
        }
    }
    else {
        while (i < ilen) {
            output[i] = input[i];
            ++i;
        }
    }

    *inputLength -= i;
    *outputLength = i;
}


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Nov 11 1998
        Added newline check to the decoder
 */
