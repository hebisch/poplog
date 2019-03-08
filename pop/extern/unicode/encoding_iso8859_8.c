/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:            C.all/extern/unicode/encoding_iso8859_8.c
 * Purpose:         Unicode <-> ISO 8859-8
 * Author:          Robert Duncan, Nov  9 1998
 */

/*
 *
 *  ISO 8859-8
 *  ==========
 *  Hebrew
 *
 */

#include "encoding.h"
#include "iso8859_8_tables.h"

static UNICODE_TO_X unicode_to_iso8859_8;
static X_TO_UNICODE iso8859_8_to_unicode;

CODING_PAIR encoding_iso8859_8 = { unicode_to_iso8859_8, iso8859_8_to_unicode };

static void
unicode_to_iso8859_8(
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
        /* input is 8-bit */
        const CHAR8* charInput = (CHAR8*)input;
        if (flags & STOP_AT_NEWLINE) {
            while (i < ilen) {
                CHAR8 cin = charInput[i];
                output[i] = encode_iso8859_8[cin];
                ++i;
                if (cin == '\n') break;
            }
        }
        else {
            while (i < ilen) {
                output[i] = encode_iso8859_8[charInput[i]];
                ++i;
            }
        }
    }
    else {
        while (i < ilen) {
            CHAR16 cin = input[i];
            if (cin <= 0x7F) {
                /* ASCII */
                output[i] = cin;
                ++i;
                if (cin == '\n' && (flags & STOP_AT_NEWLINE)) break;
            }
            else if (cin >= 0x05D0 && cin <= 0x5EA) {
                /* Hebrew */
                output[i] = cin - 0x4F0;
                ++i;
            }
            else if (cin <= 0xff) {
                output[i] = encode_iso8859_8[cin];
                ++i;
            }
            else if (cin == 0x2017) {
                output[i] = 0xDF;
                ++i;
            }
            else if (cin == 0x203E) {
                output[i] = 0xAF;
                ++i;
            }
            else {
                output[i] = 0x3F;
                ++i;
            }
        }
    }

    *inputLength -= i;
    *outputLength = i;
}

static void
iso8859_8_to_unicode(
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
            output[i] = decode_iso8859_8[cin];
            ++i;
            if (cin == '\n') break;
        }
    }
    else {
        while (i < ilen) {
            output[i] = decode_iso8859_8[input[i]];
            ++i;
        }
    }

    *inputLength -= i;
    *outputLength = i;
}
