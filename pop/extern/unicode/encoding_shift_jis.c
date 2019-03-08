/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:            C.all/extern/unicode/encoding_shift_jis.c
 * Purpose:         Unicode <-> Shift-jis
 * Author:          Robert John Duncan, Feb 27 1997
 * Documentation:
 * Related Files:
 */

/*
 *
 *  Shift-JIS
 *  =========
 *  Tables derived from Microsoft Windows CodePage 932
 *
 */

#include "encoding.h"
#include "shift_jis_tables.h"

static UNICODE_TO_X unicode_to_shift_jis;
static X_TO_UNICODE shift_jis_to_unicode;

CODING_PAIR encoding_shift_jis = { unicode_to_shift_jis, shift_jis_to_unicode };

static void
unicode_to_shift_jis(
    const CHAR16*   input,
    unsigned*       inputLength,
    CHAR8*          output,
    unsigned*       outputLength,
    unsigned*       state,
    unsigned        flags
) {
    unsigned ilen = *inputLength;
    unsigned olen = *outputLength;

    /* set output bytes to value c */
#define SET_OUTPUT(c)                                       \
    if (c <= 0xFF) {                                        \
        /* one-byte */                                      \
        output[0] = (CHAR8)c;                               \
        ++output, --olen;                                   \
    }                                                       \
    else {                                                  \
        /* two-byte */                                      \
        if (olen == 1) break;                               \
        output[0] = (CHAR8)(c >> 8);                        \
        output[1] = (CHAR8)(c & 0xFF);                      \
        output += 2, olen -= 2;                             \
    }

    if (flags & CHAR8_INPUT) {
        /* input is 8-bit */
        const CHAR8* charInput = (CHAR8*)input;
        while (ilen > 0 && olen > 0) {
            CHAR16 cin = charInput[0];
            CHAR16 cout = encode[0][cin];
            SET_OUTPUT(cout)
            ++charInput, --ilen;
            if (cin == '\n' && (flags & STOP_AT_NEWLINE)) break;
        }
    }
    else
        while (ilen > 0 && olen > 0) {
            CHAR16 cin = input[0];
            CHAR16 cout = encode[cin >> 8][cin & 0xFF];
            SET_OUTPUT(cout)
            ++input, --ilen;
            if (cin == '\n' && (flags & STOP_AT_NEWLINE)) break;
        }

    *inputLength = ilen;
    *outputLength -= olen;
}

static void
shift_jis_to_unicode(
    const CHAR8*    input,
    unsigned*       inputLength,
    CHAR16*         output,
    unsigned*       outputLength,
    unsigned*       state,
    unsigned        flags
) {
    unsigned ilen = *inputLength;
    unsigned olen = *outputLength;

    while (ilen > 0 && olen > 0) {
        CHAR16 c = decode_0[input[0]];
        if (c != U_SENTINEL) {
            /* one-byte character, or something unrecognised */
            ++input, --ilen;
        }
        else if (ilen > 1) {
            /* two-byte character, both available */
            c = decode_1[input[0]][input[1]];
            input += 2, ilen -= 2;
        }
        else if (flags & INPUT_INCOMPLETE)
            /* lead byte with next to follow */
            break;
        else {
            /* lead byte with nothing to follow */
            c = U_REPLACEMENT_CHARACTER;
            ilen = 0;
        }
        output[0] = c;
        ++output, --olen;
        if (c == '\n' && (flags & STOP_AT_NEWLINE)) break;
    }

    *inputLength = ilen;
    *outputLength -= olen;
}
