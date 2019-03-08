/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:            C.all/extern/unicode/encoding_utf_8.c
 * Purpose:         Unicode <-> UTF-8
 * Author:          Robert Duncan, Apr 18 1997
 * Documentation:
 * Related Files:
 */

/*
 *
 *  UTF-8
 *  =====
 *  Based on the Annex to ISO 10646 but limited to UCS-2 and with improper
 *  treatment of surrogates.
 *
 */

#include "encoding.h"

static UNICODE_TO_X unicode_to_utf_8;
static X_TO_UNICODE utf_8_to_unicode;

CODING_PAIR encoding_utf_8 = { unicode_to_utf_8, utf_8_to_unicode };

static void
unicode_to_utf_8(
    const CHAR16*   input,
    unsigned*       inputLength,
    CHAR8*          output,
    unsigned*       outputLength,
    unsigned*       state,
    unsigned        flags
) {
    unsigned ilen = *inputLength;
    unsigned olen = *outputLength;

    if (flags & CHAR8_INPUT) {
        /* input is 8-bit */
        const CHAR8* charInput = (CHAR8*)input;
        while (ilen > 0 && olen > 0) {
            CHAR8 c = charInput[0];
            if (c < 0x80) {
                /* simple ASCII */
                output[0] = c;
                ++charInput, --ilen, ++output, --olen;
                if (c == '\n' && (flags & STOP_AT_NEWLINE)) break;
            }
            else {
                /* two-bytes needed */
                if (olen == 1) break;
                output[1] = 0x80 | (c & 0x3f);
                output[0] = 0xc0 | (c >> 6);
                ++charInput, --ilen, output += 2, olen -= 2;
            }
        }
    }
    else {
        while (ilen > 0 && olen > 0) {
            CHAR16 c = input[0];
            if (c < 0x80) {
                /* 1-byte ASCII */
                output[0] = (CHAR8)c;
                ++input, --ilen, ++output, --olen;
                if (c == '\n' && (flags & STOP_AT_NEWLINE)) break;
            }
            else if (c < 0x800) {
                /* 2-byte */
                if (olen == 1) break;
                output[1] = 0x80 | (c & 0x3f);
                output[0] = 0xc0 | (c >> 6);
                ++input, --ilen, output += 2, olen -= 2;
            }
            else {
                /* 3-byte */
                if (olen < 3) break;
                output[2] = 0x80 | (c & 0x3f);
                output[1] = 0x80 | ((c >> 6) & 0x3f);
                output[0] = 0xe0 | (c >> 12);
                ++input, --ilen, output += 3, olen -= 3;
            }
        }
    }

    *inputLength = ilen;
    *outputLength -= olen;
}

static void
utf_8_to_unicode(
    const CHAR8*    input,
    unsigned*       inputLength,
    CHAR16*         output,
    unsigned*       outputLength,
    unsigned*       state,
    unsigned        flags
) {
    unsigned ilen = *inputLength;
    unsigned olen = *outputLength;

    /* NB: this assumes the correct input format */
    while (ilen > 0 && olen > 0) {
        CHAR8 c = input[0];
        if (c < 0xc0) {
            output[0] = c;
            ++input, --ilen, ++output, --olen;
            if (c == '\n' && (flags & STOP_AT_NEWLINE)) break;
        }
        else if (c < 0xe0 && ilen >= 2) {
            output[0] = ((c - 0xc0) << 6) | (input[1] - 0x80);
            input += 2, ilen -= 2, ++output, --olen;
        }
        else if (c < 0xf0 && ilen >= 3) {
            output[0] = ((c - 0xe0) << 12) | ((input[1] - 0x80) << 6) |
                            (input[2] - 0x80);
            input += 3, ilen -= 3, ++output, --olen;
        }
        else if (c < 0xf8 && ilen >= 4) {
            output[0] = U_REPLACEMENT_CHARACTER;
            input += 4, ilen -= 4, ++output, --olen;
        }
        else if (c < 0xfc && ilen >= 5) {
            output[0] = U_REPLACEMENT_CHARACTER;
            input += 5, ilen -= 5, ++output, --olen;
        }
        else if (ilen >= 6) {
            output[0] = U_REPLACEMENT_CHARACTER;
            input += 6, ilen -= 6, ++output, --olen;
        }
        else if (flags & INPUT_INCOMPLETE) {
            break;
        }
        else {
            output[0] = U_REPLACEMENT_CHARACTER;
            input += ilen, ilen = 0, ++output, --olen;
        }
    }

    *inputLength = ilen;
    *outputLength -= olen;
}
