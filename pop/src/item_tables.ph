/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/src/item_tables.ph
 > Purpose:         Itemiser character tables
 > Author:          John Gibson, Mar 29 1997 (see revisions)
 */


    /*  Itemiser default 7-bit character table (ASCII / ISO Latin / Unicode)
    */
lconstant iso_lextable_7 = writeable consstring(

;;;         00      01      02      03      04      05      06      07
;;;         Ctrl-@  Ctrl-A  Ctrl-B  Ctrl-C  Ctrl-D  Ctrl-E  Ctrl-F  Ctrl-G
            IT_SP,  IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP,

;;;         08      09      0A      0B      0C      0D      0E      0F
;;;         Ctrl-H  Ctrl-I  Ctrl-J  Ctrl-K  Ctrl-L  Ctrl-M  Ctrl-N  Ctrl-O
            IT_SEP, IT_SP,  IT_SP,  IT_SEP, IT_SP,  IT_SP,  IT_SEP, IT_SEP,

;;;         10      11      12      13      14      15      16      17
;;;         Ctrl-P  Ctrl-Q  Ctrl-R  Ctrl-S  Ctrl-T  Ctrl-U  Ctrl-V  Ctrl-W
            IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP,

;;;         18      19      1A      1B      1C      1D      1E      1F
;;;         Ctrl-X  Ctrl-Y  Ctrl-Z  Ctrl-[  Ctrl-\  Ctrl-]  Ctrl-^  Ctrl-_
            IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP,

;;;         20      21      22      23      24      25      26      27
;;;         \s      !       "       #       $       %       &       '
            IT_SP,  IT_SGN, IT_SEP, IT_SGN, IT_SGN, IT_SEP, IT_SGN, IT_STR,

;;;         28      29      2A      2B      2C      2D      2E      2F
;;;         (       )       *       +       ,       -       .       /
            IT_SEP, IT_SEP, IT_BC2, IT_SGN, IT_SEP, IT_SGN, IT_SEP, IT_BC1,

;;;         30      31      32      33      34      35      36      37
;;;         0       1       2       3       4       5       6       7
            IT_DIG, IT_DIG, IT_DIG, IT_DIG, IT_DIG, IT_DIG, IT_DIG, IT_DIG,

;;;         38      39      3A      3B      3C      3D      3E      3F
;;;         8       9       :       ;       <       =       >       ?
            IT_DIG, IT_DIG, IT_SGN, IT_SCN, IT_SGN, IT_SGN, IT_SGN, IT_SGN,

;;;         40      41      42      43      44      45      46      47
;;;         @       A       B       C       D       E       F       G
            IT_SGN, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET,

;;;         48      49      4A      4B      4C      4D      4E      4F
;;;         H       I       J       K       L       M       N       O
            IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET,

;;;         50      51      52      53      54      55      56      57
;;;         P       Q       R       S       T       U       V       W
            IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET,

;;;         58      59      5A      5B      5C      5D      5E      5F
;;;         X       Y       Z       [       \       ]       ^       _
            IT_LET, IT_LET, IT_LET, IT_SEP, IT_SGN, IT_SEP, IT_SGN, IT_UND,

;;;         60      61      62      63      64      65      66      67
;;;         `       a       b       c       d       e       f       g
            IT_CON, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET,

;;;         68      69      6A      6B      6C      6D      6E      6F
;;;         h       i       j       k       l       m       n       o
            IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET,

;;;         70      71      72      73      74      75      76      77
;;;         p       q       r       s       t       u       v       w
            IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET,

;;;         78      79      7A      7B      7C      7D      7E      7F
;;;         x       y       z       {       |       }       ~       Ctrl-?
            IT_LET, IT_LET, IT_LET, IT_SEP, IT_SGN, IT_SEP, IT_SGN, IT_SEP,

128);

    /*  Itemiser default 8-bit character table (ISO Latin 1 / Unicode)
    */
lconstant iso_lextable_8 = writeable consstring(

;;;         80      81      82      83      84      85      86      87
;;;                 - le    - re    -       | be    -       -       -
            IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP,

;;;         88      89      8A      8B      8C      8D      8E      8F
;;;         | te    -       -       -       |       |       |       +
            IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP,

;;;         90      91      92      93      94      95      96      97
;;;         o       #       .
            IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SEP,

;;;         98      99      9A      9B      9C      9D      9E      9F
;;;                                         \Sf     \Ss     \St     \Sp
            IT_SEP, IT_SEP, IT_SEP, IT_SEP, IT_SP,  IT_SP,  IT_SP,  IT_SP,

;;;         A0      A1      A2      A3      A4      A5      A6      A7
;;;         nb sp   ! down  cent    pound   crncy   yen     brkbar  sectn
            IT_SP,  IT_SEP, IT_SGN, IT_SGN, IT_SGN, IT_SGN, IT_SGN, IT_SGN,

;;;         A8      A9      AA      AB      AC      AD      AE      AF
;;;         diers   cpyrt   ordfem  <<      lognot  hyphen  reg     macron
            IT_SEP, IT_SGN, IT_LET, IT_SEP, IT_SGN, IT_UND, IT_SGN, IT_SEP,

;;;         B0      B1      B2      B3      B4      B5      B6      B7
;;;         degree  +-      2 sup   3 sup   acute   mu      para    dot
            IT_SGN, IT_SGN, IT_EXT, IT_EXT, IT_SEP, IT_LET, IT_SGN, IT_EXT,

;;;         B8      B9      BA      BB      BC      BD      BE      BF
;;;         cedil   1 sup   ordmas  >>      1/4     1/2     3/4     ? down
            IT_SEP, IT_EXT, IT_LET, IT_SEP, IT_FRC, IT_FRC, IT_FRC, IT_SEP,

;;;         C0      C1      C2      C3      C4      C5      C6      C7
;;;         Agrave  Aacute  Acflex  Atilde  Adiers  Aring   AE      Ccedil
            IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET,

;;;         C8      C9      CA      CB      CC      CD      CE      CF
;;;         Egrave  Eacute  Ecflex  Ediers  Igrave  Iacute  Icflex  Idiers
            IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET,

;;;         D0      D1      D2      D3      D4      D5      D6      D7
;;;         Eth     Ntilde  Ograve  Oacute  Ocflex  Otilde  Odiers  mult
            IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_SGN,

;;;         D8      D9      DA      DB      DC      DD      DE      DF
;;;         Oslash  Ugrave  Uacute  Ucflex  Udiers  Yacute  Thorn   gmdbls
            IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET,

;;;         E0      E1      E2      E3      E4      E5      E6      E7
;;;         agrave  aacute  acflex  atilde  adiers  aring   ae      ccedil
            IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET,

;;;         E8      E9      EA      EB      EC      ED      EE      EF
;;;         egrave  eacute  ecflex  ediers  igrave  iacute  icflex  idiers
            IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET,

;;;         F0      F1      F2      F3      F4      F5      F6      F7
;;;         eth     ntilde  ograve  oacute  ocflex  otilde  odiers  divide
            IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_SGN,

;;;         F8      F9      FA      FB      FC      FD      FE      FF
;;;         oslash  ugrave  uacute  ucflex  udiers  yacute  thorn   ydiers
            IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET, IT_LET,

128);


    /*  Itemiser default 16-bit tables (Unicode).
        unichar_vec is a 255-element vector whose elements are integers
        or 128-byte strings constructed with cons_it_string (in which
        each byte encodes two 4-bit compact character types).
    */

cons_it_string(
;;; 0100    0101    0102    0103    0104    0105    0106    0107    0108    0109    010A    010B    010C    010D    010E    010F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0110    0111    0112    0113    0114    0115    0116    0117    0118    0119    011A    011B    011C    011D    011E    011F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0120    0121    0122    0123    0124    0125    0126    0127    0128    0129    012A    012B    012C    012D    012E    012F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0130    0131    0132    0133    0134    0135    0136    0137    0138    0139    013A    013B    013C    013D    013E    013F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0140    0141    0142    0143    0144    0145    0146    0147    0148    0149    014A    014B    014C    014D    014E    014F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0150    0151    0152    0153    0154    0155    0156    0157    0158    0159    015A    015B    015C    015D    015E    015F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0160    0161    0162    0163    0164    0165    0166    0167    0168    0169    016A    016B    016C    016D    016E    016F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0170    0171    0172    0173    0174    0175    0176    0177    0178    0179    017A    017B    017C    017D    017E    017F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0180    0181    0182    0183    0184    0185    0186    0187    0188    0189    018A    018B    018C    018D    018E    018F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0190    0191    0192    0193    0194    0195    0196    0197    0198    0199    019A    019B    019C    019D    019E    019F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 01A0    01A1    01A2    01A3    01A4    01A5    01A6    01A7    01A8    01A9    01AA    01AB    01AC    01AD    01AE    01AF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 01B0    01B1    01B2    01B3    01B4    01B5    01B6    01B7    01B8    01B9    01BA    01BB    01BC    01BD    01BE    01BF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 01C0    01C1    01C2    01C3    01C4    01C5    01C6    01C7    01C8    01C9    01CA    01CB    01CC    01CD    01CE    01CF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 01D0    01D1    01D2    01D3    01D4    01D5    01D6    01D7    01D8    01D9    01DA    01DB    01DC    01DD    01DE    01DF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 01E0    01E1    01E2    01E3    01E4    01E5    01E6    01E7    01E8    01E9    01EA    01EB    01EC    01ED    01EE    01EF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 01F0    01F1    01F2    01F3    01F4    01F5    01F6    01F7    01F8    01F9    01FA    01FB    01FC    01FD    01FE    01FF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
);

cons_it_string(
;;; 0200    0201    0202    0203    0204    0205    0206    0207    0208    0209    020A    020B    020C    020D    020E    020F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0210    0211    0212    0213    0214    0215    0216    0217    0218    0219    021A    021B    021C    021D    021E    021F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0220    0221    0222    0223    0224    0225    0226    0227    0228    0229    022A    022B    022C    022D    022E    022F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0230    0231    0232    0233    0234    0235    0236    0237    0238    0239    023A    023B    023C    023D    023E    023F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0240    0241    0242    0243    0244    0245    0246    0247    0248    0249    024A    024B    024C    024D    024E    024F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0250    0251    0252    0253    0254    0255    0256    0257    0258    0259    025A    025B    025C    025D    025E    025F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0260    0261    0262    0263    0264    0265    0266    0267    0268    0269    026A    026B    026C    026D    026E    026F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0270    0271    0272    0273    0274    0275    0276    0277    0278    0279    027A    027B    027C    027D    027E    027F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0280    0281    0282    0283    0284    0285    0286    0287    0288    0289    028A    028B    028C    028D    028E    028F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0290    0291    0292    0293    0294    0295    0296    0297    0298    0299    029A    029B    029C    029D    029E    029F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 02A0    02A1    02A2    02A3    02A4    02A5    02A6    02A7    02A8    02A9    02AA    02AB    02AC    02AD    02AE    02AF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 02B0    02B1    02B2    02B3    02B4    02B5    02B6    02B7    02B8    02B9    02BA    02BB    02BC    02BD    02BE    02BF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_EXT, it_EXT, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 02C0    02C1    02C2    02C3    02C4    02C5    02C6    02C7    02C8    02C9    02CA    02CB    02CC    02CD    02CE    02CF
    it_LET, it_LET, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT,
;;; 02D0    02D1    02D2    02D3    02D4    02D5    02D6    02D7    02D8    02D9    02DA    02DB    02DC    02DD    02DE    02DF
    it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_NTC,
;;; 02E0    02E1    02E2    02E3    02E4    02E5    02E6    02E7    02E8    02E9    02EA    02EB    02EC    02ED    02EE    02EF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 02F0    02F1    02F2    02F3    02F4    02F5    02F6    02F7    02F8    02F9    02FA    02FB    02FC    02FD    02FE    02FF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 0300    0301    0302    0303    0304    0305    0306    0307    0308    0309    030A    030B    030C    030D    030E    030F
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB,
;;; 0310    0311    0312    0313    0314    0315    0316    0317    0318    0319    031A    031B    031C    031D    031E    031F
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB,
;;; 0320    0321    0322    0323    0324    0325    0326    0327    0328    0329    032A    032B    032C    032D    032E    032F
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB,
;;; 0330    0331    0332    0333    0334    0335    0336    0337    0338    0339    033A    033B    033C    033D    033E    033F
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB,
;;; 0340    0341    0342    0343    0344    0345    0346    0347    0348    0349    034A    034B    034C    034D    034E    034F
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0350    0351    0352    0353    0354    0355    0356    0357    0358    0359    035A    035B    035C    035D    035E    035F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0360    0361    0362    0363    0364    0365    0366    0367    0368    0369    036A    036B    036C    036D    036E    036F
    it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0370    0371    0372    0373    0374    0375    0376    0377    0378    0379    037A    037B    037C    037D    037E    037F
    it_NTC, it_NTC, it_NTC, it_NTC, it_SEP, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_LET, it_NTC, it_NTC, it_NTC, it_SEP, it_NTC,
;;; 0380    0381    0382    0383    0384    0385    0386    0387    0388    0389    038A    038B    038C    038D    038E    038F
    it_NTC, it_NTC, it_NTC, it_NTC, it_SEP, it_SEP, it_LET, it_EXT, it_LET, it_LET, it_LET, it_NTC, it_LET, it_NTC, it_LET, it_LET,
;;; 0390    0391    0392    0393    0394    0395    0396    0397    0398    0399    039A    039B    039C    039D    039E    039F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 03A0    03A1    03A2    03A3    03A4    03A5    03A6    03A7    03A8    03A9    03AA    03AB    03AC    03AD    03AE    03AF
    it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 03B0    03B1    03B2    03B3    03B4    03B5    03B6    03B7    03B8    03B9    03BA    03BB    03BC    03BD    03BE    03BF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 03C0    03C1    03C2    03C3    03C4    03C5    03C6    03C7    03C8    03C9    03CA    03CB    03CC    03CD    03CE    03CF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC,
;;; 03D0    03D1    03D2    03D3    03D4    03D5    03D6    03D7    03D8    03D9    03DA    03DB    03DC    03DD    03DE    03DF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_LET, it_NTC, it_LET, it_NTC, it_LET, it_NTC,
;;; 03E0    03E1    03E2    03E3    03E4    03E5    03E6    03E7    03E8    03E9    03EA    03EB    03EC    03ED    03EE    03EF
    it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 03F0    03F1    03F2    03F3    03F4    03F5    03F6    03F7    03F8    03F9    03FA    03FB    03FC    03FD    03FE    03FF
    it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 0400    0401    0402    0403    0404    0405    0406    0407    0408    0409    040A    040B    040C    040D    040E    040F
    it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET,
;;; 0410    0411    0412    0413    0414    0415    0416    0417    0418    0419    041A    041B    041C    041D    041E    041F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0420    0421    0422    0423    0424    0425    0426    0427    0428    0429    042A    042B    042C    042D    042E    042F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0430    0431    0432    0433    0434    0435    0436    0437    0438    0439    043A    043B    043C    043D    043E    043F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0440    0441    0442    0443    0444    0445    0446    0447    0448    0449    044A    044B    044C    044D    044E    044F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0450    0451    0452    0453    0454    0455    0456    0457    0458    0459    045A    045B    045C    045D    045E    045F
    it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET,
;;; 0460    0461    0462    0463    0464    0465    0466    0467    0468    0469    046A    046B    046C    046D    046E    046F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0470    0471    0472    0473    0474    0475    0476    0477    0478    0479    047A    047B    047C    047D    047E    047F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0480    0481    0482    0483    0484    0485    0486    0487    0488    0489    048A    048B    048C    048D    048E    048F
    it_LET, it_LET, it_SGN, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0490    0491    0492    0493    0494    0495    0496    0497    0498    0499    049A    049B    049C    049D    049E    049F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 04A0    04A1    04A2    04A3    04A4    04A5    04A6    04A7    04A8    04A9    04AA    04AB    04AC    04AD    04AE    04AF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 04B0    04B1    04B2    04B3    04B4    04B5    04B6    04B7    04B8    04B9    04BA    04BB    04BC    04BD    04BE    04BF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 04C0    04C1    04C2    04C3    04C4    04C5    04C6    04C7    04C8    04C9    04CA    04CB    04CC    04CD    04CE    04CF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_LET, it_LET, it_NTC, it_NTC, it_LET, it_LET, it_NTC, it_NTC, it_NTC,
;;; 04D0    04D1    04D2    04D3    04D4    04D5    04D6    04D7    04D8    04D9    04DA    04DB    04DC    04DD    04DE    04DF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 04E0    04E1    04E2    04E3    04E4    04E5    04E6    04E7    04E8    04E9    04EA    04EB    04EC    04ED    04EE    04EF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_LET, it_LET,
;;; 04F0    04F1    04F2    04F3    04F4    04F5    04F6    04F7    04F8    04F9    04FA    04FB    04FC    04FD    04FE    04FF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 0500    0501    0502    0503    0504    0505    0506    0507    0508    0509    050A    050B    050C    050D    050E    050F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0510    0511    0512    0513    0514    0515    0516    0517    0518    0519    051A    051B    051C    051D    051E    051F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0520    0521    0522    0523    0524    0525    0526    0527    0528    0529    052A    052B    052C    052D    052E    052F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0530    0531    0532    0533    0534    0535    0536    0537    0538    0539    053A    053B    053C    053D    053E    053F
    it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0540    0541    0542    0543    0544    0545    0546    0547    0548    0549    054A    054B    054C    054D    054E    054F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0550    0551    0552    0553    0554    0555    0556    0557    0558    0559    055A    055B    055C    055D    055E    055F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_LET, it_LET, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP,
;;; 0560    0561    0562    0563    0564    0565    0566    0567    0568    0569    056A    056B    056C    056D    056E    056F
    it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0570    0571    0572    0573    0574    0575    0576    0577    0578    0579    057A    057B    057C    057D    057E    057F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0580    0581    0582    0583    0584    0585    0586    0587    0588    0589    058A    058B    058C    058D    058E    058F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_SEP, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0590    0591    0592    0593    0594    0595    0596    0597    0598    0599    059A    059B    059C    059D    059E    059F
    it_NTC, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB,
;;; 05A0    05A1    05A2    05A3    05A4    05A5    05A6    05A7    05A8    05A9    05AA    05AB    05AC    05AD    05AE    05AF
    it_CMB, it_CMB, it_NTC, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB,
;;; 05B0    05B1    05B2    05B3    05B4    05B5    05B6    05B7    05B8    05B9    05BA    05BB    05BC    05BD    05BE    05BF
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_CMB, it_CMB, it_CMB, it_SEP, it_CMB,
;;; 05C0    05C1    05C2    05C3    05C4    05C5    05C6    05C7    05C8    05C9    05CA    05CB    05CC    05CD    05CE    05CF
    it_SEP, it_CMB, it_CMB, it_SEP, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 05D0    05D1    05D2    05D3    05D4    05D5    05D6    05D7    05D8    05D9    05DA    05DB    05DC    05DD    05DE    05DF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 05E0    05E1    05E2    05E3    05E4    05E5    05E6    05E7    05E8    05E9    05EA    05EB    05EC    05ED    05EE    05EF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 05F0    05F1    05F2    05F3    05F4    05F5    05F6    05F7    05F8    05F9    05FA    05FB    05FC    05FD    05FE    05FF
    it_LET, it_LET, it_LET, it_SEP, it_SEP, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 0600    0601    0602    0603    0604    0605    0606    0607    0608    0609    060A    060B    060C    060D    060E    060F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_SEP, it_NTC, it_NTC, it_NTC,
;;; 0610    0611    0612    0613    0614    0615    0616    0617    0618    0619    061A    061B    061C    061D    061E    061F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_SEP, it_NTC, it_NTC, it_NTC, it_SEP,
;;; 0620    0621    0622    0623    0624    0625    0626    0627    0628    0629    062A    062B    062C    062D    062E    062F
    it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0630    0631    0632    0633    0634    0635    0636    0637    0638    0639    063A    063B    063C    063D    063E    063F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0640    0641    0642    0643    0644    0645    0646    0647    0648    0649    064A    064B    064C    064D    064E    064F
    it_EXT, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB,
;;; 0650    0651    0652    0653    0654    0655    0656    0657    0658    0659    065A    065B    065C    065D    065E    065F
    it_CMB, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0660    0661    0662    0663    0664    0665    0666    0667    0668    0669    066A    066B    066C    066D    066E    066F
    it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC,
;;; 0670    0671    0672    0673    0674    0675    0676    0677    0678    0679    067A    067B    067C    067D    067E    067F
    it_CMB, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0680    0681    0682    0683    0684    0685    0686    0687    0688    0689    068A    068B    068C    068D    068E    068F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0690    0691    0692    0693    0694    0695    0696    0697    0698    0699    069A    069B    069C    069D    069E    069F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 06A0    06A1    06A2    06A3    06A4    06A5    06A6    06A7    06A8    06A9    06AA    06AB    06AC    06AD    06AE    06AF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 06B0    06B1    06B2    06B3    06B4    06B5    06B6    06B7    06B8    06B9    06BA    06BB    06BC    06BD    06BE    06BF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC,
;;; 06C0    06C1    06C2    06C3    06C4    06C5    06C6    06C7    06C8    06C9    06CA    06CB    06CC    06CD    06CE    06CF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC,
;;; 06D0    06D1    06D2    06D3    06D4    06D5    06D6    06D7    06D8    06D9    06DA    06DB    06DC    06DD    06DE    06DF
    it_LET, it_LET, it_LET, it_LET, it_SEP, it_LET, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB,
;;; 06E0    06E1    06E2    06E3    06E4    06E5    06E6    06E7    06E8    06E9    06EA    06EB    06EC    06ED    06EE    06EF
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_LET, it_LET, it_CMB, it_CMB, it_SGN, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC,
;;; 06F0    06F1    06F2    06F3    06F4    06F5    06F6    06F7    06F8    06F9    06FA    06FB    06FC    06FD    06FE    06FF
    it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

;;; 0700 - 07FF
it_NTC || 16:07FFe1;

;;; 0800 - 08FF
it_NTC || 16:08FFe1;

cons_it_string(
;;; 0900    0901    0902    0903    0904    0905    0906    0907    0908    0909    090A    090B    090C    090D    090E    090F
    it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0910    0911    0912    0913    0914    0915    0916    0917    0918    0919    091A    091B    091C    091D    091E    091F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0920    0921    0922    0923    0924    0925    0926    0927    0928    0929    092A    092B    092C    092D    092E    092F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0930    0931    0932    0933    0934    0935    0936    0937    0938    0939    093A    093B    093C    093D    093E    093F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_CMB, it_LET, it_CMB, it_CMB,
;;; 0940    0941    0942    0943    0944    0945    0946    0947    0948    0949    094A    094B    094C    094D    094E    094F
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC,
;;; 0950    0951    0952    0953    0954    0955    0956    0957    0958    0959    095A    095B    095C    095D    095E    095F
    it_SGN, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0960    0961    0962    0963    0964    0965    0966    0967    0968    0969    096A    096B    096C    096D    096E    096F
    it_LET, it_LET, it_CMB, it_CMB, it_SGN, it_SGN, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG,
;;; 0970    0971    0972    0973    0974    0975    0976    0977    0978    0979    097A    097B    097C    097D    097E    097F
    it_SEP, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0980    0981    0982    0983    0984    0985    0986    0987    0988    0989    098A    098B    098C    098D    098E    098F
    it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_LET,
;;; 0990    0991    0992    0993    0994    0995    0996    0997    0998    0999    099A    099B    099C    099D    099E    099F
    it_LET, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 09A0    09A1    09A2    09A3    09A4    09A5    09A6    09A7    09A8    09A9    09AA    09AB    09AC    09AD    09AE    09AF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 09B0    09B1    09B2    09B3    09B4    09B5    09B6    09B7    09B8    09B9    09BA    09BB    09BC    09BD    09BE    09BF
    it_LET, it_NTC, it_LET, it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_CMB, it_NTC, it_CMB, it_CMB,
;;; 09C0    09C1    09C2    09C3    09C4    09C5    09C6    09C7    09C8    09C9    09CA    09CB    09CC    09CD    09CE    09CF
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC, it_CMB, it_CMB, it_NTC, it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC,
;;; 09D0    09D1    09D2    09D3    09D4    09D5    09D6    09D7    09D8    09D9    09DA    09DB    09DC    09DD    09DE    09DF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_NTC, it_LET,
;;; 09E0    09E1    09E2    09E3    09E4    09E5    09E6    09E7    09E8    09E9    09EA    09EB    09EC    09ED    09EE    09EF
    it_LET, it_LET, it_CMB, it_CMB, it_NTC, it_NTC, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG,
;;; 09F0    09F1    09F2    09F3    09F4    09F5    09F6    09F7    09F8    09F9    09FA    09FB    09FC    09FD    09FE    09FF
    it_LET, it_LET, it_SGN, it_SGN, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 0A00    0A01    0A02    0A03    0A04    0A05    0A06    0A07    0A08    0A09    0A0A    0A0B    0A0C    0A0D    0A0E    0A0F
    it_NTC, it_NTC, it_CMB, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_LET,
;;; 0A10    0A11    0A12    0A13    0A14    0A15    0A16    0A17    0A18    0A19    0A1A    0A1B    0A1C    0A1D    0A1E    0A1F
    it_LET, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0A20    0A21    0A22    0A23    0A24    0A25    0A26    0A27    0A28    0A29    0A2A    0A2B    0A2C    0A2D    0A2E    0A2F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0A30    0A31    0A32    0A33    0A34    0A35    0A36    0A37    0A38    0A39    0A3A    0A3B    0A3C    0A3D    0A3E    0A3F
    it_LET, it_NTC, it_LET, it_LET, it_NTC, it_LET, it_LET, it_NTC, it_LET, it_LET, it_NTC, it_NTC, it_CMB, it_NTC, it_CMB, it_CMB,
;;; 0A40    0A41    0A42    0A43    0A44    0A45    0A46    0A47    0A48    0A49    0A4A    0A4B    0A4C    0A4D    0A4E    0A4F
    it_CMB, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_CMB, it_NTC, it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC,
;;; 0A50    0A51    0A52    0A53    0A54    0A55    0A56    0A57    0A58    0A59    0A5A    0A5B    0A5C    0A5D    0A5E    0A5F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_SEP, it_SEP, it_SEP, it_SEP, it_NTC, it_SEP, it_NTC,
;;; 0A60    0A61    0A62    0A63    0A64    0A65    0A66    0A67    0A68    0A69    0A6A    0A6B    0A6C    0A6D    0A6E    0A6F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG,
;;; 0A70    0A71    0A72    0A73    0A74    0A75    0A76    0A77    0A78    0A79    0A7A    0A7B    0A7C    0A7D    0A7E    0A7F
    it_CMB, it_CMB, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0A80    0A81    0A82    0A83    0A84    0A85    0A86    0A87    0A88    0A89    0A8A    0A8B    0A8C    0A8D    0A8E    0A8F
    it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_NTC, it_SEP, it_NTC, it_LET,
;;; 0A90    0A91    0A92    0A93    0A94    0A95    0A96    0A97    0A98    0A99    0A9A    0A9B    0A9C    0A9D    0A9E    0A9F
    it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0AA0    0AA1    0AA2    0AA3    0AA4    0AA5    0AA6    0AA7    0AA8    0AA9    0AAA    0AAB    0AAC    0AAD    0AAE    0AAF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0AB0    0AB1    0AB2    0AB3    0AB4    0AB5    0AB6    0AB7    0AB8    0AB9    0ABA    0ABB    0ABC    0ABD    0ABE    0ABF
    it_LET, it_NTC, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_CMB, it_LET, it_CMB, it_CMB,
;;; 0AC0    0AC1    0AC2    0AC3    0AC4    0AC5    0AC6    0AC7    0AC8    0AC9    0ACA    0ACB    0ACC    0ACD    0ACE    0ACF
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC,
;;; 0AD0    0AD1    0AD2    0AD3    0AD4    0AD5    0AD6    0AD7    0AD8    0AD9    0ADA    0ADB    0ADC    0ADD    0ADE    0ADF
    it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0AE0    0AE1    0AE2    0AE3    0AE4    0AE5    0AE6    0AE7    0AE8    0AE9    0AEA    0AEB    0AEC    0AED    0AEE    0AEF
    it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG,
;;; 0AF0    0AF1    0AF2    0AF3    0AF4    0AF5    0AF6    0AF7    0AF8    0AF9    0AFA    0AFB    0AFC    0AFD    0AFE    0AFF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 0B00    0B01    0B02    0B03    0B04    0B05    0B06    0B07    0B08    0B09    0B0A    0B0B    0B0C    0B0D    0B0E    0B0F
    it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_LET,
;;; 0B10    0B11    0B12    0B13    0B14    0B15    0B16    0B17    0B18    0B19    0B1A    0B1B    0B1C    0B1D    0B1E    0B1F
    it_LET, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0B20    0B21    0B22    0B23    0B24    0B25    0B26    0B27    0B28    0B29    0B2A    0B2B    0B2C    0B2D    0B2E    0B2F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0B30    0B31    0B32    0B33    0B34    0B35    0B36    0B37    0B38    0B39    0B3A    0B3B    0B3C    0B3D    0B3E    0B3F
    it_LET, it_NTC, it_LET, it_LET, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_CMB, it_LET, it_CMB, it_CMB,
;;; 0B40    0B41    0B42    0B43    0B44    0B45    0B46    0B47    0B48    0B49    0B4A    0B4B    0B4C    0B4D    0B4E    0B4F
    it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_CMB, it_CMB, it_NTC, it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC,
;;; 0B50    0B51    0B52    0B53    0B54    0B55    0B56    0B57    0B58    0B59    0B5A    0B5B    0B5C    0B5D    0B5E    0B5F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_NTC, it_LET,
;;; 0B60    0B61    0B62    0B63    0B64    0B65    0B66    0B67    0B68    0B69    0B6A    0B6B    0B6C    0B6D    0B6E    0B6F
    it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG,
;;; 0B70    0B71    0B72    0B73    0B74    0B75    0B76    0B77    0B78    0B79    0B7A    0B7B    0B7C    0B7D    0B7E    0B7F
    it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0B80    0B81    0B82    0B83    0B84    0B85    0B86    0B87    0B88    0B89    0B8A    0B8B    0B8C    0B8D    0B8E    0B8F
    it_NTC, it_NTC, it_CMB, it_CMB, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_LET, it_LET,
;;; 0B90    0B91    0B92    0B93    0B94    0B95    0B96    0B97    0B98    0B99    0B9A    0B9B    0B9C    0B9D    0B9E    0B9F
    it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_NTC, it_LET, it_NTC, it_LET, it_LET,
;;; 0BA0    0BA1    0BA2    0BA3    0BA4    0BA5    0BA6    0BA7    0BA8    0BA9    0BAA    0BAB    0BAC    0BAD    0BAE    0BAF
    it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_LET, it_LET,
;;; 0BB0    0BB1    0BB2    0BB3    0BB4    0BB5    0BB6    0BB7    0BB8    0BB9    0BBA    0BBB    0BBC    0BBD    0BBE    0BBF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_CMB,
;;; 0BC0    0BC1    0BC2    0BC3    0BC4    0BC5    0BC6    0BC7    0BC8    0BC9    0BCA    0BCB    0BCC    0BCD    0BCE    0BCF
    it_CMB, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC,
;;; 0BD0    0BD1    0BD2    0BD3    0BD4    0BD5    0BD6    0BD7    0BD8    0BD9    0BDA    0BDB    0BDC    0BDD    0BDE    0BDF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0BE0    0BE1    0BE2    0BE3    0BE4    0BE5    0BE6    0BE7    0BE8    0BE9    0BEA    0BEB    0BEC    0BED    0BEE    0BEF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG,
;;; 0BF0    0BF1    0BF2    0BF3    0BF4    0BF5    0BF6    0BF7    0BF8    0BF9    0BFA    0BFB    0BFC    0BFD    0BFE    0BFF
    it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 0C00    0C01    0C02    0C03    0C04    0C05    0C06    0C07    0C08    0C09    0C0A    0C0B    0C0C    0C0D    0C0E    0C0F
    it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET,
;;; 0C10    0C11    0C12    0C13    0C14    0C15    0C16    0C17    0C18    0C19    0C1A    0C1B    0C1C    0C1D    0C1E    0C1F
    it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0C20    0C21    0C22    0C23    0C24    0C25    0C26    0C27    0C28    0C29    0C2A    0C2B    0C2C    0C2D    0C2E    0C2F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0C30    0C31    0C32    0C33    0C34    0C35    0C36    0C37    0C38    0C39    0C3A    0C3B    0C3C    0C3D    0C3E    0C3F
    it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_CMB,
;;; 0C40    0C41    0C42    0C43    0C44    0C45    0C46    0C47    0C48    0C49    0C4A    0C4B    0C4C    0C4D    0C4E    0C4F
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC,
;;; 0C50    0C51    0C52    0C53    0C54    0C55    0C56    0C57    0C58    0C59    0C5A    0C5B    0C5C    0C5D    0C5E    0C5F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0C60    0C61    0C62    0C63    0C64    0C65    0C66    0C67    0C68    0C69    0C6A    0C6B    0C6C    0C6D    0C6E    0C6F
    it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG,
;;; 0C70    0C71    0C72    0C73    0C74    0C75    0C76    0C77    0C78    0C79    0C7A    0C7B    0C7C    0C7D    0C7E    0C7F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0C80    0C81    0C82    0C83    0C84    0C85    0C86    0C87    0C88    0C89    0C8A    0C8B    0C8C    0C8D    0C8E    0C8F
    it_NTC, it_NTC, it_CMB, it_CMB, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET,
;;; 0C90    0C91    0C92    0C93    0C94    0C95    0C96    0C97    0C98    0C99    0C9A    0C9B    0C9C    0C9D    0C9E    0C9F
    it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0CA0    0CA1    0CA2    0CA3    0CA4    0CA5    0CA6    0CA7    0CA8    0CA9    0CAA    0CAB    0CAC    0CAD    0CAE    0CAF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0CB0    0CB1    0CB2    0CB3    0CB4    0CB5    0CB6    0CB7    0CB8    0CB9    0CBA    0CBB    0CBC    0CBD    0CBE    0CBF
    it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_CMB,
;;; 0CC0    0CC1    0CC2    0CC3    0CC4    0CC5    0CC6    0CC7    0CC8    0CC9    0CCA    0CCB    0CCC    0CCD    0CCE    0CCF
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC,
;;; 0CD0    0CD1    0CD2    0CD3    0CD4    0CD5    0CD6    0CD7    0CD8    0CD9    0CDA    0CDB    0CDC    0CDD    0CDE    0CDF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_LET, it_NTC,
;;; 0CE0    0CE1    0CE2    0CE3    0CE4    0CE5    0CE6    0CE7    0CE8    0CE9    0CEA    0CEB    0CEC    0CED    0CEE    0CEF
    it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG,
;;; 0CF0    0CF1    0CF2    0CF3    0CF4    0CF5    0CF6    0CF7    0CF8    0CF9    0CFA    0CFB    0CFC    0CFD    0CFE    0CFF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 0D00    0D01    0D02    0D03    0D04    0D05    0D06    0D07    0D08    0D09    0D0A    0D0B    0D0C    0D0D    0D0E    0D0F
    it_NTC, it_NTC, it_CMB, it_CMB, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET,
;;; 0D10    0D11    0D12    0D13    0D14    0D15    0D16    0D17    0D18    0D19    0D1A    0D1B    0D1C    0D1D    0D1E    0D1F
    it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0D20    0D21    0D22    0D23    0D24    0D25    0D26    0D27    0D28    0D29    0D2A    0D2B    0D2C    0D2D    0D2E    0D2F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0D30    0D31    0D32    0D33    0D34    0D35    0D36    0D37    0D38    0D39    0D3A    0D3B    0D3C    0D3D    0D3E    0D3F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_CMB,
;;; 0D40    0D41    0D42    0D43    0D44    0D45    0D46    0D47    0D48    0D49    0D4A    0D4B    0D4C    0D4D    0D4E    0D4F
    it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC, it_CMB, it_CMB, it_CMB, it_NTC, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC,
;;; 0D50    0D51    0D52    0D53    0D54    0D55    0D56    0D57    0D58    0D59    0D5A    0D5B    0D5C    0D5D    0D5E    0D5F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0D60    0D61    0D62    0D63    0D64    0D65    0D66    0D67    0D68    0D69    0D6A    0D6B    0D6C    0D6D    0D6E    0D6F
    it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG,
;;; 0D70    0D71    0D72    0D73    0D74    0D75    0D76    0D77    0D78    0D79    0D7A    0D7B    0D7C    0D7D    0D7E    0D7F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0D80    0D81    0D82    0D83    0D84    0D85    0D86    0D87    0D88    0D89    0D8A    0D8B    0D8C    0D8D    0D8E    0D8F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0D90    0D91    0D92    0D93    0D94    0D95    0D96    0D97    0D98    0D99    0D9A    0D9B    0D9C    0D9D    0D9E    0D9F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0DA0    0DA1    0DA2    0DA3    0DA4    0DA5    0DA6    0DA7    0DA8    0DA9    0DAA    0DAB    0DAC    0DAD    0DAE    0DAF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0DB0    0DB1    0DB2    0DB3    0DB4    0DB5    0DB6    0DB7    0DB8    0DB9    0DBA    0DBB    0DBC    0DBD    0DBE    0DBF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0DC0    0DC1    0DC2    0DC3    0DC4    0DC5    0DC6    0DC7    0DC8    0DC9    0DCA    0DCB    0DCC    0DCD    0DCE    0DCF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0DD0    0DD1    0DD2    0DD3    0DD4    0DD5    0DD6    0DD7    0DD8    0DD9    0DDA    0DDB    0DDC    0DDD    0DDE    0DDF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0DE0    0DE1    0DE2    0DE3    0DE4    0DE5    0DE6    0DE7    0DE8    0DE9    0DEA    0DEB    0DEC    0DED    0DEE    0DEF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0DF0    0DF1    0DF2    0DF3    0DF4    0DF5    0DF6    0DF7    0DF8    0DF9    0DFA    0DFB    0DFC    0DFD    0DFE    0DFF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 0E00    0E01    0E02    0E03    0E04    0E05    0E06    0E07    0E08    0E09    0E0A    0E0B    0E0C    0E0D    0E0E    0E0F
    it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0E10    0E11    0E12    0E13    0E14    0E15    0E16    0E17    0E18    0E19    0E1A    0E1B    0E1C    0E1D    0E1E    0E1F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0E20    0E21    0E22    0E23    0E24    0E25    0E26    0E27    0E28    0E29    0E2A    0E2B    0E2C    0E2D    0E2E    0E2F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_SEP,
;;; 0E30    0E31    0E32    0E33    0E34    0E35    0E36    0E37    0E38    0E39    0E3A    0E3B    0E3C    0E3D    0E3E    0E3F
    it_LET, it_CMB, it_LET, it_LET, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_SGN,
;;; 0E40    0E41    0E42    0E43    0E44    0E45    0E46    0E47    0E48    0E49    0E4A    0E4B    0E4C    0E4D    0E4E    0E4F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_EXT, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_SEP,
;;; 0E50    0E51    0E52    0E53    0E54    0E55    0E56    0E57    0E58    0E59    0E5A    0E5B    0E5C    0E5D    0E5E    0E5F
    it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_SEP, it_SEP, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0E60    0E61    0E62    0E63    0E64    0E65    0E66    0E67    0E68    0E69    0E6A    0E6B    0E6C    0E6D    0E6E    0E6F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0E70    0E71    0E72    0E73    0E74    0E75    0E76    0E77    0E78    0E79    0E7A    0E7B    0E7C    0E7D    0E7E    0E7F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0E80    0E81    0E82    0E83    0E84    0E85    0E86    0E87    0E88    0E89    0E8A    0E8B    0E8C    0E8D    0E8E    0E8F
    it_NTC, it_LET, it_LET, it_NTC, it_LET, it_NTC, it_NTC, it_LET, it_LET, it_NTC, it_LET, it_NTC, it_NTC, it_LET, it_NTC, it_NTC,
;;; 0E90    0E91    0E92    0E93    0E94    0E95    0E96    0E97    0E98    0E99    0E9A    0E9B    0E9C    0E9D    0E9E    0E9F
    it_NTC, it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0EA0    0EA1    0EA2    0EA3    0EA4    0EA5    0EA6    0EA7    0EA8    0EA9    0EAA    0EAB    0EAC    0EAD    0EAE    0EAF
    it_NTC, it_LET, it_LET, it_LET, it_NTC, it_LET, it_NTC, it_LET, it_NTC, it_NTC, it_LET, it_LET, it_NTC, it_LET, it_LET, it_SEP,
;;; 0EB0    0EB1    0EB2    0EB3    0EB4    0EB5    0EB6    0EB7    0EB8    0EB9    0EBA    0EBB    0EBC    0EBD    0EBE    0EBF
    it_LET, it_CMB, it_LET, it_LET, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_CMB, it_CMB, it_LET, it_NTC, it_NTC,
;;; 0EC0    0EC1    0EC2    0EC3    0EC4    0EC5    0EC6    0EC7    0EC8    0EC9    0ECA    0ECB    0ECC    0ECD    0ECE    0ECF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_EXT, it_NTC, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC,
;;; 0ED0    0ED1    0ED2    0ED3    0ED4    0ED5    0ED6    0ED7    0ED8    0ED9    0EDA    0EDB    0EDC    0EDD    0EDE    0EDF
    it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_NTC, it_NTC, it_LET, it_LET, it_NTC, it_NTC,
;;; 0EE0    0EE1    0EE2    0EE3    0EE4    0EE5    0EE6    0EE7    0EE8    0EE9    0EEA    0EEB    0EEC    0EED    0EEE    0EEF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0EF0    0EF1    0EF2    0EF3    0EF4    0EF5    0EF6    0EF7    0EF8    0EF9    0EFA    0EFB    0EFC    0EFD    0EFE    0EFF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 0F00    0F01    0F02    0F03    0F04    0F05    0F06    0F07    0F08    0F09    0F0A    0F0B    0F0C    0F0D    0F0E    0F0F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP,
;;; 0F10    0F11    0F12    0F13    0F14    0F15    0F16    0F17    0F18    0F19    0F1A    0F1B    0F1C    0F1D    0F1E    0F1F
    it_SEP, it_SEP, it_SEP, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_CMB, it_CMB, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 0F20    0F21    0F22    0F23    0F24    0F25    0F26    0F27    0F28    0F29    0F2A    0F2B    0F2C    0F2D    0F2E    0F2F
    it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_DIG, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP,
;;; 0F30    0F31    0F32    0F33    0F34    0F35    0F36    0F37    0F38    0F39    0F3A    0F3B    0F3C    0F3D    0F3E    0F3F
    it_SEP, it_SEP, it_SEP, it_SEP, it_SGN, it_CMB, it_SGN, it_CMB, it_SGN, it_CMB, it_SEP, it_SEP, it_SEP, it_SEP, it_CMB, it_CMB,
;;; 0F40    0F41    0F42    0F43    0F44    0F45    0F46    0F47    0F48    0F49    0F4A    0F4B    0F4C    0F4D    0F4E    0F4F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0F50    0F51    0F52    0F53    0F54    0F55    0F56    0F57    0F58    0F59    0F5A    0F5B    0F5C    0F5D    0F5E    0F5F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 0F60    0F61    0F62    0F63    0F64    0F65    0F66    0F67    0F68    0F69    0F6A    0F6B    0F6C    0F6D    0F6E    0F6F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0F70    0F71    0F72    0F73    0F74    0F75    0F76    0F77    0F78    0F79    0F7A    0F7B    0F7C    0F7D    0F7E    0F7F
    it_NTC, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB,
;;; 0F80    0F81    0F82    0F83    0F84    0F85    0F86    0F87    0F88    0F89    0F8A    0F8B    0F8C    0F8D    0F8E    0F8F
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_SEP, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0F90    0F91    0F92    0F93    0F94    0F95    0F96    0F97    0F98    0F99    0F9A    0F9B    0F9C    0F9D    0F9E    0F9F
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_CMB, it_NTC, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB,
;;; 0FA0    0FA1    0FA2    0FA3    0FA4    0FA5    0FA6    0FA7    0FA8    0FA9    0FAA    0FAB    0FAC    0FAD    0FAE    0FAF
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC,
;;; 0FB0    0FB1    0FB2    0FB3    0FB4    0FB5    0FB6    0FB7    0FB8    0FB9    0FBA    0FBB    0FBC    0FBD    0FBE    0FBF
    it_NTC, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0FC0    0FC1    0FC2    0FC3    0FC4    0FC5    0FC6    0FC7    0FC8    0FC9    0FCA    0FCB    0FCC    0FCD    0FCE    0FCF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0FD0    0FD1    0FD2    0FD3    0FD4    0FD5    0FD6    0FD7    0FD8    0FD9    0FDA    0FDB    0FDC    0FDD    0FDE    0FDF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0FE0    0FE1    0FE2    0FE3    0FE4    0FE5    0FE6    0FE7    0FE8    0FE9    0FEA    0FEB    0FEC    0FED    0FEE    0FEF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 0FF0    0FF1    0FF2    0FF3    0FF4    0FF5    0FF6    0FF7    0FF8    0FF9    0FFA    0FFB    0FFC    0FFD    0FFE    0FFF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 1000    1001    1002    1003    1004    1005    1006    1007    1008    1009    100A    100B    100C    100D    100E    100F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 1010    1011    1012    1013    1014    1015    1016    1017    1018    1019    101A    101B    101C    101D    101E    101F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 1020    1021    1022    1023    1024    1025    1026    1027    1028    1029    102A    102B    102C    102D    102E    102F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 1030    1031    1032    1033    1034    1035    1036    1037    1038    1039    103A    103B    103C    103D    103E    103F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 1040    1041    1042    1043    1044    1045    1046    1047    1048    1049    104A    104B    104C    104D    104E    104F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 1050    1051    1052    1053    1054    1055    1056    1057    1058    1059    105A    105B    105C    105D    105E    105F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 1060    1061    1062    1063    1064    1065    1066    1067    1068    1069    106A    106B    106C    106D    106E    106F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 1070    1071    1072    1073    1074    1075    1076    1077    1078    1079    107A    107B    107C    107D    107E    107F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 1080    1081    1082    1083    1084    1085    1086    1087    1088    1089    108A    108B    108C    108D    108E    108F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 1090    1091    1092    1093    1094    1095    1096    1097    1098    1099    109A    109B    109C    109D    109E    109F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 10A0    10A1    10A2    10A3    10A4    10A5    10A6    10A7    10A8    10A9    10AA    10AB    10AC    10AD    10AE    10AF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 10B0    10B1    10B2    10B3    10B4    10B5    10B6    10B7    10B8    10B9    10BA    10BB    10BC    10BD    10BE    10BF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 10C0    10C1    10C2    10C3    10C4    10C5    10C6    10C7    10C8    10C9    10CA    10CB    10CC    10CD    10CE    10CF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 10D0    10D1    10D2    10D3    10D4    10D5    10D6    10D7    10D8    10D9    10DA    10DB    10DC    10DD    10DE    10DF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 10E0    10E1    10E2    10E3    10E4    10E5    10E6    10E7    10E8    10E9    10EA    10EB    10EC    10ED    10EE    10EF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 10F0    10F1    10F2    10F3    10F4    10F5    10F6    10F7    10F8    10F9    10FA    10FB    10FC    10FD    10FE    10FF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_SEP, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 1100    1101    1102    1103    1104    1105    1106    1107    1108    1109    110A    110B    110C    110D    110E    110F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1110    1111    1112    1113    1114    1115    1116    1117    1118    1119    111A    111B    111C    111D    111E    111F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1120    1121    1122    1123    1124    1125    1126    1127    1128    1129    112A    112B    112C    112D    112E    112F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1130    1131    1132    1133    1134    1135    1136    1137    1138    1139    113A    113B    113C    113D    113E    113F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1140    1141    1142    1143    1144    1145    1146    1147    1148    1149    114A    114B    114C    114D    114E    114F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1150    1151    1152    1153    1154    1155    1156    1157    1158    1159    115A    115B    115C    115D    115E    115F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_LET,
;;; 1160    1161    1162    1163    1164    1165    1166    1167    1168    1169    116A    116B    116C    116D    116E    116F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1170    1171    1172    1173    1174    1175    1176    1177    1178    1179    117A    117B    117C    117D    117E    117F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1180    1181    1182    1183    1184    1185    1186    1187    1188    1189    118A    118B    118C    118D    118E    118F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1190    1191    1192    1193    1194    1195    1196    1197    1198    1199    119A    119B    119C    119D    119E    119F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 11A0    11A1    11A2    11A3    11A4    11A5    11A6    11A7    11A8    11A9    11AA    11AB    11AC    11AD    11AE    11AF
    it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 11B0    11B1    11B2    11B3    11B4    11B5    11B6    11B7    11B8    11B9    11BA    11BB    11BC    11BD    11BE    11BF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 11C0    11C1    11C2    11C3    11C4    11C5    11C6    11C7    11C8    11C9    11CA    11CB    11CC    11CD    11CE    11CF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 11D0    11D1    11D2    11D3    11D4    11D5    11D6    11D7    11D8    11D9    11DA    11DB    11DC    11DD    11DE    11DF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 11E0    11E1    11E2    11E3    11E4    11E5    11E6    11E7    11E8    11E9    11EA    11EB    11EC    11ED    11EE    11EF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 11F0    11F1    11F2    11F3    11F4    11F5    11F6    11F7    11F8    11F9    11FA    11FB    11FC    11FD    11FE    11FF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

;;; 1200 - 12FF
it_NTC || 16:12FFe1;

;;; 1300 - 13FF
it_NTC || 16:13FFe1;

;;; 1400 - 14FF
it_NTC || 16:14FFe1;

;;; 1500 - 15FF
it_NTC || 16:15FFe1;

;;; 1600 - 16FF
it_NTC || 16:16FFe1;

;;; 1700 - 17FF
it_NTC || 16:17FFe1;

;;; 1800 - 18FF
it_NTC || 16:18FFe1;

;;; 1900 - 19FF
it_NTC || 16:19FFe1;

;;; 1A00 - 1AFF
it_NTC || 16:1AFFe1;

;;; 1B00 - 1BFF
it_NTC || 16:1BFFe1;

;;; 1C00 - 1CFF
it_NTC || 16:1CFFe1;

;;; 1D00 - 1DFF
it_NTC || 16:1DFFe1;

cons_it_string(
;;; 1E00    1E01    1E02    1E03    1E04    1E05    1E06    1E07    1E08    1E09    1E0A    1E0B    1E0C    1E0D    1E0E    1E0F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1E10    1E11    1E12    1E13    1E14    1E15    1E16    1E17    1E18    1E19    1E1A    1E1B    1E1C    1E1D    1E1E    1E1F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1E20    1E21    1E22    1E23    1E24    1E25    1E26    1E27    1E28    1E29    1E2A    1E2B    1E2C    1E2D    1E2E    1E2F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1E30    1E31    1E32    1E33    1E34    1E35    1E36    1E37    1E38    1E39    1E3A    1E3B    1E3C    1E3D    1E3E    1E3F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1E40    1E41    1E42    1E43    1E44    1E45    1E46    1E47    1E48    1E49    1E4A    1E4B    1E4C    1E4D    1E4E    1E4F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1E50    1E51    1E52    1E53    1E54    1E55    1E56    1E57    1E58    1E59    1E5A    1E5B    1E5C    1E5D    1E5E    1E5F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1E60    1E61    1E62    1E63    1E64    1E65    1E66    1E67    1E68    1E69    1E6A    1E6B    1E6C    1E6D    1E6E    1E6F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1E70    1E71    1E72    1E73    1E74    1E75    1E76    1E77    1E78    1E79    1E7A    1E7B    1E7C    1E7D    1E7E    1E7F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1E80    1E81    1E82    1E83    1E84    1E85    1E86    1E87    1E88    1E89    1E8A    1E8B    1E8C    1E8D    1E8E    1E8F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1E90    1E91    1E92    1E93    1E94    1E95    1E96    1E97    1E98    1E99    1E9A    1E9B    1E9C    1E9D    1E9E    1E9F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 1EA0    1EA1    1EA2    1EA3    1EA4    1EA5    1EA6    1EA7    1EA8    1EA9    1EAA    1EAB    1EAC    1EAD    1EAE    1EAF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1EB0    1EB1    1EB2    1EB3    1EB4    1EB5    1EB6    1EB7    1EB8    1EB9    1EBA    1EBB    1EBC    1EBD    1EBE    1EBF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1EC0    1EC1    1EC2    1EC3    1EC4    1EC5    1EC6    1EC7    1EC8    1EC9    1ECA    1ECB    1ECC    1ECD    1ECE    1ECF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1ED0    1ED1    1ED2    1ED3    1ED4    1ED5    1ED6    1ED7    1ED8    1ED9    1EDA    1EDB    1EDC    1EDD    1EDE    1EDF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1EE0    1EE1    1EE2    1EE3    1EE4    1EE5    1EE6    1EE7    1EE8    1EE9    1EEA    1EEB    1EEC    1EED    1EEE    1EEF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1EF0    1EF1    1EF2    1EF3    1EF4    1EF5    1EF6    1EF7    1EF8    1EF9    1EFA    1EFB    1EFC    1EFD    1EFE    1EFF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 1F00    1F01    1F02    1F03    1F04    1F05    1F06    1F07    1F08    1F09    1F0A    1F0B    1F0C    1F0D    1F0E    1F0F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1F10    1F11    1F12    1F13    1F14    1F15    1F16    1F17    1F18    1F19    1F1A    1F1B    1F1C    1F1D    1F1E    1F1F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC,
;;; 1F20    1F21    1F22    1F23    1F24    1F25    1F26    1F27    1F28    1F29    1F2A    1F2B    1F2C    1F2D    1F2E    1F2F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1F30    1F31    1F32    1F33    1F34    1F35    1F36    1F37    1F38    1F39    1F3A    1F3B    1F3C    1F3D    1F3E    1F3F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1F40    1F41    1F42    1F43    1F44    1F45    1F46    1F47    1F48    1F49    1F4A    1F4B    1F4C    1F4D    1F4E    1F4F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC,
;;; 1F50    1F51    1F52    1F53    1F54    1F55    1F56    1F57    1F58    1F59    1F5A    1F5B    1F5C    1F5D    1F5E    1F5F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_NTC, it_LET, it_NTC, it_LET, it_NTC, it_LET,
;;; 1F60    1F61    1F62    1F63    1F64    1F65    1F66    1F67    1F68    1F69    1F6A    1F6B    1F6C    1F6D    1F6E    1F6F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1F70    1F71    1F72    1F73    1F74    1F75    1F76    1F77    1F78    1F79    1F7A    1F7B    1F7C    1F7D    1F7E    1F7F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC,
;;; 1F80    1F81    1F82    1F83    1F84    1F85    1F86    1F87    1F88    1F89    1F8A    1F8B    1F8C    1F8D    1F8E    1F8F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1F90    1F91    1F92    1F93    1F94    1F95    1F96    1F97    1F98    1F99    1F9A    1F9B    1F9C    1F9D    1F9E    1F9F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1FA0    1FA1    1FA2    1FA3    1FA4    1FA5    1FA6    1FA7    1FA8    1FA9    1FAA    1FAB    1FAC    1FAD    1FAE    1FAF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 1FB0    1FB1    1FB2    1FB3    1FB4    1FB5    1FB6    1FB7    1FB8    1FB9    1FBA    1FBB    1FBC    1FBD    1FBE    1FBF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_SEP, it_LET, it_SEP,
;;; 1FC0    1FC1    1FC2    1FC3    1FC4    1FC5    1FC6    1FC7    1FC8    1FC9    1FCA    1FCB    1FCC    1FCD    1FCE    1FCF
    it_SEP, it_SEP, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_SEP, it_SEP, it_SEP,
;;; 1FD0    1FD1    1FD2    1FD3    1FD4    1FD5    1FD6    1FD7    1FD8    1FD9    1FDA    1FDB    1FDC    1FDD    1FDE    1FDF
    it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_SEP, it_SEP, it_SEP,
;;; 1FE0    1FE1    1FE2    1FE3    1FE4    1FE5    1FE6    1FE7    1FE8    1FE9    1FEA    1FEB    1FEC    1FED    1FEE    1FEF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_SEP, it_SEP, it_SEP,
;;; 1FF0    1FF1    1FF2    1FF3    1FF4    1FF5    1FF6    1FF7    1FF8    1FF9    1FFA    1FFB    1FFC    1FFD    1FFE    1FFF
    it_NTC, it_NTC, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_SEP, it_SEP, it_NTC,
);

cons_it_string(
;;; 2000    2001    2002    2003    2004    2005    2006    2007    2008    2009    200A    200B    200C    200D    200E    200F
    it_SP,  it_SP,  it_SP,  it_SP,  it_SP,  it_SP,  it_SP,  it_SP,  it_SP,  it_SP,  it_SP,  it_SP,  it_IGN, it_IGN, it_IGN, it_IGN,
;;; 2010    2011    2012    2013    2014    2015    2016    2017    2018    2019    201A    201B    201C    201D    201E    201F
    it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP,
;;; 2020    2021    2022    2023    2024    2025    2026    2027    2028    2029    202A    202B    202C    202D    202E    202F
    it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SP,  it_SP,  it_IGN, it_IGN, it_IGN, it_IGN, it_IGN, it_NTC,
;;; 2030    2031    2032    2033    2034    2035    2036    2037    2038    2039    203A    203B    203C    203D    203E    203F
    it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SGN, it_SGN, it_SGN,
;;; 2040    2041    2042    2043    2044    2045    2046    2047    2048    2049    204A    204B    204C    204D    204E    204F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SEP, it_SEP, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2050    2051    2052    2053    2054    2055    2056    2057    2058    2059    205A    205B    205C    205D    205E    205F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2060    2061    2062    2063    2064    2065    2066    2067    2068    2069    206A    206B    206C    206D    206E    206F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_IGN, it_IGN, it_IGN, it_IGN, it_IGN, it_IGN,
;;; 2070    2071    2072    2073    2074    2075    2076    2077    2078    2079    207A    207B    207C    207D    207E    207F
    it_EXT, it_NTC, it_NTC, it_NTC, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT,
;;; 2080    2081    2082    2083    2084    2085    2086    2087    2088    2089    208A    208B    208C    208D    208E    208F
    it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_NTC,
;;; 2090    2091    2092    2093    2094    2095    2096    2097    2098    2099    209A    209B    209C    209D    209E    209F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 20A0    20A1    20A2    20A3    20A4    20A5    20A6    20A7    20A8    20A9    20AA    20AB    20AC    20AD    20AE    20AF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 20B0    20B1    20B2    20B3    20B4    20B5    20B6    20B7    20B8    20B9    20BA    20BB    20BC    20BD    20BE    20BF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 20C0    20C1    20C2    20C3    20C4    20C5    20C6    20C7    20C8    20C9    20CA    20CB    20CC    20CD    20CE    20CF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 20D0    20D1    20D2    20D3    20D4    20D5    20D6    20D7    20D8    20D9    20DA    20DB    20DC    20DD    20DE    20DF
    it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_ENC, it_ENC, it_ENC,
;;; 20E0    20E1    20E2    20E3    20E4    20E5    20E6    20E7    20E8    20E9    20EA    20EB    20EC    20ED    20EE    20EF
    it_ENC, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 20F0    20F1    20F2    20F3    20F4    20F5    20F6    20F7    20F8    20F9    20FA    20FB    20FC    20FD    20FE    20FF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 2100    2101    2102    2103    2104    2105    2106    2107    2108    2109    210A    210B    210C    210D    210E    210F
    it_SGN, it_SGN, it_LET, it_SGN, it_SGN, it_SGN, it_SGN, it_LET, it_SGN, it_SGN, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 2110    2111    2112    2113    2114    2115    2116    2117    2118    2119    211A    211B    211C    211D    211E    211F
    it_LET, it_LET, it_LET, it_LET, it_SGN, it_LET, it_SEP, it_SEP, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_SGN, it_SGN,
;;; 2120    2121    2122    2123    2124    2125    2126    2127    2128    2129    212A    212B    212C    212D    212E    212F
    it_SEP, it_SEP, it_SEP, it_SGN, it_LET, it_SGN, it_LET, it_SGN, it_LET, it_SGN, it_LET, it_LET, it_LET, it_LET, it_SEP, it_LET,
;;; 2130    2131    2132    2133    2134    2135    2136    2137    2138    2139    213A    213B    213C    213D    213E    213F
    it_LET, it_LET, it_SGN, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2140    2141    2142    2143    2144    2145    2146    2147    2148    2149    214A    214B    214C    214D    214E    214F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2150    2151    2152    2153    2154    2155    2156    2157    2158    2159    215A    215B    215C    215D    215E    215F
    it_NTC, it_NTC, it_NTC, it_FRC, it_FRC, it_FRC, it_FRC, it_FRC, it_FRC, it_FRC, it_FRC, it_FRC, it_FRC, it_FRC, it_FRC, it_SGN,
;;; 2160    2161    2162    2163    2164    2165    2166    2167    2168    2169    216A    216B    216C    216D    216E    216F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 2170    2171    2172    2173    2174    2175    2176    2177    2178    2179    217A    217B    217C    217D    217E    217F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 2180    2181    2182    2183    2184    2185    2186    2187    2188    2189    218A    218B    218C    218D    218E    218F
    it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2190    2191    2192    2193    2194    2195    2196    2197    2198    2199    219A    219B    219C    219D    219E    219F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 21A0    21A1    21A2    21A3    21A4    21A5    21A6    21A7    21A8    21A9    21AA    21AB    21AC    21AD    21AE    21AF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 21B0    21B1    21B2    21B3    21B4    21B5    21B6    21B7    21B8    21B9    21BA    21BB    21BC    21BD    21BE    21BF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 21C0    21C1    21C2    21C3    21C4    21C5    21C6    21C7    21C8    21C9    21CA    21CB    21CC    21CD    21CE    21CF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 21D0    21D1    21D2    21D3    21D4    21D5    21D6    21D7    21D8    21D9    21DA    21DB    21DC    21DD    21DE    21DF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 21E0    21E1    21E2    21E3    21E4    21E5    21E6    21E7    21E8    21E9    21EA    21EB    21EC    21ED    21EE    21EF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 21F0    21F1    21F2    21F3    21F4    21F5    21F6    21F7    21F8    21F9    21FA    21FB    21FC    21FD    21FE    21FF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

;;; 2200 - 22FF
it_SGN || 16:22F1e1;

cons_it_string(
;;; 2300    2301    2302    2303    2304    2305    2306    2307    2308    2309    230A    230B    230C    230D    230E    230F
    it_SGN, it_NTC, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2310    2311    2312    2313    2314    2315    2316    2317    2318    2319    231A    231B    231C    231D    231E    231F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2320    2321    2322    2323    2324    2325    2326    2327    2328    2329    232A    232B    232C    232D    232E    232F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SEP, it_SEP, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2330    2331    2332    2333    2334    2335    2336    2337    2338    2339    233A    233B    233C    233D    233E    233F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2340    2341    2342    2343    2344    2345    2346    2347    2348    2349    234A    234B    234C    234D    234E    234F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2350    2351    2352    2353    2354    2355    2356    2357    2358    2359    235A    235B    235C    235D    235E    235F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2360    2361    2362    2363    2364    2365    2366    2367    2368    2369    236A    236B    236C    236D    236E    236F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2370    2371    2372    2373    2374    2375    2376    2377    2378    2379    237A    237B    237C    237D    237E    237F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2380    2381    2382    2383    2384    2385    2386    2387    2388    2389    238A    238B    238C    238D    238E    238F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2390    2391    2392    2393    2394    2395    2396    2397    2398    2399    239A    239B    239C    239D    239E    239F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 23A0    23A1    23A2    23A3    23A4    23A5    23A6    23A7    23A8    23A9    23AA    23AB    23AC    23AD    23AE    23AF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 23B0    23B1    23B2    23B3    23B4    23B5    23B6    23B7    23B8    23B9    23BA    23BB    23BC    23BD    23BE    23BF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 23C0    23C1    23C2    23C3    23C4    23C5    23C6    23C7    23C8    23C9    23CA    23CB    23CC    23CD    23CE    23CF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 23D0    23D1    23D2    23D3    23D4    23D5    23D6    23D7    23D8    23D9    23DA    23DB    23DC    23DD    23DE    23DF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 23E0    23E1    23E2    23E3    23E4    23E5    23E6    23E7    23E8    23E9    23EA    23EB    23EC    23ED    23EE    23EF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 23F0    23F1    23F2    23F3    23F4    23F5    23F6    23F7    23F8    23F9    23FA    23FB    23FC    23FD    23FE    23FF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 2400    2401    2402    2403    2404    2405    2406    2407    2408    2409    240A    240B    240C    240D    240E    240F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2410    2411    2412    2413    2414    2415    2416    2417    2418    2419    241A    241B    241C    241D    241E    241F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2420    2421    2422    2423    2424    2425    2426    2427    2428    2429    242A    242B    242C    242D    242E    242F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2430    2431    2432    2433    2434    2435    2436    2437    2438    2439    243A    243B    243C    243D    243E    243F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2440    2441    2442    2443    2444    2445    2446    2447    2448    2449    244A    244B    244C    244D    244E    244F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2450    2451    2452    2453    2454    2455    2456    2457    2458    2459    245A    245B    245C    245D    245E    245F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2460    2461    2462    2463    2464    2465    2466    2467    2468    2469    246A    246B    246C    246D    246E    246F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2470    2471    2472    2473    2474    2475    2476    2477    2478    2479    247A    247B    247C    247D    247E    247F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2480    2481    2482    2483    2484    2485    2486    2487    2488    2489    248A    248B    248C    248D    248E    248F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2490    2491    2492    2493    2494    2495    2496    2497    2498    2499    249A    249B    249C    249D    249E    249F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 24A0    24A1    24A2    24A3    24A4    24A5    24A6    24A7    24A8    24A9    24AA    24AB    24AC    24AD    24AE    24AF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 24B0    24B1    24B2    24B3    24B4    24B5    24B6    24B7    24B8    24B9    24BA    24BB    24BC    24BD    24BE    24BF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 24C0    24C1    24C2    24C3    24C4    24C5    24C6    24C7    24C8    24C9    24CA    24CB    24CC    24CD    24CE    24CF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 24D0    24D1    24D2    24D3    24D4    24D5    24D6    24D7    24D8    24D9    24DA    24DB    24DC    24DD    24DE    24DF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 24E0    24E1    24E2    24E3    24E4    24E5    24E6    24E7    24E8    24E9    24EA    24EB    24EC    24ED    24EE    24EF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 24F0    24F1    24F2    24F3    24F4    24F5    24F6    24F7    24F8    24F9    24FA    24FB    24FC    24FD    24FE    24FF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 2500    2501    2502    2503    2504    2505    2506    2507    2508    2509    250A    250B    250C    250D    250E    250F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2510    2511    2512    2513    2514    2515    2516    2517    2518    2519    251A    251B    251C    251D    251E    251F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2520    2521    2522    2523    2524    2525    2526    2527    2528    2529    252A    252B    252C    252D    252E    252F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2530    2531    2532    2533    2534    2535    2536    2537    2538    2539    253A    253B    253C    253D    253E    253F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2540    2541    2542    2543    2544    2545    2546    2547    2548    2549    254A    254B    254C    254D    254E    254F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2550    2551    2552    2553    2554    2555    2556    2557    2558    2559    255A    255B    255C    255D    255E    255F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2560    2561    2562    2563    2564    2565    2566    2567    2568    2569    256A    256B    256C    256D    256E    256F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2570    2571    2572    2573    2574    2575    2576    2577    2578    2579    257A    257B    257C    257D    257E    257F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2580    2581    2582    2583    2584    2585    2586    2587    2588    2589    258A    258B    258C    258D    258E    258F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2590    2591    2592    2593    2594    2595    2596    2597    2598    2599    259A    259B    259C    259D    259E    259F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 25A0    25A1    25A2    25A3    25A4    25A5    25A6    25A7    25A8    25A9    25AA    25AB    25AC    25AD    25AE    25AF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 25B0    25B1    25B2    25B3    25B4    25B5    25B6    25B7    25B8    25B9    25BA    25BB    25BC    25BD    25BE    25BF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 25C0    25C1    25C2    25C3    25C4    25C5    25C6    25C7    25C8    25C9    25CA    25CB    25CC    25CD    25CE    25CF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 25D0    25D1    25D2    25D3    25D4    25D5    25D6    25D7    25D8    25D9    25DA    25DB    25DC    25DD    25DE    25DF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 25E0    25E1    25E2    25E3    25E4    25E5    25E6    25E7    25E8    25E9    25EA    25EB    25EC    25ED    25EE    25EF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 25F0    25F1    25F2    25F3    25F4    25F5    25F6    25F7    25F8    25F9    25FA    25FB    25FC    25FD    25FE    25FF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 2600    2601    2602    2603    2604    2605    2606    2607    2608    2609    260A    260B    260C    260D    260E    260F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2610    2611    2612    2613    2614    2615    2616    2617    2618    2619    261A    261B    261C    261D    261E    261F
    it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2620    2621    2622    2623    2624    2625    2626    2627    2628    2629    262A    262B    262C    262D    262E    262F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2630    2631    2632    2633    2634    2635    2636    2637    2638    2639    263A    263B    263C    263D    263E    263F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2640    2641    2642    2643    2644    2645    2646    2647    2648    2649    264A    264B    264C    264D    264E    264F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2650    2651    2652    2653    2654    2655    2656    2657    2658    2659    265A    265B    265C    265D    265E    265F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2660    2661    2662    2663    2664    2665    2666    2667    2668    2669    266A    266B    266C    266D    266E    266F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2670    2671    2672    2673    2674    2675    2676    2677    2678    2679    267A    267B    267C    267D    267E    267F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2680    2681    2682    2683    2684    2685    2686    2687    2688    2689    268A    268B    268C    268D    268E    268F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2690    2691    2692    2693    2694    2695    2696    2697    2698    2699    269A    269B    269C    269D    269E    269F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 26A0    26A1    26A2    26A3    26A4    26A5    26A6    26A7    26A8    26A9    26AA    26AB    26AC    26AD    26AE    26AF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 26B0    26B1    26B2    26B3    26B4    26B5    26B6    26B7    26B8    26B9    26BA    26BB    26BC    26BD    26BE    26BF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 26C0    26C1    26C2    26C3    26C4    26C5    26C6    26C7    26C8    26C9    26CA    26CB    26CC    26CD    26CE    26CF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 26D0    26D1    26D2    26D3    26D4    26D5    26D6    26D7    26D8    26D9    26DA    26DB    26DC    26DD    26DE    26DF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 26E0    26E1    26E2    26E3    26E4    26E5    26E6    26E7    26E8    26E9    26EA    26EB    26EC    26ED    26EE    26EF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 26F0    26F1    26F2    26F3    26F4    26F5    26F6    26F7    26F8    26F9    26FA    26FB    26FC    26FD    26FE    26FF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 2700    2701    2702    2703    2704    2705    2706    2707    2708    2709    270A    270B    270C    270D    270E    270F
    it_NTC, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2710    2711    2712    2713    2714    2715    2716    2717    2718    2719    271A    271B    271C    271D    271E    271F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2720    2721    2722    2723    2724    2725    2726    2727    2728    2729    272A    272B    272C    272D    272E    272F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2730    2731    2732    2733    2734    2735    2736    2737    2738    2739    273A    273B    273C    273D    273E    273F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2740    2741    2742    2743    2744    2745    2746    2747    2748    2749    274A    274B    274C    274D    274E    274F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_SGN, it_NTC, it_SGN,
;;; 2750    2751    2752    2753    2754    2755    2756    2757    2758    2759    275A    275B    275C    275D    275E    275F
    it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_SGN, it_NTC, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC,
;;; 2760    2761    2762    2763    2764    2765    2766    2767    2768    2769    276A    276B    276C    276D    276E    276F
    it_NTC, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 2770    2771    2772    2773    2774    2775    2776    2777    2778    2779    277A    277B    277C    277D    277E    277F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2780    2781    2782    2783    2784    2785    2786    2787    2788    2789    278A    278B    278C    278D    278E    278F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 2790    2791    2792    2793    2794    2795    2796    2797    2798    2799    279A    279B    279C    279D    279E    279F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 27A0    27A1    27A2    27A3    27A4    27A5    27A6    27A7    27A8    27A9    27AA    27AB    27AC    27AD    27AE    27AF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 27B0    27B1    27B2    27B3    27B4    27B5    27B6    27B7    27B8    27B9    27BA    27BB    27BC    27BD    27BE    27BF
    it_NTC, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC,
;;; 27C0    27C1    27C2    27C3    27C4    27C5    27C6    27C7    27C8    27C9    27CA    27CB    27CC    27CD    27CE    27CF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 27D0    27D1    27D2    27D3    27D4    27D5    27D6    27D7    27D8    27D9    27DA    27DB    27DC    27DD    27DE    27DF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 27E0    27E1    27E2    27E3    27E4    27E5    27E6    27E7    27E8    27E9    27EA    27EB    27EC    27ED    27EE    27EF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 27F0    27F1    27F2    27F3    27F4    27F5    27F6    27F7    27F8    27F9    27FA    27FB    27FC    27FD    27FE    27FF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

;;; 2800 - 28FF
it_NTC || 16:28FFe1;

;;; 2900 - 29FF
it_NTC || 16:29FFe1;

;;; 2A00 - 2AFF
it_NTC || 16:2AFFe1;

;;; 2B00 - 2BFF
it_NTC || 16:2BFFe1;

;;; 2C00 - 2CFF
it_NTC || 16:2CFFe1;

;;; 2D00 - 2DFF
it_NTC || 16:2DFFe1;

;;; 2E00 - 2EFF
it_NTC || 16:2EFFe1;

;;; 2F00 - 2FFF
it_NTC || 16:2FFFe1;

cons_it_string(
;;; 3000    3001    3002    3003    3004    3005    3006    3007    3008    3009    300A    300B    300C    300D    300E    300F
    it_SP,  it_SEP, it_SEP, it_SEP, it_SGN, it_EXT, it_SEP, it_LET, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP,
;;; 3010    3011    3012    3013    3014    3015    3016    3017    3018    3019    301A    301B    301C    301D    301E    301F
    it_SEP, it_SEP, it_SGN, it_SGN, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SGN, it_SGN, it_SGN,
;;; 3020    3021    3022    3023    3024    3025    3026    3027    3028    3029    302A    302B    302C    302D    302E    302F
    it_SGN, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB, it_CMB,
;;; 3030    3031    3032    3033    3034    3035    3036    3037    3038    3039    303A    303B    303C    303D    303E    303F
    it_SEP, it_EXT, it_EXT, it_EXT, it_EXT, it_EXT, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_SGN,
;;; 3040    3041    3042    3043    3044    3045    3046    3047    3048    3049    304A    304B    304C    304D    304E    304F
    it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 3050    3051    3052    3053    3054    3055    3056    3057    3058    3059    305A    305B    305C    305D    305E    305F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 3060    3061    3062    3063    3064    3065    3066    3067    3068    3069    306A    306B    306C    306D    306E    306F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 3070    3071    3072    3073    3074    3075    3076    3077    3078    3079    307A    307B    307C    307D    307E    307F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 3080    3081    3082    3083    3084    3085    3086    3087    3088    3089    308A    308B    308C    308D    308E    308F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 3090    3091    3092    3093    3094    3095    3096    3097    3098    3099    309A    309B    309C    309D    309E    309F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_CMB, it_EXT, it_EXT, it_EXT, it_EXT, it_NTC,
;;; 30A0    30A1    30A2    30A3    30A4    30A5    30A6    30A7    30A8    30A9    30AA    30AB    30AC    30AD    30AE    30AF
    it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 30B0    30B1    30B2    30B3    30B4    30B5    30B6    30B7    30B8    30B9    30BA    30BB    30BC    30BD    30BE    30BF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 30C0    30C1    30C2    30C3    30C4    30C5    30C6    30C7    30C8    30C9    30CA    30CB    30CC    30CD    30CE    30CF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 30D0    30D1    30D2    30D3    30D4    30D5    30D6    30D7    30D8    30D9    30DA    30DB    30DC    30DD    30DE    30DF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 30E0    30E1    30E2    30E3    30E4    30E5    30E6    30E7    30E8    30E9    30EA    30EB    30EC    30ED    30EE    30EF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 30F0    30F1    30F2    30F3    30F4    30F5    30F6    30F7    30F8    30F9    30FA    30FB    30FC    30FD    30FE    30FF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_SEP, it_EXT, it_EXT, it_EXT, it_NTC,
);

cons_it_string(
;;; 3100    3101    3102    3103    3104    3105    3106    3107    3108    3109    310A    310B    310C    310D    310E    310F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 3110    3111    3112    3113    3114    3115    3116    3117    3118    3119    311A    311B    311C    311D    311E    311F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 3120    3121    3122    3123    3124    3125    3126    3127    3128    3129    312A    312B    312C    312D    312E    312F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC,
;;; 3130    3131    3132    3133    3134    3135    3136    3137    3138    3139    313A    313B    313C    313D    313E    313F
    it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 3140    3141    3142    3143    3144    3145    3146    3147    3148    3149    314A    314B    314C    314D    314E    314F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 3150    3151    3152    3153    3154    3155    3156    3157    3158    3159    315A    315B    315C    315D    315E    315F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 3160    3161    3162    3163    3164    3165    3166    3167    3168    3169    316A    316B    316C    316D    316E    316F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 3170    3171    3172    3173    3174    3175    3176    3177    3178    3179    317A    317B    317C    317D    317E    317F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; 3180    3181    3182    3183    3184    3185    3186    3187    3188    3189    318A    318B    318C    318D    318E    318F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC,
;;; 3190    3191    3192    3193    3194    3195    3196    3197    3198    3199    319A    319B    319C    319D    319E    319F
    it_SGN, it_SGN, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP,
;;; 31A0    31A1    31A2    31A3    31A4    31A5    31A6    31A7    31A8    31A9    31AA    31AB    31AC    31AD    31AE    31AF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 31B0    31B1    31B2    31B3    31B4    31B5    31B6    31B7    31B8    31B9    31BA    31BB    31BC    31BD    31BE    31BF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 31C0    31C1    31C2    31C3    31C4    31C5    31C6    31C7    31C8    31C9    31CA    31CB    31CC    31CD    31CE    31CF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 31D0    31D1    31D2    31D3    31D4    31D5    31D6    31D7    31D8    31D9    31DA    31DB    31DC    31DD    31DE    31DF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 31E0    31E1    31E2    31E3    31E4    31E5    31E6    31E7    31E8    31E9    31EA    31EB    31EC    31ED    31EE    31EF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 31F0    31F1    31F2    31F3    31F4    31F5    31F6    31F7    31F8    31F9    31FA    31FB    31FC    31FD    31FE    31FF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; 3200    3201    3202    3203    3204    3205    3206    3207    3208    3209    320A    320B    320C    320D    320E    320F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3210    3211    3212    3213    3214    3215    3216    3217    3218    3219    321A    321B    321C    321D    321E    321F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC,
;;; 3220    3221    3222    3223    3224    3225    3226    3227    3228    3229    322A    322B    322C    322D    322E    322F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3230    3231    3232    3233    3234    3235    3236    3237    3238    3239    323A    323B    323C    323D    323E    323F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3240    3241    3242    3243    3244    3245    3246    3247    3248    3249    324A    324B    324C    324D    324E    324F
    it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 3250    3251    3252    3253    3254    3255    3256    3257    3258    3259    325A    325B    325C    325D    325E    325F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 3260    3261    3262    3263    3264    3265    3266    3267    3268    3269    326A    326B    326C    326D    326E    326F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3270    3271    3272    3273    3274    3275    3276    3277    3278    3279    327A    327B    327C    327D    327E    327F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_SGN,
;;; 3280    3281    3282    3283    3284    3285    3286    3287    3288    3289    328A    328B    328C    328D    328E    328F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3290    3291    3292    3293    3294    3295    3296    3297    3298    3299    329A    329B    329C    329D    329E    329F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 32A0    32A1    32A2    32A3    32A4    32A5    32A6    32A7    32A8    32A9    32AA    32AB    32AC    32AD    32AE    32AF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 32B0    32B1    32B2    32B3    32B4    32B5    32B6    32B7    32B8    32B9    32BA    32BB    32BC    32BD    32BE    32BF
    it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 32C0    32C1    32C2    32C3    32C4    32C5    32C6    32C7    32C8    32C9    32CA    32CB    32CC    32CD    32CE    32CF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC,
;;; 32D0    32D1    32D2    32D3    32D4    32D5    32D6    32D7    32D8    32D9    32DA    32DB    32DC    32DD    32DE    32DF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 32E0    32E1    32E2    32E3    32E4    32E5    32E6    32E7    32E8    32E9    32EA    32EB    32EC    32ED    32EE    32EF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 32F0    32F1    32F2    32F3    32F4    32F5    32F6    32F7    32F8    32F9    32FA    32FB    32FC    32FD    32FE    32FF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC,
);

cons_it_string(
;;; 3300    3301    3302    3303    3304    3305    3306    3307    3308    3309    330A    330B    330C    330D    330E    330F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3310    3311    3312    3313    3314    3315    3316    3317    3318    3319    331A    331B    331C    331D    331E    331F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3320    3321    3322    3323    3324    3325    3326    3327    3328    3329    332A    332B    332C    332D    332E    332F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3330    3331    3332    3333    3334    3335    3336    3337    3338    3339    333A    333B    333C    333D    333E    333F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3340    3341    3342    3343    3344    3345    3346    3347    3348    3349    334A    334B    334C    334D    334E    334F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3350    3351    3352    3353    3354    3355    3356    3357    3358    3359    335A    335B    335C    335D    335E    335F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3360    3361    3362    3363    3364    3365    3366    3367    3368    3369    336A    336B    336C    336D    336E    336F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3370    3371    3372    3373    3374    3375    3376    3377    3378    3379    337A    337B    337C    337D    337E    337F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3380    3381    3382    3383    3384    3385    3386    3387    3388    3389    338A    338B    338C    338D    338E    338F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 3390    3391    3392    3393    3394    3395    3396    3397    3398    3399    339A    339B    339C    339D    339E    339F
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 33A0    33A1    33A2    33A3    33A4    33A5    33A6    33A7    33A8    33A9    33AA    33AB    33AC    33AD    33AE    33AF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 33B0    33B1    33B2    33B3    33B4    33B5    33B6    33B7    33B8    33B9    33BA    33BB    33BC    33BD    33BE    33BF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 33C0    33C1    33C2    33C3    33C4    33C5    33C6    33C7    33C8    33C9    33CA    33CB    33CC    33CD    33CE    33CF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 33D0    33D1    33D2    33D3    33D4    33D5    33D6    33D7    33D8    33D9    33DA    33DB    33DC    33DD    33DE    33DF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC, it_NTC,
;;; 33E0    33E1    33E2    33E3    33E4    33E5    33E6    33E7    33E8    33E9    33EA    33EB    33EC    33ED    33EE    33EF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; 33F0    33F1    33F2    33F3    33F4    33F5    33F6    33F7    33F8    33F9    33FA    33FB    33FC    33FD    33FE    33FF
    it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_NTC,
);

;;; 3400 - 34FF
it_NTC || 16:34FFe1;

;;; 3500 - 35FF
it_NTC || 16:35FFe1;

;;; 3600 - 36FF
it_NTC || 16:36FFe1;

;;; 3700 - 37FF
it_NTC || 16:37FFe1;

;;; 3800 - 38FF
it_NTC || 16:38FFe1;

;;; 3900 - 39FF
it_NTC || 16:39FFe1;

;;; 3A00 - 3AFF
it_NTC || 16:3AFFe1;

;;; 3B00 - 3BFF
it_NTC || 16:3BFFe1;

;;; 3C00 - 3CFF
it_NTC || 16:3CFFe1;

;;; 3D00 - 3DFF
it_NTC || 16:3DFFe1;

;;; 3E00 - 3EFF
it_NTC || 16:3EFFe1;

;;; 3F00 - 3FFF
it_NTC || 16:3FFFe1;

;;; 4000 - 40FF
it_NTC || 16:40FFe1;

;;; 4100 - 41FF
it_NTC || 16:41FFe1;

;;; 4200 - 42FF
it_NTC || 16:42FFe1;

;;; 4300 - 43FF
it_NTC || 16:43FFe1;

;;; 4400 - 44FF
it_NTC || 16:44FFe1;

;;; 4500 - 45FF
it_NTC || 16:45FFe1;

;;; 4600 - 46FF
it_NTC || 16:46FFe1;

;;; 4700 - 47FF
it_NTC || 16:47FFe1;

;;; 4800 - 48FF
it_NTC || 16:48FFe1;

;;; 4900 - 49FF
it_NTC || 16:49FFe1;

;;; 4A00 - 4AFF
it_NTC || 16:4AFFe1;

;;; 4B00 - 4BFF
it_NTC || 16:4BFFe1;

;;; 4C00 - 4CFF
it_NTC || 16:4CFFe1;

;;; 4D00 - 4DFF
it_NTC || 16:4DFFe1;

;;; 4E00 - 4EFF
it_LET || 16:4EFFe1;        ;;; <CJK Ideograph, First>
;;; 4F00 - 4FFF
it_LET || 16:4FFFe1;
;;; 5000 - 50FF
it_LET || 16:50FFe1;
;;; 5100 - 51FF
it_LET || 16:51FFe1;
;;; 5200 - 52FF
it_LET || 16:52FFe1;
;;; 5300 - 53FF
it_LET || 16:53FFe1;
;;; 5400 - 54FF
it_LET || 16:54FFe1;
;;; 5500 - 55FF
it_LET || 16:55FFe1;
;;; 5600 - 56FF
it_LET || 16:56FFe1;
;;; 5700 - 57FF
it_LET || 16:57FFe1;
;;; 5800 - 58FF
it_LET || 16:58FFe1;
;;; 5900 - 59FF
it_LET || 16:59FFe1;
;;; 5A00 - 5AFF
it_LET || 16:5AFFe1;
;;; 5B00 - 5BFF
it_LET || 16:5BFFe1;
;;; 5C00 - 5CFF
it_LET || 16:5CFFe1;
;;; 5D00 - 5DFF
it_LET || 16:5DFFe1;
;;; 5E00 - 5EFF
it_LET || 16:5EFFe1;
;;; 5F00 - 5FFF
it_LET || 16:5FFFe1;
;;; 6000 - 60FF
it_LET || 16:60FFe1;
;;; 6100 - 61FF
it_LET || 16:61FFe1;
;;; 6200 - 62FF
it_LET || 16:62FFe1;
;;; 6300 - 63FF
it_LET || 16:63FFe1;
;;; 6400 - 64FF
it_LET || 16:64FFe1;
;;; 6500 - 65FF
it_LET || 16:65FFe1;
;;; 6600 - 66FF
it_LET || 16:66FFe1;
;;; 6700 - 67FF
it_LET || 16:67FFe1;
;;; 6800 - 68FF
it_LET || 16:68FFe1;
;;; 6900 - 69FF
it_LET || 16:69FFe1;
;;; 6A00 - 6AFF
it_LET || 16:6AFFe1;
;;; 6B00 - 6BFF
it_LET || 16:6BFFe1;
;;; 6C00 - 6CFF
it_LET || 16:6CFFe1;
;;; 6D00 - 6DFF
it_LET || 16:6DFFe1;
;;; 6E00 - 6EFF
it_LET || 16:6EFFe1;
;;; 6F00 - 6FFF
it_LET || 16:6FFFe1;
;;; 7000 - 70FF
it_LET || 16:70FFe1;
;;; 7100 - 71FF
it_LET || 16:71FFe1;
;;; 7200 - 72FF
it_LET || 16:72FFe1;
;;; 7300 - 73FF
it_LET || 16:73FFe1;
;;; 7400 - 74FF
it_LET || 16:74FFe1;
;;; 7500 - 75FF
it_LET || 16:75FFe1;
;;; 7600 - 76FF
it_LET || 16:76FFe1;
;;; 7700 - 77FF
it_LET || 16:77FFe1;
;;; 7800 - 78FF
it_LET || 16:78FFe1;
;;; 7900 - 79FF
it_LET || 16:79FFe1;
;;; 7A00 - 7AFF
it_LET || 16:7AFFe1;
;;; 7B00 - 7BFF
it_LET || 16:7BFFe1;
;;; 7C00 - 7CFF
it_LET || 16:7CFFe1;
;;; 7D00 - 7DFF
it_LET || 16:7DFFe1;
;;; 7E00 - 7EFF
it_LET || 16:7EFFe1;
;;; 7F00 - 7FFF
it_LET || 16:7FFFe1;
;;; 8000 - 80FF
it_LET || 16:80FFe1;
;;; 8100 - 81FF
it_LET || 16:81FFe1;
;;; 8200 - 82FF
it_LET || 16:82FFe1;
;;; 8300 - 83FF
it_LET || 16:83FFe1;
;;; 8400 - 84FF
it_LET || 16:84FFe1;
;;; 8500 - 85FF
it_LET || 16:85FFe1;
;;; 8600 - 86FF
it_LET || 16:86FFe1;
;;; 8700 - 87FF
it_LET || 16:87FFe1;
;;; 8800 - 88FF
it_LET || 16:88FFe1;
;;; 8900 - 89FF
it_LET || 16:89FFe1;
;;; 8A00 - 8AFF
it_LET || 16:8AFFe1;
;;; 8B00 - 8BFF
it_LET || 16:8BFFe1;
;;; 8C00 - 8CFF
it_LET || 16:8CFFe1;
;;; 8D00 - 8DFF
it_LET || 16:8DFFe1;
;;; 8E00 - 8EFF
it_LET || 16:8EFFe1;
;;; 8F00 - 8FFF
it_LET || 16:8FFFe1;
;;; 9000 - 90FF
it_LET || 16:90FFe1;
;;; 9100 - 91FF
it_LET || 16:91FFe1;
;;; 9200 - 92FF
it_LET || 16:92FFe1;
;;; 9300 - 93FF
it_LET || 16:93FFe1;
;;; 9400 - 94FF
it_LET || 16:94FFe1;
;;; 9500 - 95FF
it_LET || 16:95FFe1;
;;; 9600 - 96FF
it_LET || 16:96FFe1;
;;; 9700 - 97FF
it_LET || 16:97FFe1;
;;; 9800 - 98FF
it_LET || 16:98FFe1;
;;; 9900 - 99FF
it_LET || 16:99FFe1;
;;; 9A00 - 9AFF
it_LET || 16:9AFFe1;
;;; 9B00 - 9BFF
it_LET || 16:9BFFe1;
;;; 9C00 - 9CFF
it_LET || 16:9CFFe1;
;;; 9D00 - 9DFF
it_LET || 16:9DFFe1;
;;; 9E00 - 9EFF
it_LET || 16:9EFFe1;
;;; 9F00 - 9FFF
it_LET || 16:9FA5e1;        ;;; <CJK Ideograph, Last>

;;; A000 - A0FF
it_NTC || 16:A0FFe1;

;;; A100 - A1FF
it_NTC || 16:A1FFe1;

;;; A200 - A2FF
it_NTC || 16:A2FFe1;

;;; A300 - A3FF
it_NTC || 16:A3FFe1;

;;; A400 - A4FF
it_NTC || 16:A4FFe1;

;;; A500 - A5FF
it_NTC || 16:A5FFe1;

;;; A600 - A6FF
it_NTC || 16:A6FFe1;

;;; A700 - A7FF
it_NTC || 16:A7FFe1;

;;; A800 - A8FF
it_NTC || 16:A8FFe1;

;;; A900 - A9FF
it_NTC || 16:A9FFe1;

;;; AA00 - AAFF
it_NTC || 16:AAFFe1;

;;; AB00 - ABFF
it_NTC || 16:ABFFe1;

;;; AC00 - ACFF
it_LET || 16:ACFFe1;        ;;; <Hangul Syllable, First>
;;; AD00 - ADFF
it_LET || 16:ADFFe1;
;;; AE00 - AEFF
it_LET || 16:AEFFe1;
;;; AF00 - AFFF
it_LET || 16:AFFFe1;
;;; B000 - B0FF
it_LET || 16:B0FFe1;
;;; B100 - B1FF
it_LET || 16:B1FFe1;
;;; B200 - B2FF
it_LET || 16:B2FFe1;
;;; B300 - B3FF
it_LET || 16:B3FFe1;
;;; B400 - B4FF
it_LET || 16:B4FFe1;
;;; B500 - B5FF
it_LET || 16:B5FFe1;
;;; B600 - B6FF
it_LET || 16:B6FFe1;
;;; B700 - B7FF
it_LET || 16:B7FFe1;
;;; B800 - B8FF
it_LET || 16:B8FFe1;
;;; B900 - B9FF
it_LET || 16:B9FFe1;
;;; BA00 - BAFF
it_LET || 16:BAFFe1;
;;; BB00 - BBFF
it_LET || 16:BBFFe1;
;;; BC00 - BCFF
it_LET || 16:BCFFe1;
;;; BD00 - BDFF
it_LET || 16:BDFFe1;
;;; BE00 - BEFF
it_LET || 16:BEFFe1;
;;; BF00 - BFFF
it_LET || 16:BFFFe1;
;;; C000 - C0FF
it_LET || 16:C0FFe1;
;;; C100 - C1FF
it_LET || 16:C1FFe1;
;;; C200 - C2FF
it_LET || 16:C2FFe1;
;;; C300 - C3FF
it_LET || 16:C3FFe1;
;;; C400 - C4FF
it_LET || 16:C4FFe1;
;;; C500 - C5FF
it_LET || 16:C5FFe1;
;;; C600 - C6FF
it_LET || 16:C6FFe1;
;;; C700 - C7FF
it_LET || 16:C7FFe1;
;;; C800 - C8FF
it_LET || 16:C8FFe1;
;;; C900 - C9FF
it_LET || 16:C9FFe1;
;;; CA00 - CAFF
it_LET || 16:CAFFe1;
;;; CB00 - CBFF
it_LET || 16:CBFFe1;
;;; CC00 - CCFF
it_LET || 16:CCFFe1;
;;; CD00 - CDFF
it_LET || 16:CDFFe1;
;;; CE00 - CEFF
it_LET || 16:CEFFe1;
;;; CF00 - CFFF
it_LET || 16:CFFFe1;
;;; D000 - D0FF
it_LET || 16:D0FFe1;
;;; D100 - D1FF
it_LET || 16:D1FFe1;
;;; D200 - D2FF
it_LET || 16:D2FFe1;
;;; D300 - D3FF
it_LET || 16:D3FFe1;
;;; D400 - D4FF
it_LET || 16:D4FFe1;
;;; D500 - D5FF
it_LET || 16:D5FFe1;
;;; D600 - D6FF
it_LET || 16:D6FFe1;
;;; D700 - D7FF
it_LET || 16:D7A3e1;        ;;; <Hangul Syllable, Last>

;;; D800 - D8FF
it_NTC || 16:D8FFe1;        ;;; <Unassigned High Surrogate, First>
;;; D900 - D9FF
it_NTC || 16:D9FFe1;
;;; DA00 - DAFF
it_NTC || 16:DAFFe1;
;;; DB00 - DBFF
it_NTC || 16:DBFFe1;        ;;; Private High Surrogate starts DB80

;;; DC00 - DCFF
it_NTC || 16:DCFFe1;        ;;; <Low Surrogate, First>
;;; DD00 - DDFF
it_NTC || 16:DDFFe1;
;;; DE00 - DEFF
it_NTC || 16:DEFFe1;
;;; DF00 - DFFF
it_NTC || 16:DFFFe1;        ;;; <Low Surrogate, Last>

;;; E000 - E0FF
it_SEP || 16:E0FFe1;        ;;; <Private Use, First>
;;; E100 - E1FF
it_SEP || 16:E1FFe1;
;;; E200 - E2FF
it_SEP || 16:E2FFe1;
;;; E300 - E3FF
it_SEP || 16:E3FFe1;
;;; E400 - E4FF
it_SEP || 16:E4FFe1;
;;; E500 - E5FF
it_SEP || 16:E5FFe1;
;;; E600 - E6FF
it_SEP || 16:E6FFe1;
;;; E700 - E7FF
it_SEP || 16:E7FFe1;
;;; E800 - E8FF
it_SEP || 16:E8FFe1;
;;; E900 - E9FF
it_SEP || 16:E9FFe1;
;;; EA00 - EAFF
it_SEP || 16:EAFFe1;
;;; EB00 - EBFF
it_SEP || 16:EBFFe1;
;;; EC00 - ECFF
it_SEP || 16:ECFFe1;
;;; ED00 - EDFF
it_SEP || 16:EDFFe1;
;;; EE00 - EEFF
it_SEP || 16:EEFFe1;
;;; EF00 - EFFF
it_SEP || 16:EFFFe1;
;;; F000 - F0FF
it_SEP || 16:F0FFe1;
;;; F100 - F1FF
it_SEP || 16:F1FFe1;
;;; F200 - F2FF
it_SEP || 16:F2FFe1;
;;; F300 - F3FF
it_SEP || 16:F3FFe1;
;;; F400 - F4FF
it_SEP || 16:F4FFe1;
;;; F500 - F5FF
it_SEP || 16:F5FFe1;
;;; F600 - F6FF
it_SEP || 16:F6FFe1;
;;; F700 - F7FF
it_SEP || 16:F7FFe1;
;;; F800 - F8FF
it_SEP || 16:F8FFe1;        ;;; <Private Use, Last>

;;; F900 - F9FF
it_LET || 16:F9FFe1;        ;;; <CJK Compatibility Ideograph, First>
;;; FA00 - FAFF
it_LET || 16:FA2De1;        ;;; <CJK Compatibility Ideograph, Last>

cons_it_string(
;;; FB00    FB01    FB02    FB03    FB04    FB05    FB06    FB07    FB08    FB09    FB0A    FB0B    FB0C    FB0D    FB0E    FB0F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; FB10    FB11    FB12    FB13    FB14    FB15    FB16    FB17    FB18    FB19    FB1A    FB1B    FB1C    FB1D    FB1E    FB1F
    it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_CMB, it_LET,
;;; FB20    FB21    FB22    FB23    FB24    FB25    FB26    FB27    FB28    FB29    FB2A    FB2B    FB2C    FB2D    FB2E    FB2F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_SGN, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FB30    FB31    FB32    FB33    FB34    FB35    FB36    FB37    FB38    FB39    FB3A    FB3B    FB3C    FB3D    FB3E    FB3F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_LET, it_NTC,
;;; FB40    FB41    FB42    FB43    FB44    FB45    FB46    FB47    FB48    FB49    FB4A    FB4B    FB4C    FB4D    FB4E    FB4F
    it_LET, it_LET, it_NTC, it_LET, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FB50    FB51    FB52    FB53    FB54    FB55    FB56    FB57    FB58    FB59    FB5A    FB5B    FB5C    FB5D    FB5E    FB5F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FB60    FB61    FB62    FB63    FB64    FB65    FB66    FB67    FB68    FB69    FB6A    FB6B    FB6C    FB6D    FB6E    FB6F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FB70    FB71    FB72    FB73    FB74    FB75    FB76    FB77    FB78    FB79    FB7A    FB7B    FB7C    FB7D    FB7E    FB7F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FB80    FB81    FB82    FB83    FB84    FB85    FB86    FB87    FB88    FB89    FB8A    FB8B    FB8C    FB8D    FB8E    FB8F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FB90    FB91    FB92    FB93    FB94    FB95    FB96    FB97    FB98    FB99    FB9A    FB9B    FB9C    FB9D    FB9E    FB9F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FBA0    FBA1    FBA2    FBA3    FBA4    FBA5    FBA6    FBA7    FBA8    FBA9    FBAA    FBAB    FBAC    FBAD    FBAE    FBAF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FBB0    FBB1    FBB2    FBB3    FBB4    FBB5    FBB6    FBB7    FBB8    FBB9    FBBA    FBBB    FBBC    FBBD    FBBE    FBBF
    it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; FBC0    FBC1    FBC2    FBC3    FBC4    FBC5    FBC6    FBC7    FBC8    FBC9    FBCA    FBCB    FBCC    FBCD    FBCE    FBCF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; FBD0    FBD1    FBD2    FBD3    FBD4    FBD5    FBD6    FBD7    FBD8    FBD9    FBDA    FBDB    FBDC    FBDD    FBDE    FBDF
    it_NTC, it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FBE0    FBE1    FBE2    FBE3    FBE4    FBE5    FBE6    FBE7    FBE8    FBE9    FBEA    FBEB    FBEC    FBED    FBEE    FBEF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FBF0    FBF1    FBF2    FBF3    FBF4    FBF5    FBF6    FBF7    FBF8    FBF9    FBFA    FBFB    FBFC    FBFD    FBFE    FBFF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
);

;;; FC00 - FCFF
it_LET || 16:FCFFe1;

cons_it_string(
;;; FD00    FD01    FD02    FD03    FD04    FD05    FD06    FD07    FD08    FD09    FD0A    FD0B    FD0C    FD0D    FD0E    FD0F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FD10    FD11    FD12    FD13    FD14    FD15    FD16    FD17    FD18    FD19    FD1A    FD1B    FD1C    FD1D    FD1E    FD1F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FD20    FD21    FD22    FD23    FD24    FD25    FD26    FD27    FD28    FD29    FD2A    FD2B    FD2C    FD2D    FD2E    FD2F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FD30    FD31    FD32    FD33    FD34    FD35    FD36    FD37    FD38    FD39    FD3A    FD3B    FD3C    FD3D    FD3E    FD3F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_SEP, it_SEP,
;;; FD40    FD41    FD42    FD43    FD44    FD45    FD46    FD47    FD48    FD49    FD4A    FD4B    FD4C    FD4D    FD4E    FD4F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; FD50    FD51    FD52    FD53    FD54    FD55    FD56    FD57    FD58    FD59    FD5A    FD5B    FD5C    FD5D    FD5E    FD5F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FD60    FD61    FD62    FD63    FD64    FD65    FD66    FD67    FD68    FD69    FD6A    FD6B    FD6C    FD6D    FD6E    FD6F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FD70    FD71    FD72    FD73    FD74    FD75    FD76    FD77    FD78    FD79    FD7A    FD7B    FD7C    FD7D    FD7E    FD7F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FD80    FD81    FD82    FD83    FD84    FD85    FD86    FD87    FD88    FD89    FD8A    FD8B    FD8C    FD8D    FD8E    FD8F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FD90    FD91    FD92    FD93    FD94    FD95    FD96    FD97    FD98    FD99    FD9A    FD9B    FD9C    FD9D    FD9E    FD9F
    it_NTC, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FDA0    FDA1    FDA2    FDA3    FDA4    FDA5    FDA6    FDA7    FDA8    FDA9    FDAA    FDAB    FDAC    FDAD    FDAE    FDAF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FDB0    FDB1    FDB2    FDB3    FDB4    FDB5    FDB6    FDB7    FDB8    FDB9    FDBA    FDBB    FDBC    FDBD    FDBE    FDBF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FDC0    FDC1    FDC2    FDC3    FDC4    FDC5    FDC6    FDC7    FDC8    FDC9    FDCA    FDCB    FDCC    FDCD    FDCE    FDCF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; FDD0    FDD1    FDD2    FDD3    FDD4    FDD5    FDD6    FDD7    FDD8    FDD9    FDDA    FDDB    FDDC    FDDD    FDDE    FDDF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; FDE0    FDE1    FDE2    FDE3    FDE4    FDE5    FDE6    FDE7    FDE8    FDE9    FDEA    FDEB    FDEC    FDED    FDEE    FDEF
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; FDF0    FDF1    FDF2    FDF3    FDF4    FDF5    FDF6    FDF7    FDF8    FDF9    FDFA    FDFB    FDFC    FDFD    FDFE    FDFF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_NTC, it_NTC,
);

cons_it_string(
;;; FE00    FE01    FE02    FE03    FE04    FE05    FE06    FE07    FE08    FE09    FE0A    FE0B    FE0C    FE0D    FE0E    FE0F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; FE10    FE11    FE12    FE13    FE14    FE15    FE16    FE17    FE18    FE19    FE1A    FE1B    FE1C    FE1D    FE1E    FE1F
    it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; FE20    FE21    FE22    FE23    FE24    FE25    FE26    FE27    FE28    FE29    FE2A    FE2B    FE2C    FE2D    FE2E    FE2F
    it_CMB, it_CMB, it_CMB, it_CMB, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC, it_NTC,
;;; FE30    FE31    FE32    FE33    FE34    FE35    FE36    FE37    FE38    FE39    FE3A    FE3B    FE3C    FE3D    FE3E    FE3F
    it_SEP, it_SEP, it_SEP, it_SGN, it_SGN, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP,
;;; FE40    FE41    FE42    FE43    FE44    FE45    FE46    FE47    FE48    FE49    FE4A    FE4B    FE4C    FE4D    FE4E    FE4F
    it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_NTC, it_NTC, it_NTC, it_NTC, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN, it_SGN,
;;; FE50    FE51    FE52    FE53    FE54    FE55    FE56    FE57    FE58    FE59    FE5A    FE5B    FE5C    FE5D    FE5E    FE5F
    it_SEP, it_SEP, it_SEP, it_NTC, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SEP, it_SGN,
;;; FE60    FE61    FE62    FE63    FE64    FE65    FE66    FE67    FE68    FE69    FE6A    FE6B    FE6C    FE6D    FE6E    FE6F
    it_SGN, it_SGN, it_SGN, it_SEP, it_SGN, it_SGN, it_SGN, it_NTC, it_SEP, it_SGN, it_SEP, it_SGN, it_NTC, it_NTC, it_NTC, it_NTC,
;;; FE70    FE71    FE72    FE73    FE74    FE75    FE76    FE77    FE78    FE79    FE7A    FE7B    FE7C    FE7D    FE7E    FE7F
    it_LET, it_LET, it_LET, it_NTC, it_LET, it_NTC, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FE80    FE81    FE82    FE83    FE84    FE85    FE86    FE87    FE88    FE89    FE8A    FE8B    FE8C    FE8D    FE8E    FE8F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FE90    FE91    FE92    FE93    FE94    FE95    FE96    FE97    FE98    FE99    FE9A    FE9B    FE9C    FE9D    FE9E    FE9F
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FEA0    FEA1    FEA2    FEA3    FEA4    FEA5    FEA6    FEA7    FEA8    FEA9    FEAA    FEAB    FEAC    FEAD    FEAE    FEAF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FEB0    FEB1    FEB2    FEB3    FEB4    FEB5    FEB6    FEB7    FEB8    FEB9    FEBA    FEBB    FEBC    FEBD    FEBE    FEBF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FEC0    FEC1    FEC2    FEC3    FEC4    FEC5    FEC6    FEC7    FEC8    FEC9    FECA    FECB    FECC    FECD    FECE    FECF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FED0    FED1    FED2    FED3    FED4    FED5    FED6    FED7    FED8    FED9    FEDA    FEDB    FEDC    FEDD    FEDE    FEDF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FEE0    FEE1    FEE2    FEE3    FEE4    FEE5    FEE6    FEE7    FEE8    FEE9    FEEA    FEEB    FEEC    FEED    FEEE    FEEF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET,
;;; FEF0    FEF1    FEF2    FEF3    FEF4    FEF5    FEF6    FEF7    FEF8    FEF9    FEFA    FEFB    FEFC    FEFD    FEFE    FEFF
    it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_LET, it_NTC, it_NTC, it_SP,
);

false;      ;;; means translate FF01 - FF5E to ASCII, and use FF_block for
            ;;; FF5F - FFFF

lconstant unichar_vec = consvector(255);


    /*  Translations for characters FF5F - FFFF. A value greater than
        ITM_LAST is taken to be an equivalent character to map to.
    */
lconstant FF_vec = consvector(
;;; FF5F
    IT_NTC,
;;; FF60    FF61    FF62    FF63    FF64    FF65    FF66    FF67    FF68    FF69    FF6A    FF6B    FF6C    FF6D    FF6E    FF6F
    IT_NTC, 16:3002,16:300C,16:300D,16:3001,16:30FB,16:30F2,16:30A1,16:30A3,16:30A5,16:30A7,16:30A9,16:30E3,16:30E5,16:30E7,16:30C3,
;;; FF70    FF71    FF72    FF73    FF74    FF75    FF76    FF77    FF78    FF79    FF7A    FF7B    FF7C    FF7D    FF7E    FF7F
    16:30FC,16:30A2,16:30A4,16:30A6,16:30A8,16:30AA,16:30AB,16:30AD,16:30AF,16:30B1,16:30B3,16:30B5,16:30B7,16:30B9,16:30BB,16:30BD,
;;; FF80    FF81    FF82    FF83    FF84    FF85    FF86    FF87    FF88    FF89    FF8A    FF8B    FF8C    FF8D    FF8E    FF8F
    16:30BF,16:30C1,16:30C4,16:30C6,16:30C8,16:30CA,16:30CB,16:30CC,16:30CD,16:30CE,16:30CF,16:30D2,16:30D5,16:30D8,16:30DB,16:30DE,
;;; FF90    FF91    FF92    FF93    FF94    FF95    FF96    FF97    FF98    FF99    FF9A    FF9B    FF9C    FF9D    FF9E    FF9F
    16:30DF,16:30E0,16:30E1,16:30E2,16:30E4,16:30E6,16:30E8,16:30E9,16:30EA,16:30EB,16:30EC,16:30ED,16:30EF,16:30F3,16:309B,16:309C,
;;; FFA0    FFA1    FFA2    FFA3    FFA4    FFA5    FFA6    FFA7    FFA8    FFA9    FFAA    FFAB    FFAC    FFAD    FFAE    FFAF
    16:3164,16:3131,16:3132,16:3133,16:3134,16:3135,16:3136,16:3137,16:3138,16:3139,16:313A,16:313B,16:313C,16:313D,16:313E,16:313F,
;;; FFB0    FFB1    FFB2    FFB3    FFB4    FFB5    FFB6    FFB7    FFB8    FFB9    FFBA    FFBB    FFBC    FFBD    FFBE    FFBF
    16:3140,16:3141,16:3142,16:3143,16:3144,16:3145,16:3146,16:3147,16:3148,16:3149,16:314A,16:314B,16:314C,16:314D,16:314E,IT_NTC,
;;; FFC0    FFC1    FFC2    FFC3    FFC4    FFC5    FFC6    FFC7    FFC8    FFC9    FFCA    FFCB    FFCC    FFCD    FFCE    FFCF
    IT_NTC, IT_NTC, 16:314F,16:3150,16:3151,16:3152,16:3153,16:3154,IT_NTC, IT_NTC, 16:3155,16:3156,16:3157,16:3158,16:3159,16:315A,
;;; FFD0    FFD1    FFD2    FFD3    FFD4    FFD5    FFD6    FFD7    FFD8    FFD9    FFDA    FFDB    FFDC    FFDD    FFDE    FFDF
    IT_NTC, IT_NTC, 16:315B,16:315C,16:315D,16:315E,16:315F,16:3160,IT_NTC, IT_NTC, 16:3161,16:3162,16:3163,IT_NTC, IT_NTC, IT_NTC,
;;; FFE0    FFE1    FFE2    FFE3    FFE4    FFE5    FFE6    FFE7    FFE8    FFE9    FFEA    FFEB    FFEC    FFED    FFEE    FFEF
    16:00A2,16:00A3,16:00AC,16:00AF,16:00A6,16:00A5,16:20A9,IT_NTC, 16:2502,16:2190,16:2191,16:2192,16:2193,16:25A0,16:25CB,IT_NTC,
;;; FFF0    FFF1    FFF2    FFF3    FFF4    FFF5    FFF6    FFF7    FFF8    FFF9    FFFA    FFFB    FFFC    FFFD    FFFE    FFFF
    IT_NTC, IT_NTC, IT_NTC, IT_NTC, IT_NTC, IT_NTC, IT_NTC, IT_NTC, IT_NTC, IT_NTC, IT_NTC, IT_NTC, IT_NTC, IT_SGN, IT_NTC, IT_NTC,
161);

    /*  struct RAW_SHORTS and FF_block can be removed when corepop11 supports
        string16s; FF_vec can then be renamed FF_block and the consvector
        changed to consstring.
    */
struct RAW_SHORTS
  { word    RAW_SIZE;       ;;; size as word offset
    full    KEY;
>-> short   RAW_SHORTS[];
  };

lconstant FF_block = struct RAW_SHORTS =>>
      {%
        @@(w)[_int(datalength(FF_vec)) | s.r] _sub @@POPBASE,
        rawstruct_key,
        =>> {% appdata(FF_vec, _int) %}
      %};



    /*  Unicode characters that are 0 digits (in reverse order,
        and excluding `0`).
    */
lconstant zero_digits = {
    16:FF10 16:0F20 16:0ED0 16:0E50 16:0D66 16:0CE6 16:0C66 16:0BE6
    16:0B66 16:0AE6 16:0A66 16:09E6 16:0966 16:06F0 16:0660
    0           ;;; marks end
};



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 30 1998
        Put values in zero_digits the right way round!
 */
