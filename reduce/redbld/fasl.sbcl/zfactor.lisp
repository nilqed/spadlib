(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ZFACTOR)) 
(EXPORTS (LIST 'NEXTPRIME 'PRIMEP 'ZFACTOR 'ZFACTOR1 'NROOTNN)) 
(IMPORTS
 (LIST 'EVENP 'GCDN 'GENERAL-MODULAR-EXPT 'GENERAL-MODULAR-TIMES 'IDIFFERENCE
       'IGREATERP 'ILESSP 'IPLUS2 'IROOT 'ISQRT 'LEQ 'MODULAR-EXPT
       'MODULAR-TIMES 'NEQ 'PREPF 'PRIN2T 'RANDOM 'REVERSIP
       'SET-GENERAL-MODULUS 'SET-MODULUS 'SET-SMALL-MODULUS 'TYPERR)) 
(FLUID '(*MAXTRYS*)) 
(SETQ *MAXTRYS* 10) 
(GLOBAL
 '(*LAST-PRIME-SQUARED* *PRIMELIST* *LAST-PRIME-IN-LIST* LARGEST-SMALL-MODULUS)) 
(SETQ *PRIMELIST*
        '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89
          97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179
          181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271
          277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379
          383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479
          487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599
          601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701
          709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823
          827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941
          947 953 967 971 977 983 991 997 1009 1013 1019 1021 1031 1033 1039
          1049 1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129
          1151 1153 1163 1171 1181 1187 1193 1201 1213 1217 1223 1229 1231 1237
          1249 1259 1277 1279 1283 1289 1291 1297 1301 1303 1307 1319 1321 1327
          1361 1367 1373 1381 1399 1409 1423 1427 1429 1433 1439 1447 1451 1453
          1459 1471 1481 1483 1487 1489 1493 1499 1511 1523 1531 1543 1549 1553
          1559 1567 1571 1579 1583 1597 1601 1607 1609 1613 1619 1621 1627 1637
          1657 1663 1667 1669 1693 1697 1699 1709 1721 1723 1733 1741 1747 1753
          1759 1777 1783 1787 1789 1801 1811 1823 1831 1847 1861 1867 1871 1873
          1877 1879 1889 1901 1907 1913 1931 1933 1949 1951 1973 1979 1987 1993
          1997 1999 2003 2011 2017 2027 2029 2039 2053 2063 2069 2081 2083 2087
          2089 2099 2111 2113 2129 2131 2137 2141 2143 2153 2161 2179 2203 2207
          2213 2221 2237 2239 2243 2251 2267 2269 2273 2281 2287 2293 2297 2309
          2311 2333 2339 2341 2347 2351 2357 2371 2377 2381 2383 2389 2393 2399
          2411 2417 2423 2437 2441 2447 2459 2467 2473 2477 2503 2521 2531 2539
          2543 2549 2551 2557 2579 2591 2593 2609 2617 2621 2633 2647 2657 2659
          2663 2671 2677 2683 2687 2689 2693 2699 2707 2711 2713 2719 2729 2731
          2741 2749 2753 2767 2777 2789 2791 2797 2801 2803 2819 2833 2837 2843
          2851 2857 2861 2879 2887 2897 2903 2909 2917 2927 2939 2953 2957 2963
          2969 2971 2999 3001 3011 3019 3023 3037 3041 3049 3061 3067 3079 3083
          3089 3109 3119 3121 3137 3163 3167 3169 3181 3187 3191 3203 3209 3217
          3221 3229 3251 3253 3257 3259 3271 3299 3301 3307 3313 3319 3323 3329
          3331 3343 3347 3359 3361 3371 3373 3389 3391 3407 3413 3433 3449 3457
          3461 3463 3467 3469 3491 3499 3511 3517 3527 3529 3533 3539 3541 3547
          3557 3559 3571)) 
(SETQ *LAST-PRIME-IN-LIST* (CAR (REVERSE *PRIMELIST*))) 
(SETQ *LAST-PRIME-SQUARED* (EXPT *LAST-PRIME-IN-LIST* 2)) 
(FLAG '(EVENP) 'OPFN) 
(PUT 'ADD-FACTOR 'NUMBER-OF-ARGS 2) 
(PUT 'ADD-FACTOR 'DEFINED-ON-LINE '118) 
(PUT 'ADD-FACTOR 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'ADD-FACTOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADD-FACTOR (N L)
    ((LAMBDA (P)
       (COND (P (PROGN (RPLACD P (ADD1 (CDR P))) L)) (T (CONS (CONS N 1) L))))
     (COND ((PAIRP L) (COND ((GREATERP N (CAAR L)) NIL) (T (ASSOC N L))))
           (T NIL)))) 
(PUT 'ZFACTOR 'NUMBER-OF-ARGS 1) 
(PUT 'ZFACTOR 'DEFINED-ON-LINE '122) 
(PUT 'ZFACTOR 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'ZFACTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZFACTOR (N) (ZFACTOR1 N T)) 
(PUT 'ZFACTOR1 'NUMBER-OF-ARGS 2) 
(PUT 'ZFACTOR1 'DEFINED-ON-LINE '124) 
(PUT 'ZFACTOR1 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'ZFACTOR1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ZFACTOR1 (N BOOL)
    (COND ((LESSP N 0) (CONS (CONS (MINUS 1) 1) (ZFACTOR1 (MINUS N) BOOL)))
          ((LESSP N 4) (LIST (CONS N 1)))
          (T
           (PROG (PRIMELIST FACTOR-LIST P QR)
             (SETQ PRIMELIST *PRIMELIST*)
             (SETQ FACTOR-LIST NIL)
             (PROG ()
              WHILELABEL
               (COND ((NOT PRIMELIST) (RETURN NIL)))
               (PROGN
                (SETQ P (CAR PRIMELIST))
                (SETQ PRIMELIST (CDR PRIMELIST))
                (PROG ()
                 WHILELABEL
                  (COND
                   ((NOT (EQUAL (CDR (SETQ QR (DIVIDE N P))) 0)) (RETURN NIL)))
                  (PROGN
                   (SETQ N (CAR QR))
                   (SETQ FACTOR-LIST (ADD-FACTOR P FACTOR-LIST)))
                  (GO WHILELABEL))
                (COND
                 ((AND (NEQ N 1) (GREATERP (TIMES P P) N))
                  (PROGN
                   (SETQ PRIMELIST NIL)
                   (SETQ FACTOR-LIST (ADD-FACTOR N FACTOR-LIST))
                   (SETQ N 1)))))
               (GO WHILELABEL))
             (RETURN
              (COND ((EQUAL N 1) FACTOR-LIST)
                    ((NULL BOOL) (CONS (CONS N 1) FACTOR-LIST))
                    (T (MCFACTOR* N FACTOR-LIST)))))))) 
(PUT 'MCFACTOR* 'NUMBER-OF-ARGS 2) 
(PUT 'MCFACTOR* 'DEFINED-ON-LINE '144) 
(PUT 'MCFACTOR* 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'MCFACTOR* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MCFACTOR* (N FACTORS-SO-FAR)
    (COND ((INTERNAL-PRIMEP N) (ADD-FACTOR N FACTORS-SO-FAR))
          (T
           (PROGN
            (SETQ N
                    ((LAMBDA (P TRIES)
                       (PROGN
                        (PROG ()
                         WHILELABEL
                          (COND
                           ((NOT (AND (ATOM P) (LESSP TRIES *MAXTRYS*)))
                            (RETURN NIL)))
                          (PROGN
                           (SETQ TRIES (PLUS TRIES 1))
                           (SETQ P (MCFACTOR N TRIES)))
                          (GO WHILELABEL))
                        (COND
                         ((GREATERP TRIES *MAXTRYS*)
                          (PROGN
                           (PRIN2 "ZFACTOR(mcfactor!*): Assuming ")
                           (PRIN2 N)
                           (PRIN2T " is prime")
                           (SETQ P (LIST N))))
                         (T P))))
                     (MCFACTOR N 1) 1))
            (COND ((ATOM N) (ADD-FACTOR N FACTORS-SO-FAR))
                  ((LESSP (CAR N) (CDR N))
                   (MCFACTOR* (CDR N) (MCFACTOR* (CAR N) FACTORS-SO-FAR)))
                  (T
                   (MCFACTOR* (CAR N) (MCFACTOR* (CDR N) FACTORS-SO-FAR)))))))) 
(PUT 'MCFACTOR 'NUMBER-OF-ARGS 2) 
(PUT 'MCFACTOR 'DEFINED-ON-LINE '161) 
(PUT 'MCFACTOR 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'MCFACTOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MCFACTOR (N P)
    (PROG (GG K M Q R X Y YS)
      (SETQ M 20)
      (SETQ Y 0)
      (SETQ R (SETQ Q 1))
     OUTER
      (SETQ X Y)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (SETQ Y (REMAINDER (PLUS (TIMES Y Y) P) N))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ K 0)
     INNER
      (SETQ YS Y)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((MINUSP
           (DIFFERENCE
            (COND ((LESSP M (DIFFERENCE R K)) M) (T (DIFFERENCE R K))) I))
          (RETURN NIL)))
        (PROGN
         (SETQ Y (REMAINDER (PLUS (TIMES Y Y) P) N))
         (SETQ Q (REMAINDER (TIMES Q (ABS (DIFFERENCE X Y))) N)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ GG (GCDN Q N))
      (SETQ K (PLUS K M))
      (COND ((AND (LESSP K R) (LEQ GG 1)) (GO INNER)))
      (SETQ R (TIMES 2 R))
      (COND ((LEQ GG 1) (GO OUTER)))
      (COND
       ((EQUAL GG N)
        (PROG ()
         LOOP
          (SETQ YS (REMAINDER (PLUS (TIMES YS YS) P) N))
          (SETQ GG (GCDN (ABS (DIFFERENCE X YS)) N))
          (COND ((LEQ GG 1) (GO LOOP))))))
      (RETURN (COND ((EQUAL GG N) N) (T (CONS GG (QUOTIENT N GG))))))) 
(GLOBAL '(HAS-PRIMEP64*)) 
(SETQ HAS-PRIMEP64* (NOT (NULL (GETD 'PRIMEP64)))) 
(PUT 'PRIMEP 'NUMBER-OF-ARGS 1) 
(PUT 'PRIMEP 'DEFINED-ON-LINE '196) 
(PUT 'PRIMEP 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'PRIMEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIMEP (N)
    (COND ((NOT (FIXP N)) (TYPERR N "integer"))
          ((LESSP N 0) (PRIMEP (MINUS N)))
          ((AND HAS-PRIMEP64* (LESSP N 18446744073709551616)) (PRIMEP64 N))
          ((AND (LEQ N 4294967295) (LEQ N LARGEST-SMALL-MODULUS)) (PRIMEP32 N))
          ((OR (EVENP N) (EQUAL (REMAINDER N 3) 0) (EQUAL (REMAINDER N 5) 0)
               (EQUAL (REMAINDER N 7) 0))
           NIL)
          ((NOT (GENERAL-MILLER-RABIN 2 N)) NIL)
          ((LESSP N 18446744073709551616)
           (AND (GENERAL-MILLER-RABIN 325 N) (GENERAL-MILLER-RABIN 9375 N)
                (GENERAL-MILLER-RABIN 28178 N) (GENERAL-MILLER-RABIN 450775 N)
                (GENERAL-MILLER-RABIN 9780504 N)
                (GENERAL-MILLER-RABIN 1795265022 N)))
          (T (LUCAS_TEST N)))) 
(FLAG '(PRIMEP) 'BOOLEAN) 
(PUT 'INTERNAL-PRIMEP 'NUMBER-OF-ARGS 1) 
(PUT 'INTERNAL-PRIMEP 'DEFINED-ON-LINE '233) 
(PUT 'INTERNAL-PRIMEP 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'INTERNAL-PRIMEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTERNAL-PRIMEP (N) (PRIMEP N)) 
(PUT 'LSD 'NUMBER-OF-ARGS 1) 
(PUT 'LSD 'DEFINED-ON-LINE '259) 
(PUT 'LSD 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'LSD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LSD (N)
    (PROG (R)
      (COND ((EQUAL N 0) (RETURN 0)))
      (SETQ R 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL (LAND N 18446744073709551615) 0)) (RETURN NIL)))
        (PROGN (SETQ N (ASHIFT N (MINUS 64))) (SETQ R (PLUS R 64)))
        (GO WHILELABEL))
      (COND
       ((EQUAL (LAND N 4294967295) 0)
        (PROGN (SETQ N (ASHIFT N (MINUS 32))) (SETQ R (PLUS R 32)))))
      (COND
       ((EQUAL (LAND N 65535) 0)
        (PROGN (SETQ N (ASHIFT N (MINUS 16))) (SETQ R (PLUS R 16)))))
      (COND
       ((EQUAL (LAND N 255) 0)
        (PROGN (SETQ N (ASHIFT N (MINUS 8))) (SETQ R (PLUS R 8)))))
      (COND
       ((EQUAL (LAND N 15) 0)
        (PROGN (SETQ N (ASHIFT N (MINUS 4))) (SETQ R (PLUS R 4)))))
      (COND
       ((EQUAL (LAND N 3) 0)
        (PROGN (SETQ N (ASHIFT N (MINUS 2))) (SETQ R (PLUS R 2)))))
      (COND ((EQUAL (LAND N 1) 0) (SETQ R (PLUS R 1))))
      (RETURN R))) 
(FLAG '(LSD) 'RLISP) 
(PUT 'LOGBITP 'NUMBER-OF-ARGS 2) 
(PUT 'LOGBITP 'DEFINED-ON-LINE '287) 
(PUT 'LOGBITP 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'LOGBITP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LOGBITP (I N) (NOT (ZEROP (LAND N (ASHIFT 1 I))))) 
(FLAG '(LOGBITP) 'RLISP) 
(PUT 'INTEGER-LENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'INTEGER-LENGTH 'DEFINED-ON-LINE '313) 
(PUT 'INTEGER-LENGTH 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'INTEGER-LENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTEGER-LENGTH (N)
    (PROG (R)
      (COND ((LESSP N 0) (SETQ N (DIFFERENCE (MINUS N) 1))))
      (COND ((EQUAL N 0) (RETURN 0)))
      (SETQ R 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (GEQ N 18446744073709551616)) (RETURN NIL)))
        (PROGN (SETQ N (ASHIFT N (MINUS 64))) (SETQ R (PLUS R 64)))
        (GO WHILELABEL))
      (COND
       ((GEQ N 4294967296)
        (PROGN (SETQ N (ASHIFT N (MINUS 32))) (SETQ R (PLUS R 32)))))
      (COND
       ((GEQ N 65536)
        (PROGN (SETQ N (ASHIFT N (MINUS 16))) (SETQ R (PLUS R 16)))))
      (COND
       ((GEQ N 256) (PROGN (SETQ N (ASHIFT N (MINUS 8))) (SETQ R (PLUS R 8)))))
      (COND
       ((GEQ N 16) (PROGN (SETQ N (ASHIFT N (MINUS 4))) (SETQ R (PLUS R 4)))))
      (COND
       ((GEQ N 4) (PROGN (SETQ N (ASHIFT N (MINUS 2))) (SETQ R (PLUS R 2)))))
      (COND ((GEQ N 2) (SETQ R (PLUS R 1))))
      (RETURN R))) 
(FLAG '(INTEGER-LENGTH) 'RLISP) 
(GLOBAL '(WITNESS-TABLE ODDPRIME-BITMAP)) 
(SETQ ODDPRIME-BITMAP
        (LIST-TO-VECTOR
         '(1689570158 2171409050 2253016114 563511821 1510225097 2760466720
           696652100 1244168913 843202608 137664801 1109722699 344606852
           1815101957 188789768 307300512 1694796072 2152476824 2148680854
           1091715617 3223389385 9973042 134498560 2187892353 570959280
           1084393800 2416191065 809713670 1761645124 134775312 306253220
           604029024 140583634 2182808708 17879809 3225722882 345071650
           78004236 153130194 5382292 1285689352 1359053312 2760574992
           1144035877 55316609 2152867074 545540360 402666064 605110434
           25428264 172072961 905987346 690356232 3227615875 269485184
           1210187885 3255576152 612976656 1159747616 419439752 281027842
           18090088 2150118090 642450432 1620054784))) 
(SETQ WITNESS-TABLE
        (LIST-TO-VECTOR
         '(17490 5756 7143 10476 13223 5143 54949 46324 11327 21776 14 11348
           1837 11945 17130 814 24668 27778 29292 12320 27999 24665 217 2136
           370 15513 11577 11464 9734 5117 4796 11231 1760 9541 13930 1807
           10976 11212 46077 10885 13981 148 415 4031 26689 9236 2257 14300 183
           6148 31088 7970 6283 556 2674 6442 3501 17049 20938 44337 7812 4627
           21294 6768 5134 40093 4662 774 12178 10453 16975 20017 3405 32346
           11745 294 14936 20713 3371 13471 3728 4090 40339 57759 22007 1115
           24211 10564 13850 11754 2278 5745 16753 51913 13076 1160 2581 13858
           13147 1072 44224 5022 1417 19493 39737 6276 6792 4207 6345 40285
           23786 51941 4542 3302 9249 6428 35246 4981 9628 9231 23685 15481
           2335 34333 27605 11926 6602 6167 2161 6073 10601 4248 46263 2678
           6247 8332 5569 4439 50964 2326 17596 1511 43893 11640 2691 40811
           4676 32329 3214 18961 9118 3713 41097 4067 9690 8901 3074 67 3153
           985 33378 8698 16533 41199 47465 47912 21939 21286 652 21348 12998
           3723 1294 8768 7897 60772 9880 25647 5644 1481 16626 1608 16379
           25558 176 5553 17031 9330 6323 2764 5798 4108 6234 51499 19125 1845
           22910 9111 5817 55318 2221 7784 13964 46759 3442 14692 6748 6657
           7293 1576 330 27166 1625 10388 16052 6421))) 
(FLUID '(*TRACE_PRIMEP)) 
(SETQ *TRACE_PRIMEP NIL) 
(PUT 'PRIMEP32 'NUMBER-OF-ARGS 1) 
(PUT 'PRIMEP32 'DEFINED-ON-LINE '386) 
(PUT 'PRIMEP32 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'PRIMEP32 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIMEP32 (N)
    (PROG (L M X Y W SAVE RESULT)
      (SETQ L 0)
      (SETQ M 0)
      (SETQ X 0)
      (SETQ Y 0)
      (SETQ W 0)
      (SETQ SAVE 0)
      (COND ((LESSP N 0) (SETQ N (MINUS N))))
      (COND
       ((LEQ N 4096)
        (PROGN
         (COND
          (*TRACE_PRIMEP
           (PRINTF_INTERNAL "%fTesting %w. <= 4096 so use table lookup%n"
                            (LIST N))))
         (COND ((EVENP N) (RETURN (EQUAL N 2)))
               (T
                (RETURN
                 (LOGBITP (REMAINDER (QUOTIENT N 2) 32)
                          (GETV ODDPRIME-BITMAP (QUOTIENT N 64))))))))
       ((LOGBITP (REMAINDER N 42) 2058857338845) (RETURN NIL))
       ((LOGBITP (REMAINDER N 55) 1179820038851617) (RETURN NIL)))
      (SETQ W (TIMES 10074622553898968683 N))
      (SETQ W (LAND (ASHIFT W (MINUS 31)) 4294967295))
      (SETQ W (REMAINDER W 216))
      (COND
       (*TRACE_PRIMEP
        (PRINTF_INTERNAL "%fTesting %w which hashes to %w," (LIST N W))))
      (SETQ W (GETV WITNESS-TABLE W))
      (COND (*TRACE_PRIMEP (PRINTF_INTERNAL " so use base %w%n" (LIST W))))
      (SETQ SAVE (SET-SMALL-MODULUS N))
      (SETQ M (DIFFERENCE N 1))
      (SETQ L (SUB1 (LSD M)))
      (SETQ M (QUOTIENT M (ASHIFT 1 L)))
      (SETQ X (MODULAR-EXPT W M))
      (COND
       (*TRACE_PRIMEP
        (PRINTF_INTERNAL "%w = %w * %w, and %w^%w = %w%n"
                         (LIST (DIFFERENCE N 1) M (ASHIFT 1 L) W M X))))
      (SETQ RESULT T)
      (COND
       ((NEQ X 1)
        (PROGN
         (PROG (K)
           (SETQ K 1)
          LAB
           (COND ((MINUSP (DIFFERENCE L K)) (RETURN NIL)))
           (PROGN
            (SETQ Y (REMAINDER (TIMES X X) CURRENT-MODULUS))
            (COND
             (*TRACE_PRIMEP
              (PROGN
               (SETQ M (TIMES 2 M))
               (PRINTF_INTERNAL "%w^%w = %w%n" (LIST W M Y)))))
            (COND
             ((AND (EQUAL Y 1) (NEQ X (DIFFERENCE N 1)) (NEQ X 1))
              (SETQ RESULT NIL))
             (T (SETQ X Y))))
           (SETQ K (PLUS2 K 1))
           (GO LAB))
         (COND ((NEQ X 1) (SETQ RESULT NIL))))))
      (SET-SMALL-MODULUS SAVE)
      (COND (*TRACE_PRIMEP (PRINTF_INTERNAL "result is %w%n" (LIST RESULT))))
      (RETURN RESULT))) 
(PUT 'GENERAL-MILLER-RABIN 'NUMBER-OF-ARGS 2) 
(PUT 'GENERAL-MILLER-RABIN 'DEFINED-ON-LINE '455) 
(PUT 'GENERAL-MILLER-RABIN 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'GENERAL-MILLER-RABIN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GENERAL-MILLER-RABIN (W N)
    (PROG (M SAVE L RESULT X Y)
      (COND
       (*TRACE_PRIMEP
        (PRINTF_INTERNAL "%fGeneral M-R test on %w using base %w%n"
                         (LIST M W))))
      (SETQ M (DIFFERENCE N 1))
      (SETQ SAVE (SET-GENERAL-MODULUS N))
      (SETQ L 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (EVENP M)) (RETURN NIL)))
        (PROGN (SETQ M (QUOTIENT M 2)) (SETQ L (PLUS L 1)))
        (GO WHILELABEL))
      (SETQ RESULT T)
      (SETQ X (GENERAL-MODULAR-EXPT W M))
      (COND (*TRACE_PRIMEP (PRINTF_INTERNAL "%w^%w = %w%n" (LIST W M X))))
      (COND
       ((NEQ X 1)
        (PROGN
         (PROG (K)
           (SETQ K 1)
          LAB
           (COND ((MINUSP (DIFFERENCE L K)) (RETURN NIL)))
           (PROGN
            (SETQ Y (GENERAL-MODULAR-TIMES X X))
            (COND
             (*TRACE_PRIMEP
              (PROGN
               (SETQ M (TIMES 2 M))
               (PRINTF_INTERNAL "%w^%w = %w%n" (LIST W M Y)))))
            (COND
             ((AND (EQUAL Y 1) (NEQ X (DIFFERENCE N 1)) (NEQ X 1))
              (SETQ RESULT NIL))
             (T (SETQ X Y))))
           (SETQ K (PLUS2 K 1))
           (GO LAB))
         (COND ((NEQ X 1) (SETQ RESULT NIL))))))
      (SET-GENERAL-MODULUS SAVE)
      (COND (*TRACE_PRIMEP (PRINTF_INTERNAL "result = %w%n" (LIST RESULT))))
      (RETURN RESULT))) 
(PUT 'GENERAL-PRIMEP 'NUMBER-OF-ARGS 1) 
(PUT 'GENERAL-PRIMEP 'DEFINED-ON-LINE '491) 
(PUT 'GENERAL-PRIMEP 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'GENERAL-PRIMEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENERAL-PRIMEP (N)
    (PROG ()
      (COND ((LESSP N 0) (SETQ N (MINUS N))))
      (COND
       ((OR (EVENP N) (EQUAL (REMAINDER N 3) 0) (EQUAL (REMAINDER N 5) 0)
            (EQUAL (REMAINDER N 7) 0))
        (RETURN NIL)))
      (COND ((NOT (GENERAL-MILLER-RABIN 2 N)) (RETURN NIL)))
      (COND
       ((LESSP N 18446744073709551616)
        (PROGN
         (COND ((NOT (GENERAL-MILLER-RABIN 325 N)) (RETURN NIL)))
         (COND ((NOT (GENERAL-MILLER-RABIN 325 N)) (RETURN NIL)))
         (COND ((NOT (GENERAL-MILLER-RABIN 9375 N)) (RETURN NIL)))
         (COND ((NOT (GENERAL-MILLER-RABIN 28178 N)) (RETURN NIL)))
         (COND ((NOT (GENERAL-MILLER-RABIN 9780504 N)) (RETURN NIL)))
         (COND ((NOT (GENERAL-MILLER-RABIN 1795265022 N)) (RETURN NIL)))
         (RETURN T))))
      (RETURN (LUCAS_TEST N)))) 
(PUT 'JACOBI-SYMBOL 'NUMBER-OF-ARGS 2) 
(PUT 'JACOBI-SYMBOL 'DEFINED-ON-LINE '553) 
(PUT 'JACOBI-SYMBOL 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'JACOBI-SYMBOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE JACOBI-SYMBOL (A B)
    (COND ((OR (LEQ B 0) (EVENP B)) 0)
          (T
           (PROG (J R)
             (SETQ J 1)
             (COND
              ((LESSP A 0)
               (PROGN
                (SETQ A (MINUS A))
                (COND ((EQUAL (LAND B 3) 3) (SETQ J (MINUS J)))))))
             (PROG ()
              WHILELABEL
               (COND ((NOT (NOT (ZEROP A))) (RETURN NIL)))
               (PROGN
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (EVENP A)) (RETURN NIL)))
                  (PROGN
                   (SETQ A (QUOTIENT A 2))
                   (COND
                    ((OR (EQUAL (SETQ R (LAND B 7)) 3) (EQUAL R 5))
                     (SETQ J (MINUS J)))))
                  (GO WHILELABEL))
                (SETQ R A)
                (SETQ A B)
                (SETQ B R)
                (COND
                 ((AND (EQUAL (LAND A 3) 3) (EQUAL (LAND B 3) 3))
                  (SETQ J (MINUS J))))
                (SETQ A (REMAINDER A B)))
               (GO WHILELABEL))
             (COND ((EQUAL B 1) (RETURN J)) (T (RETURN 0))))))) 
(FLAG '(JACOBI-SYMBOL) 'RLISP) 
(PUT 'IS-PERFECT-SQUARE 'NUMBER-OF-ARGS 1) 
(PUT 'IS-PERFECT-SQUARE 'DEFINED-ON-LINE '586) 
(PUT 'IS-PERFECT-SQUARE 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'IS-PERFECT-SQUARE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IS-PERFECT-SQUARE (N) ((LAMBDA (R) (EQUAL N (TIMES R R))) (ISQRT N))) 
(FLAG '(IS-PERFECT-SQUARE) 'RLISP) 
(PUT 'LUCAS_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'LUCAS_TEST 'DEFINED-ON-LINE '597) 
(PUT 'LUCAS_TEST 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'LUCAS_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LUCAS_TEST (C)
    (PROG (D J K KK U V Q QK L LL TMP SAVEMOD)
      (SETQ D 5)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (GREATERP (SETQ J (JACOBI-SYMBOL D C)) 0)
                (OR (NEQ D 21) (NOT (IS-PERFECT-SQUARE C)))))
          (RETURN NIL)))
        (COND ((GREATERP D 0) (SETQ D (DIFFERENCE (MINUS D) 2)))
              (T (SETQ D (PLUS (MINUS D) 2))))
        (GO WHILELABEL))
      (COND
       (*TRACE_PRIMEP
        (PRINTF_INTERNAL "%fTest %w using D=%w, j=%w%n" (LIST C D J))))
      (COND ((GEQ J 0) (RETURN (AND (EQUAL C (ABS D)) (PRIMEP32 C)))))
      (SETQ Q (QUOTIENT (DIFFERENCE 1 D) 4))
      (COND (*TRACE_PRIMEP (PRINTF_INTERNAL "will use P=1, Q=%w%n" (LIST Q))))
      (COND ((NEQ (GCDN C Q) 1) (RETURN NIL)))
      (SETQ K (PLUS C 1))
      (SETQ SAVEMOD (SET-GENERAL-MODULUS C))
      (SETQ U 1)
      (SETQ V 1)
      (SETQ QK (SETQ Q (GENERAL-MODULAR-NUMBER Q)))
      (SETQ D (GENERAL-MODULAR-NUMBER D))
      (SETQ L (SUB1 (INTEGER-LENGTH K)))
      (SETQ LL (SUB1 (LSD K)))
      (COND
       (*TRACE_PRIMEP
        (PRINTF_INTERNAL "k=%w uses %w bits and has %w trailing zero bits%n"
                         (LIST K (ADD1 L) LL))))
      (SETQ KK 1)
      (COND (*TRACE_PRIMEP (PRINTF_INTERNAL "1:     [1, 1]%n" (LIST))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GEQ (SETQ L (DIFFERENCE L 1)) LL)) (RETURN NIL)))
        (PROGN
         (SETQ U (GENERAL-MODULAR-TIMES U V))
         (SETQ V
                 (GENERAL-MODULAR-DIFFERENCE (GENERAL-MODULAR-TIMES V V)
                                             (GENERAL-MODULAR-TIMES 2 QK)))
         (SETQ KK (TIMES 2 KK))
         (SETQ QK (GENERAL-MODULAR-TIMES QK QK))
         (COND
          (*TRACE_PRIMEP
           (PRINTF_INTERNAL "%f%w: %t[%w, %w] q^k=%w%n" (LIST KK 7 U V QK))))
         (COND
          (*TRACE_PRIMEP
           (PRINTF_INTERNAL "(A) l=%w ll=%w k=%w logbit=%w%n"
                            (LIST L LL K (LOGBITP L K)))))
         (COND
          ((LOGBITP L K)
           (PROGN
            (SETQ TMP (GENERAL-MODULAR-PLUS U V))
            (SETQ V (GENERAL-MODULAR-PLUS (GENERAL-MODULAR-TIMES D U) V))
            (SETQ U TMP)
            (COND ((NOT (EVENP U)) (SETQ U (PLUS U C))))
            (SETQ U (QUOTIENT U 2))
            (COND ((NOT (EVENP V)) (SETQ V (PLUS V C))))
            (SETQ V (QUOTIENT V 2))
            (SETQ KK (PLUS KK 1))
            (SETQ QK (GENERAL-MODULAR-TIMES Q QK))
            (COND
             (*TRACE_PRIMEP
              (PRINTF_INTERNAL "%f%w: %t[%w, %w] q^k=%w%n"
                               (LIST KK 7 U V QK))))))))
        (GO WHILELABEL))
      (COND
       ((EQUAL U 0)
        (PROGN
         (COND
          (*TRACE_PRIMEP
           (PRINTF_INTERNAL "u=0 so value is probably prime%n" (LIST))))
         (SET-GENERAL-MODULUS SAVEMOD)
         (RETURN T))))
      (COND
       (*TRACE_PRIMEP
        (PROGN
         (PRINTF_INTERNAL "After final non-doubling step u = %w%n" (LIST U))
         (PRINTF_INTERNAL "Will just do doubling steps from now on...%n"
                          (LIST)))))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NEQ V 0) (GEQ (SETQ L (DIFFERENCE L 1)) 0)))
          (RETURN NIL)))
        (PROGN
         (SETQ V
                 (GENERAL-MODULAR-DIFFERENCE (GENERAL-MODULAR-TIMES V V)
                                             (GENERAL-MODULAR-TIMES 2 QK)))
         (SETQ KK (TIMES 2 KK))
         (SETQ QK (GENERAL-MODULAR-TIMES QK QK))
         (COND
          (*TRACE_PRIMEP
           (PRINTF_INTERNAL "%f%w: %t[??, %w] qk=%w%n" (LIST KK 7 V QK))))
         (COND
          (*TRACE_PRIMEP
           (PRINTF_INTERNAL "(B) l=%w ll=%w k=%w logbit=%w%n"
                            (LIST L LL K (LOGBITP L K)))))
         NIL)
        (GO WHILELABEL))
      (COND
       (*TRACE_PRIMEP
        (PRINTF_INTERNAL "exit loop with l = %w and v = %w%n" (LIST L V))))
      (SET-GENERAL-MODULUS SAVEMOD)
      (RETURN (EQUAL V 0)))) 
(PUT 'NEXTPRIME 'NUMBER-OF-ARGS 1) 
(PUT 'NEXTPRIME 'DEFINED-ON-LINE '764) 
(PUT 'NEXTPRIME 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'NEXTPRIME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEXTPRIME (P)
    (COND
     ((OR (NULL P) (EQUAL P 0) (EQUAL P 1) (EQUAL P (MINUS 1))
          (EQUAL P (MINUS 2)))
      2)
     ((EQUAL P (MINUS 3)) (MINUS 2))
     ((NOT (FIXP P)) (TYPERR (PREPF P) "integer"))
     (T
      (PROG ()
        (COND ((EVENP P) (SETQ P (PLUS P 1))) (T (SETQ P (PLUS P 2))))
        (PROG ()
         WHILELABEL
          (COND ((NOT (NOT (PRIMEP P))) (RETURN NIL)))
          (SETQ P (PLUS P 2))
          (GO WHILELABEL))
        (RETURN P))))) 
(PUT 'NEXTPRIME 'POLYFN 'NEXTPRIME) 
(PUT 'NROOTNN 'NUMBER-OF-ARGS 2) 
(PUT 'NROOTNN 'DEFINED-ON-LINE '779) 
(PUT 'NROOTNN 'DEFINED-IN-FILE 'ALG/ZFACTOR.RED) 
(PUT 'NROOTNN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NROOTNN (N X)
    (PROG (PL SIGNN QR W R S P Q)
      (SETQ R 0)
      (SETQ S 0)
      (SETQ P 0)
      (SETQ Q 0)
      (SETQ R 1)
      (SETQ S 1)
      (COND
       ((LESSP N 0)
        (PROGN
         (SETQ N (MINUS N))
         (COND ((EVENP X) (SETQ SIGNN T)) (T (SETQ R (MINUS 1)))))))
      (SETQ PL *PRIMELIST*)
     LOOP
      (SETQ P (CAR PL))
      (SETQ PL (CDR PL))
      (SETQ Q 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL (CDR (SETQ QR (DIVIDE N P))) 0)) (RETURN NIL)))
        (PROGN (SETQ N (CAR QR)) (SETQ Q (IPLUS2 Q 1)))
        (GO WHILELABEL))
      (COND
       ((NOT (ILESSP Q X))
        (PROGN
         (SETQ W (DIVIDE Q X))
         (SETQ R (TIMES R (EXPT P (CAR W))))
         (SETQ Q (CDR W)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (IGREATERP Q 0)) (RETURN NIL)))
        (PROGN (SETQ S (TIMES S P)) (SETQ Q (IDIFFERENCE Q 1)))
        (GO WHILELABEL))
      (COND ((LESSP (CAR QR) P) (PROGN (SETQ S (TIMES N S)) (GO DONE))))
      (COND (PL (GO LOOP)))
      (COND
       ((GREATERP (EXPT 10 20) N)
        (PROGN
         (SETQ Q (MCFACTOR* N NIL))
         (PROG (J)
           (SETQ J Q)
          LAB
           (COND ((NULL J) (RETURN NIL)))
           ((LAMBDA (J)
              (PROGN
               (SETQ W (DIVIDE (CDR J) X))
               (SETQ R (TIMES (EXPT (CAR J) (CAR W)) R))
               (SETQ S (TIMES (EXPT (CAR J) (CDR W)) S))))
            (CAR J))
           (SETQ J (CDR J))
           (GO LAB))
         NIL))
       ((SETQ Q (IROOT N X)) (SETQ R (TIMES R Q))) (T (SETQ S (TIMES N S))))
     DONE
      (COND (SIGNN (SETQ S (MINUS S))))
      (RETURN (CONS R S)))) 
(ENDMODULE) 