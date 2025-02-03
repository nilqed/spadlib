(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFVSLISTS)) 
(REVISION 'OFSFVSLISTS
          "$Id: ofsfvslists.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFVSLISTS "(c) 2015-2017 M. Kosta, T. Sturm") 
(FLUID '(RLCLUSTERING*)) 
(FLUID '(RLMAXDEG*)) 
(SETQ RLCLUSTERING* T) 
(SETQ RLMAXDEG* 3) 
(FLUID '(DEG-TYPE-CODE-ALIST*)) 
(FLUID '(RSL-ALIST*)) 
(FLUID '(RSL-ALIST-CLUSTERING*)) 
(FLUID '(GUARD-FNALIST*)) 
(FLUID '(VSUB-FNALIST*)) 
(SETQ DEG-TYPE-CODE-ALIST*
        '(((1 -1 0 1) . 1) ((1 1 0 -1) . -1) ((2 1 0 -1 0 1) . 1)
          ((2 1 0 1) . 2) ((2 -1 0 1 0 -1) . -1) ((2 -1 0 -1) . -2)
          ((3 -1 0 1) . 1) ((3 -1 0 -1 0 1) . 2) ((3 -1 0 1 0 1) . 3)
          ((3 -1 0 1 0 -1 0 1) . 4) ((3 1 0 -1) . -1) ((3 1 0 1 0 -1) . -2)
          ((3 1 0 -1 0 -1) . -3) ((3 1 0 -1 0 1 0 -1) . -4))) 
(SETQ RSL-ALIST*
        '(((1 -1 EQUAL) (IP ((-1 . 1))))
          ((1 NIL EQUAL) (IP ((1 . 1)) ((-1 . 1))))
          ((1 1 EQUAL) (IP ((1 . 1)))) ((1 -1 NEQ) (EP ((-1 . 1))))
          ((1 NIL NEQ) (EP ((1 . 1)) ((-1 . 1)))) ((1 1 NEQ) (EP ((1 . 1))))
          ((1 -1 LESSP) (SLB ((-1 . 1))))
          ((1 NIL LESSP) (SLB ((-1 . 1))) (SUB ((1 . 1))))
          ((1 1 LESSP) (SUB ((1 . 1)))) ((1 -1 LEQ) (WLB ((-1 . 1))))
          ((1 NIL LEQ) (WLB ((-1 . 1))) (WUB ((1 . 1))))
          ((1 1 LEQ) (WUB ((1 . 1)))) ((1 -1 GEQ) (WUB ((-1 . 1))))
          ((1 NIL GEQ) (WLB ((1 . 1))) (WUB ((-1 . 1))))
          ((1 1 GEQ) (WLB ((1 . 1)))) ((1 -1 GREATERP) (SUB ((-1 . 1))))
          ((1 NIL GREATERP) (SLB ((1 . 1))) (SUB ((-1 . 1))))
          ((1 1 GREATERP) (SLB ((1 . 1))))
          ((2 -1 EQUAL) (IP ((-1 . 1)) ((-1 . 2)) ((-2 . 1))))
          ((2 NIL EQUAL)
           (IP ((1 . 1)) ((1 . 2)) ((2 . 1)) ((-1 . 1)) ((-1 . 2)) ((-2 . 1))))
          ((2 1 EQUAL) (IP ((1 . 1)) ((1 . 2)) ((2 . 1))))
          ((2 -1 NEQ) (EP ((-1 . 1)) ((-1 . 2)) ((-2 . 1))))
          ((2 NIL NEQ)
           (EP ((1 . 1)) ((1 . 2)) ((2 . 1)) ((-1 . 1)) ((-1 . 2)) ((-2 . 1))))
          ((2 1 NEQ) (EP ((1 . 1)) ((1 . 2)) ((2 . 1))))
          ((2 -1 LESSP) (EP ((-2 . 1))) (SLB ((-1 . 2))) (SUB ((-1 . 1))))
          ((2 NIL LESSP) (EP ((-2 . 1))) (SLB ((1 . 1)) ((-1 . 2)))
           (SUB ((1 . 2)) ((-1 . 1))))
          ((2 1 LESSP) (SLB ((1 . 1))) (SUB ((1 . 2))))
          ((2 -1 LEQ) (WLB ((-1 . 2))) (WUB ((-1 . 1))))
          ((2 NIL LEQ) (IP ((2 . 1))) (WLB ((1 . 1)) ((-1 . 2)))
           (WUB ((1 . 2)) ((-1 . 1))))
          ((2 1 LEQ) (IP ((2 . 1))) (WLB ((1 . 1))) (WUB ((1 . 2))))
          ((2 -1 GEQ) (IP ((-2 . 1))) (WLB ((-1 . 1))) (WUB ((-1 . 2))))
          ((2 NIL GEQ) (IP ((-2 . 1))) (WLB ((1 . 2)) ((-1 . 1)))
           (WUB ((1 . 1)) ((-1 . 2))))
          ((2 1 GEQ) (WLB ((1 . 2))) (WUB ((1 . 1))))
          ((2 -1 GREATERP) (SLB ((-1 . 1))) (SUB ((-1 . 2))))
          ((2 NIL GREATERP) (EP ((2 . 1))) (SLB ((1 . 2)) ((-1 . 1)))
           (SUB ((1 . 1)) ((-1 . 2))))
          ((2 1 GREATERP) (EP ((2 . 1))) (SLB ((1 . 2))) (SUB ((1 . 1))))
          ((3 -1 EQUAL)
           (IP ((-1 . 1)) ((-2 . 1)) ((-2 . 2)) ((-3 . 1)) ((-3 . 2))
            ((-4 . 1)) ((-4 . 2)) ((-4 . 3))))
          ((3 NIL EQUAL)
           (IP ((1 . 1)) ((2 . 1)) ((2 . 2)) ((3 . 1)) ((3 . 2)) ((4 . 1))
            ((4 . 2)) ((4 . 3)) ((-1 . 1)) ((-2 . 1)) ((-2 . 2)) ((-3 . 1))
            ((-3 . 2)) ((-4 . 1)) ((-4 . 2)) ((-4 . 3))))
          ((3 1 EQUAL)
           (IP ((1 . 1)) ((2 . 1)) ((2 . 2)) ((3 . 1)) ((3 . 2)) ((4 . 1))
            ((4 . 2)) ((4 . 3))))
          ((3 -1 NEQ)
           (EP ((-1 . 1)) ((-2 . 1)) ((-2 . 2)) ((-3 . 1)) ((-3 . 2))
            ((-4 . 1)) ((-4 . 2)) ((-4 . 3))))
          ((3 NIL NEQ)
           (EP ((1 . 1)) ((2 . 1)) ((2 . 2)) ((3 . 1)) ((3 . 2)) ((4 . 1))
            ((4 . 2)) ((4 . 3)) ((-1 . 1)) ((-2 . 1)) ((-2 . 2)) ((-3 . 1))
            ((-3 . 2)) ((-4 . 1)) ((-4 . 2)) ((-4 . 3))))
          ((3 1 NEQ)
           (EP ((1 . 1)) ((2 . 1)) ((2 . 2)) ((3 . 1)) ((3 . 2)) ((4 . 1))
            ((4 . 2)) ((4 . 3))))
          ((3 -1 LESSP) (EP ((-3 . 2)))
           (SLB ((-1 . 1)) ((-2 . 2)) ((-3 . 1)) ((-4 . 1)) ((-4 . 3)))
           (SUB ((-4 . 2))))
          ((3 NIL LESSP) (EP ((2 . 1)) ((-3 . 2)))
           (SLB ((4 . 2)) ((-1 . 1)) ((-2 . 2)) ((-3 . 1)) ((-4 . 1))
            ((-4 . 3)))
           (SUB ((1 . 1)) ((2 . 2)) ((3 . 1)) ((4 . 1)) ((4 . 3)) ((-4 . 2))))
          ((3 1 LESSP) (EP ((2 . 1))) (SLB ((4 . 2)))
           (SUB ((1 . 1)) ((2 . 2)) ((3 . 1)) ((4 . 1)) ((4 . 3))))
          ((3 -1 LEQ) (IP ((-2 . 1)))
           (WLB ((-1 . 1)) ((-2 . 2)) ((-3 . 1)) ((-4 . 1)) ((-4 . 3)))
           (WUB ((-4 . 2))))
          ((3 NIL LEQ) (IP ((3 . 2)) ((-2 . 1)))
           (WLB ((4 . 2)) ((-1 . 1)) ((-2 . 2)) ((-3 . 1)) ((-4 . 1))
            ((-4 . 3)))
           (WUB ((1 . 1)) ((2 . 2)) ((3 . 1)) ((4 . 1)) ((4 . 3)) ((-4 . 2))))
          ((3 1 LEQ) (IP ((3 . 2))) (WLB ((4 . 2)))
           (WUB ((1 . 1)) ((2 . 2)) ((3 . 1)) ((4 . 1)) ((4 . 3))))
          ((3 -1 GEQ) (IP ((-3 . 2))) (WLB ((-4 . 2)))
           (WUB ((-1 . 1)) ((-2 . 2)) ((-3 . 1)) ((-4 . 1)) ((-4 . 3))))
          ((3 NIL GEQ) (IP ((2 . 1)) ((-3 . 2)))
           (WLB ((1 . 1)) ((2 . 2)) ((3 . 1)) ((4 . 1)) ((4 . 3)) ((-4 . 2)))
           (WUB ((4 . 2)) ((-1 . 1)) ((-2 . 2)) ((-3 . 1)) ((-4 . 1))
            ((-4 . 3))))
          ((3 1 GEQ) (IP ((2 . 1)))
           (WLB ((1 . 1)) ((2 . 2)) ((3 . 1)) ((4 . 1)) ((4 . 3)))
           (WUB ((4 . 2))))
          ((3 -1 GREATERP) (EP ((-2 . 1))) (SLB ((-4 . 2)))
           (SUB ((-1 . 1)) ((-2 . 2)) ((-3 . 1)) ((-4 . 1)) ((-4 . 3))))
          ((3 NIL GREATERP) (EP ((3 . 2)) ((-2 . 1)))
           (SLB ((1 . 1)) ((2 . 2)) ((3 . 1)) ((4 . 1)) ((4 . 3)) ((-4 . 2)))
           (SUB ((4 . 2)) ((-1 . 1)) ((-2 . 2)) ((-3 . 1)) ((-4 . 1))
            ((-4 . 3))))
          ((3 1 GREATERP) (EP ((3 . 2)))
           (SLB ((1 . 1)) ((2 . 2)) ((3 . 1)) ((4 . 1)) ((4 . 3)))
           (SUB ((4 . 2)))))) 
(SETQ RSL-ALIST-CLUSTERING*
        '(((1 -1 EQUAL) (IP ((-1 . 1))))
          ((1 NIL EQUAL) (IP ((1 . 1) (-1 . 1)))) ((1 1 EQUAL) (IP ((1 . 1))))
          ((1 -1 NEQ) (EP ((-1 . 1)))) ((1 NIL NEQ) (EP ((1 . 1) (-1 . 1))))
          ((1 1 NEQ) (EP ((1 . 1)))) ((1 -1 LESSP) (SLB ((-1 . 1))))
          ((1 NIL LESSP) (SLB ((-1 . 1))) (SUB ((1 . 1))))
          ((1 1 LESSP) (SUB ((1 . 1)))) ((1 -1 LEQ) (WLB ((-1 . 1))))
          ((1 NIL LEQ) (WLB ((-1 . 1))) (WUB ((1 . 1))))
          ((1 1 LEQ) (WUB ((1 . 1)))) ((1 -1 GEQ) (WUB ((-1 . 1))))
          ((1 NIL GEQ) (WLB ((1 . 1))) (WUB ((-1 . 1))))
          ((1 1 GEQ) (WLB ((1 . 1)))) ((1 -1 GREATERP) (SUB ((-1 . 1))))
          ((1 NIL GREATERP) (SLB ((1 . 1))) (SUB ((-1 . 1))))
          ((1 1 GREATERP) (SLB ((1 . 1))))
          ((2 -1 EQUAL) (IP ((-1 . 2) (-2 . 1)) ((-1 . 1) (-2 . 1))))
          ((2 NIL EQUAL)
           (IP ((1 . 1) (2 . 1) (-1 . 2) (-2 . 1))
            ((1 . 2) (2 . 1) (-1 . 1) (-2 . 1))))
          ((2 1 EQUAL) (IP ((1 . 1) (2 . 1)) ((1 . 2) (2 . 1))))
          ((2 -1 NEQ) (EP ((-1 . 2) (-2 . 1)) ((-1 . 1) (-2 . 1))))
          ((2 NIL NEQ)
           (EP ((1 . 1) (2 . 1) (-1 . 2) (-2 . 1))
            ((1 . 2) (2 . 1) (-1 . 1) (-2 . 1))))
          ((2 1 NEQ) (EP ((1 . 1) (2 . 1)) ((1 . 2) (2 . 1))))
          ((2 -1 LESSP) (SLB ((-1 . 2) (-2 . 1))) (SUB ((-1 . 1) (-2 . 1))))
          ((2 NIL LESSP) (SLB ((1 . 1) (2 . 1) (-1 . 2) (-2 . 1)))
           (SUB ((1 . 2) (2 . 1) (-1 . 1) (-2 . 1))))
          ((2 1 LESSP) (SLB ((1 . 1) (2 . 1))) (SUB ((1 . 2) (2 . 1))))
          ((2 -1 LEQ) (WLB ((-1 . 2) (-2 . 1))) (WUB ((-1 . 1) (-2 . 1))))
          ((2 NIL LEQ) (WLB ((1 . 1) (2 . 1) (-1 . 2) (-2 . 1)))
           (WUB ((1 . 2) (2 . 1) (-1 . 1) (-2 . 1))))
          ((2 1 LEQ) (WLB ((1 . 1) (2 . 1))) (WUB ((1 . 2) (2 . 1))))
          ((2 -1 GEQ) (WLB ((-1 . 1) (-2 . 1))) (WUB ((-1 . 2) (-2 . 1))))
          ((2 NIL GEQ) (WLB ((1 . 2) (2 . 1) (-1 . 1) (-2 . 1)))
           (WUB ((1 . 1) (2 . 1) (-1 . 2) (-2 . 1))))
          ((2 1 GEQ) (WLB ((1 . 2) (2 . 1))) (WUB ((1 . 1) (2 . 1))))
          ((2 -1 GREATERP) (SLB ((-1 . 1) (-2 . 1))) (SUB ((-1 . 2) (-2 . 1))))
          ((2 NIL GREATERP) (SLB ((1 . 2) (2 . 1) (-1 . 1) (-2 . 1)))
           (SUB ((1 . 1) (2 . 1) (-1 . 2) (-2 . 1))))
          ((2 1 GREATERP) (SLB ((1 . 2) (2 . 1))) (SUB ((1 . 1) (2 . 1))))
          ((3 -1 EQUAL)
           (IP ((-1 . 1)) ((-2 . 1) (-3 . 1) (-4 . 1))
            ((-2 . 1) (-3 . 2) (-4 . 2)) ((-2 . 2) (-3 . 2) (-4 . 3))))
          ((3 NIL EQUAL)
           (IP ((1 . 1)) ((2 . 1) (3 . 1) (4 . 1)) ((2 . 1) (3 . 2) (4 . 2))
            ((2 . 2) (3 . 2) (4 . 3)) ((-1 . 1)) ((-2 . 1) (-3 . 1) (-4 . 1))
            ((-2 . 1) (-3 . 2) (-4 . 2)) ((-2 . 2) (-3 . 2) (-4 . 3))))
          ((3 1 EQUAL)
           (IP ((1 . 1)) ((2 . 1) (3 . 1) (4 . 1)) ((2 . 1) (3 . 2) (4 . 2))
            ((2 . 2) (3 . 2) (4 . 3))))
          ((3 -1 NEQ)
           (EP ((-1 . 1)) ((-2 . 1) (-3 . 1) (-4 . 1))
            ((-2 . 1) (-3 . 2) (-4 . 2)) ((-2 . 2) (-3 . 2) (-4 . 3))))
          ((3 NIL NEQ)
           (EP ((1 . 1)) ((2 . 1) (3 . 1) (4 . 1)) ((2 . 1) (3 . 2) (4 . 2))
            ((2 . 2) (3 . 2) (4 . 3)) ((-1 . 1)) ((-2 . 1) (-3 . 1) (-4 . 1))
            ((-2 . 1) (-3 . 2) (-4 . 2)) ((-2 . 2) (-3 . 2) (-4 . 3))))
          ((3 1 NEQ)
           (EP ((1 . 1)) ((2 . 1) (3 . 1) (4 . 1)) ((2 . 1) (3 . 2) (4 . 2))
            ((2 . 2) (3 . 2) (4 . 3))))
          ((3 -1 LESSP)
           (SLB ((-1 . 1)) ((-2 . 1) (-3 . 1) (-4 . 1))
            ((-2 . 2) (-3 . 2) (-4 . 3)))
           (SUB ((-2 . 1) (-3 . 2) (-4 . 2))))
          ((3 NIL LESSP)
           (SLB ((2 . 1) (3 . 2) (4 . 2)) ((-1 . 1))
            ((-2 . 1) (-3 . 1) (-4 . 1)) ((-2 . 2) (-3 . 2) (-4 . 3)))
           (SUB ((1 . 1)) ((2 . 1) (3 . 1) (4 . 1)) ((2 . 2) (3 . 2) (4 . 3))
            ((-2 . 1) (-3 . 2) (-4 . 2))))
          ((3 1 LESSP) (SLB ((2 . 1) (3 . 2) (4 . 2)))
           (SUB ((1 . 1)) ((2 . 1) (3 . 1) (4 . 1)) ((2 . 2) (3 . 2) (4 . 3))))
          ((3 -1 LEQ)
           (WLB ((-1 . 1)) ((-2 . 1) (-3 . 1) (-4 . 1))
            ((-2 . 2) (-3 . 2) (-4 . 3)))
           (WUB ((-2 . 1) (-3 . 2) (-4 . 2))))
          ((3 NIL LEQ)
           (WLB ((2 . 1) (3 . 2) (4 . 2)) ((-1 . 1))
            ((-2 . 1) (-3 . 1) (-4 . 1)) ((-2 . 2) (-3 . 2) (-4 . 3)))
           (WUB ((1 . 1)) ((2 . 1) (3 . 1) (4 . 1)) ((2 . 2) (3 . 2) (4 . 3))
            ((-2 . 1) (-3 . 2) (-4 . 2))))
          ((3 1 LEQ) (WLB ((2 . 1) (3 . 2) (4 . 2)))
           (WUB ((1 . 1)) ((2 . 1) (3 . 1) (4 . 1)) ((2 . 2) (3 . 2) (4 . 3))))
          ((3 -1 GEQ) (WLB ((-2 . 1) (-3 . 2) (-4 . 2)))
           (WUB ((-1 . 1)) ((-2 . 1) (-3 . 1) (-4 . 1))
            ((-2 . 2) (-3 . 2) (-4 . 3))))
          ((3 NIL GEQ)
           (WLB ((1 . 1)) ((2 . 1) (3 . 1) (4 . 1)) ((2 . 2) (3 . 2) (4 . 3))
            ((-2 . 1) (-3 . 2) (-4 . 2)))
           (WUB ((2 . 1) (3 . 2) (4 . 2)) ((-1 . 1))
            ((-2 . 1) (-3 . 1) (-4 . 1)) ((-2 . 2) (-3 . 2) (-4 . 3))))
          ((3 1 GEQ)
           (WLB ((1 . 1)) ((2 . 1) (3 . 1) (4 . 1)) ((2 . 2) (3 . 2) (4 . 3)))
           (WUB ((2 . 1) (3 . 2) (4 . 2))))
          ((3 -1 GREATERP) (SLB ((-2 . 1) (-3 . 2) (-4 . 2)))
           (SUB ((-1 . 1)) ((-2 . 1) (-3 . 1) (-4 . 1))
            ((-2 . 2) (-3 . 2) (-4 . 3))))
          ((3 NIL GREATERP)
           (SLB ((1 . 1)) ((2 . 1) (3 . 1) (4 . 1)) ((2 . 2) (3 . 2) (4 . 3))
            ((-2 . 1) (-3 . 2) (-4 . 2)))
           (SUB ((2 . 1) (3 . 2) (4 . 2)) ((-1 . 1))
            ((-2 . 1) (-3 . 1) (-4 . 1)) ((-2 . 2) (-3 . 2) (-4 . 3))))
          ((3 1 GREATERP)
           (SLB ((1 . 1)) ((2 . 1) (3 . 1) (4 . 1)) ((2 . 2) (3 . 2) (4 . 3)))
           (SUB ((2 . 1) (3 . 2) (4 . 2)))))) 
(SETQ GUARD-FNALIST*
        '(((1 (-1)) . GUARD-1-1M) ((1 (1)) . GUARD-1-1)
          ((1 (-1 1)) . GUARD-1-2) ((2 (-2)) . GUARD-2-2M)
          ((2 (-1)) . GUARD-2-1M) ((2 (1)) . GUARD-2-1) ((2 (2)) . GUARD-2-2)
          ((2 (-2 -1)) . GUARD-2-3M) ((2 (1 2)) . GUARD-2-3)
          ((2 (-2 -1 1 2)) . GUARD-2-4) ((3 (-4)) . GUARD-3-4M)
          ((3 (-3)) . GUARD-3-3M) ((3 (-2)) . GUARD-3-2M)
          ((3 (-1)) . GUARD-3-1M) ((3 (1)) . GUARD-3-1) ((3 (2)) . GUARD-3-2)
          ((3 (3)) . GUARD-3-3) ((3 (4)) . GUARD-3-4)
          ((3 (-4 -3 -2)) . GUARD-3-5M) ((3 (2 3 4)) . GUARD-3-5))) 
(SETQ VSUB-FNALIST*
        '(((2 1 EQUAL ((1 . 1))) . VSUB-2-1-EQUAL-1)
          ((2 1 EQUAL ((1 . 2))) . VSUB-2-1-EQUAL-2)
          ((2 1 EQUAL ((2 . 1))) . VSUB-2-1-EQUAL-3)
          ((2 1 EQUAL ((1 . 1) (2 . 1) (-1 . 2) (-2 . 1))) . VSUB-2-1-EQUAL-4)
          ((2 1 EQUAL ((1 . 2) (2 . 1) (-1 . 1) (-2 . 1))) . VSUB-2-1-EQUAL-5)
          ((2 1 EQUAL ((1 . 1) (2 . 1))) . VSUB-2-1-EQUAL-1)
          ((2 1 EQUAL ((1 . 2) (2 . 1))) . VSUB-2-1-EQUAL-2)
          ((2 1 LESSP ((1 . 1))) . VSUB-2-1-LESSP-1)
          ((2 1 LESSP ((1 . 2))) . VSUB-2-1-LESSP-2)
          ((2 1 LESSP ((2 . 1))) . VSUB-2-1-LESSP-3)
          ((2 1 LESSP ((1 . 1) (2 . 1) (-1 . 2) (-2 . 1))) . VSUB-2-1-LESSP-4)
          ((2 1 LESSP ((1 . 2) (2 . 1) (-1 . 1) (-2 . 1))) . VSUB-2-1-LESSP-5)
          ((2 1 LESSP ((1 . 1) (2 . 1))) . VSUB-2-1-LESSP-1)
          ((2 1 LESSP ((1 . 2) (2 . 1))) . VSUB-2-1-LESSP-2)
          ((2 1 LEQ ((1 . 1))) . VSUB-2-1-LEQ-1)
          ((2 1 LEQ ((1 . 2))) . VSUB-2-1-LEQ-2)
          ((2 1 LEQ ((2 . 1))) . VSUB-2-1-LEQ-3)
          ((2 1 LEQ ((1 . 1) (2 . 1) (-1 . 2) (-2 . 1))) . VSUB-2-1-LEQ-4)
          ((2 1 LEQ ((1 . 2) (2 . 1) (-1 . 1) (-2 . 1))) . VSUB-2-1-LEQ-5)
          ((2 1 LEQ ((1 . 1) (2 . 1))) . VSUB-2-1-LEQ-1)
          ((2 1 LEQ ((1 . 2) (2 . 1))) . VSUB-2-1-LEQ-2)
          ((3 1 EQUAL ((1 . 1))) . VSUB-3-1-EQUAL-1)
          ((3 1 EQUAL ((2 . 1))) . VSUB-3-1-EQUAL-2)
          ((3 1 EQUAL ((2 . 2))) . VSUB-3-1-EQUAL-3)
          ((3 1 EQUAL ((3 . 1))) . VSUB-3-1-EQUAL-4)
          ((3 1 EQUAL ((3 . 2))) . VSUB-3-1-EQUAL-5)
          ((3 1 EQUAL ((4 . 1))) . VSUB-3-1-EQUAL-6)
          ((3 1 EQUAL ((4 . 2))) . VSUB-3-1-EQUAL-7)
          ((3 1 EQUAL ((4 . 3))) . VSUB-3-1-EQUAL-8)
          ((3 1 EQUAL ((2 . 1) (3 . 1) (4 . 1))) . VSUB-3-1-EQUAL-9)
          ((3 1 EQUAL ((2 . 1) (3 . 2) (4 . 2))) . VSUB-3-1-EQUAL-10)
          ((3 1 EQUAL ((2 . 2) (3 . 2) (4 . 3))) . VSUB-3-1-EQUAL-11)
          ((3 1 LESSP ((1 . 1))) . VSUB-3-1-LESSP-1)
          ((3 1 LESSP ((2 . 1))) . VSUB-3-1-LESSP-2)
          ((3 1 LESSP ((2 . 2))) . VSUB-3-1-LESSP-3)
          ((3 1 LESSP ((3 . 1))) . VSUB-3-1-LESSP-4)
          ((3 1 LESSP ((3 . 2))) . VSUB-3-1-LESSP-5)
          ((3 1 LESSP ((4 . 1))) . VSUB-3-1-LESSP-6)
          ((3 1 LESSP ((4 . 2))) . VSUB-3-1-LESSP-7)
          ((3 1 LESSP ((4 . 3))) . VSUB-3-1-LESSP-8)
          ((3 1 LESSP ((2 . 1) (3 . 1) (4 . 1))) . VSUB-3-1-LESSP-9)
          ((3 1 LESSP ((2 . 1) (3 . 2) (4 . 2))) . VSUB-3-1-LESSP-10)
          ((3 1 LESSP ((2 . 2) (3 . 2) (4 . 3))) . VSUB-3-1-LESSP-11)
          ((3 1 LEQ ((1 . 1))) . VSUB-3-1-LEQ-1)
          ((3 1 LEQ ((2 . 1))) . VSUB-3-1-LEQ-2)
          ((3 1 LEQ ((2 . 2))) . VSUB-3-1-LEQ-3)
          ((3 1 LEQ ((3 . 1))) . VSUB-3-1-LEQ-4)
          ((3 1 LEQ ((3 . 2))) . VSUB-3-1-LEQ-5)
          ((3 1 LEQ ((4 . 1))) . VSUB-3-1-LEQ-6)
          ((3 1 LEQ ((4 . 2))) . VSUB-3-1-LEQ-7)
          ((3 1 LEQ ((4 . 3))) . VSUB-3-1-LEQ-8)
          ((3 1 LEQ ((2 . 1) (3 . 1) (4 . 1))) . VSUB-3-1-LEQ-9)
          ((3 1 LEQ ((2 . 1) (3 . 2) (4 . 2))) . VSUB-3-1-LEQ-10)
          ((3 1 LEQ ((2 . 2) (3 . 2) (4 . 3))) . VSUB-3-1-LEQ-11)
          ((3 2 EQUAL ((1 . 1))) . VSUB-3-2-EQUAL-1)
          ((3 2 EQUAL ((2 . 1))) . VSUB-3-2-EQUAL-2)
          ((3 2 EQUAL ((2 . 2))) . VSUB-3-2-EQUAL-3)
          ((3 2 EQUAL ((3 . 1))) . VSUB-3-2-EQUAL-4)
          ((3 2 EQUAL ((3 . 2))) . VSUB-3-2-EQUAL-5)
          ((3 2 EQUAL ((4 . 1))) . VSUB-3-2-EQUAL-6)
          ((3 2 EQUAL ((4 . 2))) . VSUB-3-2-EQUAL-7)
          ((3 2 EQUAL ((4 . 3))) . VSUB-3-2-EQUAL-8)
          ((3 2 EQUAL ((2 . 1) (3 . 1) (4 . 1))) . VSUB-3-2-EQUAL-9)
          ((3 2 EQUAL ((2 . 1) (3 . 2) (4 . 2))) . VSUB-3-2-EQUAL-10)
          ((3 2 EQUAL ((2 . 2) (3 . 2) (4 . 3))) . VSUB-3-2-EQUAL-11)
          ((3 2 LESSP ((1 . 1))) . VSUB-3-2-LESSP-1)
          ((3 2 LESSP ((2 . 1))) . VSUB-3-2-LESSP-2)
          ((3 2 LESSP ((2 . 2))) . VSUB-3-2-LESSP-3)
          ((3 2 LESSP ((3 . 1))) . VSUB-3-2-LESSP-4)
          ((3 2 LESSP ((3 . 2))) . VSUB-3-2-LESSP-5)
          ((3 2 LESSP ((4 . 1))) . VSUB-3-2-LESSP-6)
          ((3 2 LESSP ((4 . 2))) . VSUB-3-2-LESSP-7)
          ((3 2 LESSP ((4 . 3))) . VSUB-3-2-LESSP-8)
          ((3 2 LESSP ((2 . 1) (3 . 1) (4 . 1))) . VSUB-3-2-LESSP-9)
          ((3 2 LESSP ((2 . 1) (3 . 2) (4 . 2))) . VSUB-3-2-LESSP-10)
          ((3 2 LESSP ((2 . 2) (3 . 2) (4 . 3))) . VSUB-3-2-LESSP-11)
          ((3 2 LEQ ((1 . 1))) . VSUB-3-2-LEQ-1)
          ((3 2 LEQ ((2 . 1))) . VSUB-3-2-LEQ-2)
          ((3 2 LEQ ((2 . 2))) . VSUB-3-2-LEQ-3)
          ((3 2 LEQ ((3 . 1))) . VSUB-3-2-LEQ-4)
          ((3 2 LEQ ((3 . 2))) . VSUB-3-2-LEQ-5)
          ((3 2 LEQ ((4 . 1))) . VSUB-3-2-LEQ-6)
          ((3 2 LEQ ((4 . 2))) . VSUB-3-2-LEQ-7)
          ((3 2 LEQ ((4 . 3))) . VSUB-3-2-LEQ-8)
          ((3 2 LEQ ((2 . 1) (3 . 1) (4 . 1))) . VSUB-3-2-LEQ-9)
          ((3 2 LEQ ((2 . 1) (3 . 2) (4 . 2))) . VSUB-3-2-LEQ-10)
          ((3 2 LEQ ((2 . 2) (3 . 2) (4 . 3))) . VSUB-3-2-LEQ-11))) 
(PUT 'GUARD-1-1M 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-1-1M 'DEFINED-ON-LINE '473) 
(PUT 'GUARD-1-1M 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-1-1M 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-1-1M (F) (GUARD-1-1 (NEGF F))) 
(PUT 'GUARD-1-1 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-1-1 'DEFINED-ON-LINE '476) 
(PUT 'GUARD-1-1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-1-1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-1-1 (F) (PROG (A) (SETQ A (CDAR F)) (RETURN (LIST 'GREATERP A NIL)))) 
(PUT 'GUARD-1-2 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-1-2 'DEFINED-ON-LINE '484) 
(PUT 'GUARD-1-2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-1-2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-1-2 (F) (PROG (A) (SETQ A (CDAR F)) (RETURN (LIST 'NEQ A NIL)))) 
(PUT 'GUARD-2-2M 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-2-2M 'DEFINED-ON-LINE '492) 
(PUT 'GUARD-2-2M 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-2-2M 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-2-2M (F) (GUARD-2-2 (NEGF F))) 
(PUT 'GUARD-2-1M 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-2-1M 'DEFINED-ON-LINE '495) 
(PUT 'GUARD-2-1M 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-2-1M 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-2-1M (F) (GUARD-2-1 (NEGF F))) 
(PUT 'GUARD-2-1 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-2-1 'DEFINED-ON-LINE '498) 
(PUT 'GUARD-2-1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-2-1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-2-1 (F)
    (PROG (A B C)
      (PROG (G569 G570)
        (SETQ G569 (COEFFS F))
        (SETQ G570 G569)
        (SETQ A (CAR G569))
        (SETQ G569 (CDR G569))
        (SETQ B (CAR G569))
        (SETQ G569 (CDR G569))
        (SETQ C (CAR G569))
        (SETQ G569 (CDR G569))
        (RETURN G570))
      (RETURN
       (CONS 'AND
             (LIST (OFSF_0MK2 'GREATERP A)
                   (OFSF_0MK2 'GREATERP
                              (ADDF (EXPTF B 2)
                                    (NEGF (MULTF 4 (MULTF A C)))))))))) 
(PUT 'GUARD-2-2 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-2-2 'DEFINED-ON-LINE '506) 
(PUT 'GUARD-2-2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-2-2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-2-2 (F)
    (PROG (A B C)
      (PROG (G571 G572)
        (SETQ G571 (COEFFS F))
        (SETQ G572 G571)
        (SETQ A (CAR G571))
        (SETQ G571 (CDR G571))
        (SETQ B (CAR G571))
        (SETQ G571 (CDR G571))
        (SETQ C (CAR G571))
        (SETQ G571 (CDR G571))
        (RETURN G572))
      (RETURN
       (CONS 'AND
             (LIST (OFSF_0MK2 'GREATERP A)
                   (OFSF_0MK2 'EQUAL
                              (ADDF (EXPTF B 2)
                                    (NEGF (MULTF 4 (MULTF A C)))))))))) 
(PUT 'GUARD-2-3M 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-2-3M 'DEFINED-ON-LINE '514) 
(PUT 'GUARD-2-3M 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-2-3M 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-2-3M (F) (GUARD-2-3 (NEGF F))) 
(PUT 'GUARD-2-3 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-2-3 'DEFINED-ON-LINE '517) 
(PUT 'GUARD-2-3 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-2-3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-2-3 (F)
    (PROG (A B C)
      (PROG (G573 G574)
        (SETQ G573 (COEFFS F))
        (SETQ G574 G573)
        (SETQ A (CAR G573))
        (SETQ G573 (CDR G573))
        (SETQ B (CAR G573))
        (SETQ G573 (CDR G573))
        (SETQ C (CAR G573))
        (SETQ G573 (CDR G573))
        (RETURN G574))
      (RETURN
       (CONS 'AND
             (LIST (OFSF_0MK2 'GREATERP A)
                   (OFSF_0MK2 'GEQ
                              (ADDF (EXPTF B 2)
                                    (NEGF (MULTF 4 (MULTF A C)))))))))) 
(PUT 'GUARD-2-4 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-2-4 'DEFINED-ON-LINE '525) 
(PUT 'GUARD-2-4 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-2-4 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-2-4 (F)
    (PROG (A B C)
      (PROG (G575 G576)
        (SETQ G575 (COEFFS F))
        (SETQ G576 G575)
        (SETQ A (CAR G575))
        (SETQ G575 (CDR G575))
        (SETQ B (CAR G575))
        (SETQ G575 (CDR G575))
        (SETQ C (CAR G575))
        (SETQ G575 (CDR G575))
        (RETURN G576))
      (RETURN
       (CONS 'AND
             (LIST (OFSF_0MK2 'NEQ A)
                   (OFSF_0MK2 'GEQ
                              (ADDF (EXPTF B 2)
                                    (NEGF (MULTF 4 (MULTF A C)))))))))) 
(PUT 'GUARD-3-4M 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-3-4M 'DEFINED-ON-LINE '533) 
(PUT 'GUARD-3-4M 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-3-4M 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-3-4M (F) (GUARD-3-4 (NEGF F))) 
(PUT 'GUARD-3-3M 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-3-3M 'DEFINED-ON-LINE '536) 
(PUT 'GUARD-3-3M 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-3-3M 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-3-3M (F) (GUARD-3-3 (NEGF F))) 
(PUT 'GUARD-3-2M 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-3-2M 'DEFINED-ON-LINE '539) 
(PUT 'GUARD-3-2M 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-3-2M 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-3-2M (F) (GUARD-3-2 (NEGF F))) 
(PUT 'GUARD-3-1M 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-3-1M 'DEFINED-ON-LINE '542) 
(PUT 'GUARD-3-1M 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-3-1M 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-3-1M (F) (GUARD-3-1 (NEGF F))) 
(PUT 'GUARD-3-1 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-3-1 'DEFINED-ON-LINE '545) 
(PUT 'GUARD-3-1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-3-1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-3-1 (F)
    (PROG (A B C D)
      (PROG (G577 G578)
        (SETQ G577 (COEFFS F))
        (SETQ G578 G577)
        (SETQ A (CAR G577))
        (SETQ G577 (CDR G577))
        (SETQ B (CAR G577))
        (SETQ G577 (CDR G577))
        (SETQ C (CAR G577))
        (SETQ G577 (CDR G577))
        (SETQ D (CAR G577))
        (SETQ G577 (CDR G577))
        (RETURN G578))
      (RETURN
       (CONS 'AND
             (LIST (OFSF_0MK2 'GREATERP A)
                   (RL_MKN 'OR
                           (LIST
                            (OFSF_0MK2 'EQUAL
                                       (ADDF (ADDF NIL (NEGF (EXPTF B 2)))
                                             (MULTF 3 (MULTF A C))))
                            (OFSF_0MK2 'GREATERP
                                       (ADDF
                                        (ADDF NIL
                                              (NEGF
                                               (MULTF (EXPTF B 2)
                                                      (EXPTF C 2))))
                                        (ADDF (MULTF 4 (MULTF (EXPTF C 3) A))
                                              (ADDF
                                               (MULTF 4 (MULTF (EXPTF B 3) D))
                                               (ADDF
                                                (MULTF 27
                                                       (MULTF (EXPTF D 2)
                                                              (EXPTF A 2)))
                                                (NEGF
                                                 (MULTF 18
                                                        (MULTF A
                                                               (MULTF B
                                                                      (MULTF C
                                                                             D))))))))))))))))) 
(PUT 'GUARD-3-2 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-3-2 'DEFINED-ON-LINE '554) 
(PUT 'GUARD-3-2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-3-2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-3-2 (F)
    (PROG (A B C D)
      (PROG (G579 G580)
        (SETQ G579 (COEFFS F))
        (SETQ G580 G579)
        (SETQ A (CAR G579))
        (SETQ G579 (CDR G579))
        (SETQ B (CAR G579))
        (SETQ G579 (CDR G579))
        (SETQ C (CAR G579))
        (SETQ G579 (CDR G579))
        (SETQ D (CAR G579))
        (SETQ G579 (CDR G579))
        (RETURN G580))
      (RETURN
       (CONS 'AND
             (LIST (OFSF_0MK2 'GREATERP A)
                   (OFSF_0MK2 'LESSP
                              (ADDF (ADDF NIL (NEGF (EXPTF B 2)))
                                    (MULTF 3 (MULTF A C))))
                   (OFSF_0MK2 'LESSP
                              (ADDF (MULTF 2 (EXPTF B 3))
                                    (ADDF (MULTF 27 (MULTF D (EXPTF A 2)))
                                          (NEGF
                                           (MULTF 9 (MULTF A (MULTF B C)))))))
                   (OFSF_0MK2 'EQUAL
                              (ADDF
                               (ADDF NIL
                                     (NEGF (MULTF (EXPTF B 2) (EXPTF C 2))))
                               (ADDF (MULTF 4 (MULTF (EXPTF C 3) A))
                                     (ADDF (MULTF 4 (MULTF (EXPTF B 3) D))
                                           (ADDF
                                            (MULTF 27
                                                   (MULTF (EXPTF D 2)
                                                          (EXPTF A 2)))
                                            (NEGF
                                             (MULTF 18
                                                    (MULTF A
                                                           (MULTF B
                                                                  (MULTF C
                                                                         D))))))))))))))) 
(PUT 'GUARD-3-3 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-3-3 'DEFINED-ON-LINE '563) 
(PUT 'GUARD-3-3 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-3-3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-3-3 (F)
    (PROG (A B C D)
      (PROG (G581 G582)
        (SETQ G581 (COEFFS F))
        (SETQ G582 G581)
        (SETQ A (CAR G581))
        (SETQ G581 (CDR G581))
        (SETQ B (CAR G581))
        (SETQ G581 (CDR G581))
        (SETQ C (CAR G581))
        (SETQ G581 (CDR G581))
        (SETQ D (CAR G581))
        (SETQ G581 (CDR G581))
        (RETURN G582))
      (RETURN
       (CONS 'AND
             (LIST (OFSF_0MK2 'GREATERP A)
                   (OFSF_0MK2 'LESSP
                              (ADDF (ADDF NIL (NEGF (EXPTF B 2)))
                                    (MULTF 3 (MULTF A C))))
                   (OFSF_0MK2 'GREATERP
                              (ADDF (MULTF 2 (EXPTF B 3))
                                    (ADDF (MULTF 27 (MULTF D (EXPTF A 2)))
                                          (NEGF
                                           (MULTF 9 (MULTF A (MULTF B C)))))))
                   (OFSF_0MK2 'EQUAL
                              (ADDF
                               (ADDF NIL
                                     (NEGF (MULTF (EXPTF B 2) (EXPTF C 2))))
                               (ADDF (MULTF 4 (MULTF (EXPTF C 3) A))
                                     (ADDF (MULTF 4 (MULTF (EXPTF B 3) D))
                                           (ADDF
                                            (MULTF 27
                                                   (MULTF (EXPTF D 2)
                                                          (EXPTF A 2)))
                                            (NEGF
                                             (MULTF 18
                                                    (MULTF A
                                                           (MULTF B
                                                                  (MULTF C
                                                                         D))))))))))))))) 
(PUT 'GUARD-3-4 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-3-4 'DEFINED-ON-LINE '572) 
(PUT 'GUARD-3-4 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-3-4 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-3-4 (F)
    (PROG (A B C D)
      (PROG (G583 G584)
        (SETQ G583 (COEFFS F))
        (SETQ G584 G583)
        (SETQ A (CAR G583))
        (SETQ G583 (CDR G583))
        (SETQ B (CAR G583))
        (SETQ G583 (CDR G583))
        (SETQ C (CAR G583))
        (SETQ G583 (CDR G583))
        (SETQ D (CAR G583))
        (SETQ G583 (CDR G583))
        (RETURN G584))
      (RETURN
       (CONS 'AND
             (LIST (OFSF_0MK2 'GREATERP A)
                   (OFSF_0MK2 'LESSP
                              (ADDF (ADDF NIL (NEGF (EXPTF B 2)))
                                    (MULTF 3 (MULTF A C))))
                   (OFSF_0MK2 'LESSP
                              (ADDF
                               (ADDF NIL
                                     (NEGF (MULTF (EXPTF B 2) (EXPTF C 2))))
                               (ADDF (MULTF 4 (MULTF (EXPTF C 3) A))
                                     (ADDF (MULTF 4 (MULTF (EXPTF B 3) D))
                                           (ADDF
                                            (MULTF 27
                                                   (MULTF (EXPTF D 2)
                                                          (EXPTF A 2)))
                                            (NEGF
                                             (MULTF 18
                                                    (MULTF A
                                                           (MULTF B
                                                                  (MULTF C
                                                                         D))))))))))))))) 
(PUT 'GUARD-3-5M 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-3-5M 'DEFINED-ON-LINE '581) 
(PUT 'GUARD-3-5M 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-3-5M 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-3-5M (F) (GUARD-3-5 (NEGF F))) 
(PUT 'GUARD-3-5 'NUMBER-OF-ARGS 1) 
(PUT 'GUARD-3-5 'DEFINED-ON-LINE '584) 
(PUT 'GUARD-3-5 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSLISTS.RED) 
(PUT 'GUARD-3-5 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GUARD-3-5 (F)
    (PROG (A B C D)
      (PROG (G585 G586)
        (SETQ G585 (COEFFS F))
        (SETQ G586 G585)
        (SETQ A (CAR G585))
        (SETQ G585 (CDR G585))
        (SETQ B (CAR G585))
        (SETQ G585 (CDR G585))
        (SETQ C (CAR G585))
        (SETQ G585 (CDR G585))
        (SETQ D (CAR G585))
        (SETQ G585 (CDR G585))
        (RETURN G586))
      (RETURN
       (CONS 'AND
             (LIST (OFSF_0MK2 'GREATERP A)
                   (OFSF_0MK2 'LESSP
                              (ADDF (ADDF NIL (NEGF (EXPTF B 2)))
                                    (MULTF 3 (MULTF A C))))
                   (OFSF_0MK2 'LEQ
                              (ADDF
                               (ADDF NIL
                                     (NEGF (MULTF (EXPTF B 2) (EXPTF C 2))))
                               (ADDF (MULTF 4 (MULTF (EXPTF C 3) A))
                                     (ADDF (MULTF 4 (MULTF (EXPTF B 3) D))
                                           (ADDF
                                            (MULTF 27
                                                   (MULTF (EXPTF D 2)
                                                          (EXPTF A 2)))
                                            (NEGF
                                             (MULTF 18
                                                    (MULTF A
                                                           (MULTF B
                                                                  (MULTF C
                                                                         D))))))))))))))) 
(PUT 'VSUB-2-1-EQUAL-1 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-EQUAL-1 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G587 G588)
        (SETQ G587 (COEFFS G))
        (SETQ G588 G587)
        (SETQ AA (CAR G587))
        (SETQ G587 (CDR G587))
        (SETQ BB (CAR G587))
        (SETQ G587 (CDR G587))
        (RETURN G588))
      (PROG (G589 G590)
        (SETQ G589 (COEFFS F))
        (SETQ G590 G589)
        (SETQ A (CAR G589))
        (SETQ G589 (CDR G589))
        (SETQ B (CAR G589))
        (SETQ G589 (CDR G589))
        (SETQ C (CAR G589))
        (SETQ G589 (CDR G589))
        (RETURN G590))
      (RETURN
       (CONS 'AND
             (LIST
              (OFSF_0MK2 'GEQ
                         (ADDF (MULTF 2 (MULTF A (MULTF AA BB)))
                               (NEGF (MULTF (EXPTF AA 2) B))))
              (OFSF_0MK2 'EQUAL
                         (ADDF (MULTF A (EXPTF BB 2))
                               (ADDF (MULTF (EXPTF AA 2) C)
                                     (NEGF (MULTF AA (MULTF B BB))))))))))) 
(PUT 'VSUB-2-1-EQUAL-2 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-EQUAL-2 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G591 G592)
        (SETQ G591 (COEFFS G))
        (SETQ G592 G591)
        (SETQ AA (CAR G591))
        (SETQ G591 (CDR G591))
        (SETQ BB (CAR G591))
        (SETQ G591 (CDR G591))
        (RETURN G592))
      (PROG (G593 G594)
        (SETQ G593 (COEFFS F))
        (SETQ G594 G593)
        (SETQ A (CAR G593))
        (SETQ G593 (CDR G593))
        (SETQ B (CAR G593))
        (SETQ G593 (CDR G593))
        (SETQ C (CAR G593))
        (SETQ G593 (CDR G593))
        (RETURN G594))
      (RETURN
       (CONS 'AND
             (LIST
              (OFSF_0MK2 'LEQ
                         (ADDF (MULTF 2 (MULTF A (MULTF AA BB)))
                               (NEGF (MULTF (EXPTF AA 2) B))))
              (OFSF_0MK2 'EQUAL
                         (ADDF (MULTF A (EXPTF BB 2))
                               (ADDF (MULTF (EXPTF AA 2) C)
                                     (NEGF (MULTF AA (MULTF B BB))))))))))) 
(PUT 'VSUB-2-1-EQUAL-3 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-EQUAL-3 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G595 G596)
        (SETQ G595 (COEFFS G))
        (SETQ G596 G595)
        (SETQ AA (CAR G595))
        (SETQ G595 (CDR G595))
        (SETQ BB (CAR G595))
        (SETQ G595 (CDR G595))
        (RETURN G596))
      (PROG (G597 G598)
        (SETQ G597 (COEFFS F))
        (SETQ G598 G597)
        (SETQ A (CAR G597))
        (SETQ G597 (CDR G597))
        (SETQ B (CAR G597))
        (SETQ G597 (CDR G597))
        (SETQ C (CAR G597))
        (SETQ G597 (CDR G597))
        (RETURN G598))
      (RETURN
       (LIST 'EQUAL (ADDF (MULTF 2 (MULTF A BB)) (NEGF (MULTF AA B))) NIL)))) 
(PUT 'VSUB-2-1-EQUAL-4 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-EQUAL-4 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G599 G600)
        (SETQ G599 (COEFFS G))
        (SETQ G600 G599)
        (SETQ AA (CAR G599))
        (SETQ G599 (CDR G599))
        (SETQ BB (CAR G599))
        (SETQ G599 (CDR G599))
        (RETURN G600))
      (PROG (G601 G602)
        (SETQ G601 (COEFFS F))
        (SETQ G602 G601)
        (SETQ A (CAR G601))
        (SETQ G601 (CDR G601))
        (SETQ B (CAR G601))
        (SETQ G601 (CDR G601))
        (SETQ C (CAR G601))
        (SETQ G601 (CDR G601))
        (RETURN G602))
      (RETURN
       (CONS 'AND
             (LIST
              (OFSF_0MK2 'GEQ
                         (ADDF (MULTF 2 (MULTF A (MULTF AA BB)))
                               (NEGF (MULTF (EXPTF AA 2) B))))
              (OFSF_0MK2 'EQUAL
                         (ADDF (MULTF A (EXPTF BB 2))
                               (ADDF (MULTF (EXPTF AA 2) C)
                                     (NEGF (MULTF AA (MULTF B BB))))))))))) 
(PUT 'VSUB-2-1-EQUAL-5 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-EQUAL-5 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G603 G604)
        (SETQ G603 (COEFFS G))
        (SETQ G604 G603)
        (SETQ AA (CAR G603))
        (SETQ G603 (CDR G603))
        (SETQ BB (CAR G603))
        (SETQ G603 (CDR G603))
        (RETURN G604))
      (PROG (G605 G606)
        (SETQ G605 (COEFFS F))
        (SETQ G606 G605)
        (SETQ A (CAR G605))
        (SETQ G605 (CDR G605))
        (SETQ B (CAR G605))
        (SETQ G605 (CDR G605))
        (SETQ C (CAR G605))
        (SETQ G605 (CDR G605))
        (RETURN G606))
      (RETURN
       (CONS 'AND
             (LIST
              (OFSF_0MK2 'LEQ
                         (ADDF (MULTF 2 (MULTF A (MULTF AA BB)))
                               (NEGF (MULTF (EXPTF AA 2) B))))
              (OFSF_0MK2 'EQUAL
                         (ADDF (MULTF A (EXPTF BB 2))
                               (ADDF (MULTF (EXPTF AA 2) C)
                                     (NEGF (MULTF AA (MULTF B BB))))))))))) 
(PUT 'VSUB-2-1-LESSP-1 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-LESSP-1 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G607 G608)
        (SETQ G607 (COEFFS G))
        (SETQ G608 G607)
        (SETQ AA (CAR G607))
        (SETQ G607 (CDR G607))
        (SETQ BB (CAR G607))
        (SETQ G607 (CDR G607))
        (RETURN G608))
      (PROG (G609 G610)
        (SETQ G609 (COEFFS F))
        (SETQ G610 G609)
        (SETQ A (CAR G609))
        (SETQ G609 (CDR G609))
        (SETQ B (CAR G609))
        (SETQ G609 (CDR G609))
        (SETQ C (CAR G609))
        (SETQ G609 (CDR G609))
        (RETURN G610))
      (RETURN
       (CONS 'OR
             (LIST
              (RL_MKN 'AND
                      (LIST
                       (OFSF_0MK2 'LESSP
                                  (ADDF (MULTF 2 (MULTF A BB))
                                        (NEGF (MULTF AA B))))
                       (OFSF_0MK2 'GREATERP
                                  (ADDF (MULTF A (EXPTF BB 2))
                                        (ADDF (MULTF (EXPTF AA 2) C)
                                              (NEGF
                                               (MULTF AA (MULTF B BB))))))))
              (RL_MKN 'AND
                      (LIST (OFSF_0MK2 'GEQ AA)
                            (RL_MKN 'OR
                                    (LIST
                                     (OFSF_0MK2 'LESSP
                                                (ADDF (MULTF 2 (MULTF A BB))
                                                      (NEGF (MULTF AA B))))
                                     (OFSF_0MK2 'LESSP
                                                (ADDF (MULTF A (EXPTF BB 2))
                                                      (ADDF
                                                       (MULTF (EXPTF AA 2) C)
                                                       (NEGF
                                                        (MULTF AA
                                                               (MULTF B
                                                                      BB))))))))))))))) 
(PUT 'VSUB-2-1-LESSP-2 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-LESSP-2 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G611 G612)
        (SETQ G611 (COEFFS G))
        (SETQ G612 G611)
        (SETQ AA (CAR G611))
        (SETQ G611 (CDR G611))
        (SETQ BB (CAR G611))
        (SETQ G611 (CDR G611))
        (RETURN G612))
      (PROG (G613 G614)
        (SETQ G613 (COEFFS F))
        (SETQ G614 G613)
        (SETQ A (CAR G613))
        (SETQ G613 (CDR G613))
        (SETQ B (CAR G613))
        (SETQ G613 (CDR G613))
        (SETQ C (CAR G613))
        (SETQ G613 (CDR G613))
        (RETURN G614))
      (RETURN
       (CONS 'OR
             (LIST
              (RL_MKN 'AND
                      (LIST
                       (OFSF_0MK2 'LESSP
                                  (ADDF (MULTF 2 (MULTF A BB))
                                        (NEGF (MULTF AA B))))
                       (OFSF_0MK2 'GREATERP
                                  (ADDF (MULTF A (EXPTF BB 2))
                                        (ADDF (MULTF (EXPTF AA 2) C)
                                              (NEGF
                                               (MULTF AA (MULTF B BB))))))))
              (RL_MKN 'AND
                      (LIST (OFSF_0MK2 'LEQ AA)
                            (RL_MKN 'OR
                                    (LIST
                                     (OFSF_0MK2 'LESSP
                                                (ADDF (MULTF 2 (MULTF A BB))
                                                      (NEGF (MULTF AA B))))
                                     (OFSF_0MK2 'LESSP
                                                (ADDF (MULTF A (EXPTF BB 2))
                                                      (ADDF
                                                       (MULTF (EXPTF AA 2) C)
                                                       (NEGF
                                                        (MULTF AA
                                                               (MULTF B
                                                                      BB))))))))))))))) 
(PUT 'VSUB-2-1-LESSP-3 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-LESSP-3 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G615 G616)
        (SETQ G615 (COEFFS G))
        (SETQ G616 G615)
        (SETQ AA (CAR G615))
        (SETQ G615 (CDR G615))
        (SETQ BB (CAR G615))
        (SETQ G615 (CDR G615))
        (RETURN G616))
      (PROG (G617 G618)
        (SETQ G617 (COEFFS F))
        (SETQ G618 G617)
        (SETQ A (CAR G617))
        (SETQ G617 (CDR G617))
        (SETQ B (CAR G617))
        (SETQ G617 (CDR G617))
        (SETQ C (CAR G617))
        (SETQ G617 (CDR G617))
        (RETURN G618))
      (RETURN
       (LIST 'LESSP (ADDF (MULTF 2 (MULTF A BB)) (NEGF (MULTF AA B))) NIL)))) 
(PUT 'VSUB-2-1-LESSP-4 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-LESSP-4 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G619 G620)
        (SETQ G619 (COEFFS G))
        (SETQ G620 G619)
        (SETQ AA (CAR G619))
        (SETQ G619 (CDR G619))
        (SETQ BB (CAR G619))
        (SETQ G619 (CDR G619))
        (RETURN G620))
      (PROG (G621 G622)
        (SETQ G621 (COEFFS F))
        (SETQ G622 G621)
        (SETQ A (CAR G621))
        (SETQ G621 (CDR G621))
        (SETQ B (CAR G621))
        (SETQ G621 (CDR G621))
        (SETQ C (CAR G621))
        (SETQ G621 (CDR G621))
        (RETURN G622))
      (RETURN
       (CONS 'OR
             (LIST
              (RL_MKN 'AND
                      (LIST
                       (OFSF_0MK2 'LESSP
                                  (ADDF (MULTF 2 (MULTF (EXPTF A 2) BB))
                                        (NEGF (MULTF A (MULTF AA B)))))
                       (OFSF_0MK2 'GREATERP
                                  (ADDF (MULTF (EXPTF A 2) (EXPTF BB 2))
                                        (ADDF (MULTF A (MULTF (EXPTF AA 2) C))
                                              (NEGF
                                               (MULTF A
                                                      (MULTF AA
                                                             (MULTF B
                                                                    BB)))))))))
              (RL_MKN 'AND
                      (LIST (OFSF_0MK2 'GEQ (MULTF A AA))
                            (RL_MKN 'OR
                                    (LIST
                                     (OFSF_0MK2 'LESSP
                                                (ADDF
                                                 (MULTF 2
                                                        (MULTF (EXPTF A 2) BB))
                                                 (NEGF
                                                  (MULTF A (MULTF AA B)))))
                                     (OFSF_0MK2 'LESSP
                                                (ADDF
                                                 (MULTF (EXPTF A 2)
                                                        (EXPTF BB 2))
                                                 (ADDF
                                                  (MULTF A
                                                         (MULTF (EXPTF AA 2)
                                                                C))
                                                  (NEGF
                                                   (MULTF A
                                                          (MULTF AA
                                                                 (MULTF B
                                                                        BB)))))))))))))))) 
(PUT 'VSUB-2-1-LESSP-5 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-LESSP-5 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G623 G624)
        (SETQ G623 (COEFFS G))
        (SETQ G624 G623)
        (SETQ AA (CAR G623))
        (SETQ G623 (CDR G623))
        (SETQ BB (CAR G623))
        (SETQ G623 (CDR G623))
        (RETURN G624))
      (PROG (G625 G626)
        (SETQ G625 (COEFFS F))
        (SETQ G626 G625)
        (SETQ A (CAR G625))
        (SETQ G625 (CDR G625))
        (SETQ B (CAR G625))
        (SETQ G625 (CDR G625))
        (SETQ C (CAR G625))
        (SETQ G625 (CDR G625))
        (RETURN G626))
      (RETURN
       (CONS 'OR
             (LIST
              (RL_MKN 'AND
                      (LIST
                       (OFSF_0MK2 'LESSP
                                  (ADDF (MULTF 2 (MULTF (EXPTF A 2) BB))
                                        (NEGF (MULTF A (MULTF AA B)))))
                       (OFSF_0MK2 'GREATERP
                                  (ADDF (MULTF (EXPTF A 2) (EXPTF BB 2))
                                        (ADDF (MULTF A (MULTF (EXPTF AA 2) C))
                                              (NEGF
                                               (MULTF A
                                                      (MULTF AA
                                                             (MULTF B
                                                                    BB)))))))))
              (RL_MKN 'AND
                      (LIST (OFSF_0MK2 'LEQ (MULTF A AA))
                            (RL_MKN 'OR
                                    (LIST
                                     (OFSF_0MK2 'LESSP
                                                (ADDF
                                                 (MULTF 2
                                                        (MULTF (EXPTF A 2) BB))
                                                 (NEGF
                                                  (MULTF A (MULTF AA B)))))
                                     (OFSF_0MK2 'LESSP
                                                (ADDF
                                                 (MULTF (EXPTF A 2)
                                                        (EXPTF BB 2))
                                                 (ADDF
                                                  (MULTF A
                                                         (MULTF (EXPTF AA 2)
                                                                C))
                                                  (NEGF
                                                   (MULTF A
                                                          (MULTF AA
                                                                 (MULTF B
                                                                        BB)))))))))))))))) 
(PUT 'VSUB-2-1-LEQ-1 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-LEQ-1 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G627 G628)
        (SETQ G627 (COEFFS G))
        (SETQ G628 G627)
        (SETQ AA (CAR G627))
        (SETQ G627 (CDR G627))
        (SETQ BB (CAR G627))
        (SETQ G627 (CDR G627))
        (RETURN G628))
      (PROG (G629 G630)
        (SETQ G629 (COEFFS F))
        (SETQ G630 G629)
        (SETQ A (CAR G629))
        (SETQ G629 (CDR G629))
        (SETQ B (CAR G629))
        (SETQ G629 (CDR G629))
        (SETQ C (CAR G629))
        (SETQ G629 (CDR G629))
        (RETURN G630))
      (RETURN
       (CONS 'OR
             (LIST
              (RL_MKN 'AND
                      (LIST
                       (OFSF_0MK2 'LEQ
                                  (ADDF (MULTF 2 (MULTF A BB))
                                        (NEGF (MULTF AA B))))
                       (OFSF_0MK2 'GEQ
                                  (ADDF (MULTF A (EXPTF BB 2))
                                        (ADDF (MULTF (EXPTF AA 2) C)
                                              (NEGF
                                               (MULTF AA (MULTF B BB))))))))
              (RL_MKN 'AND
                      (LIST (OFSF_0MK2 'GEQ AA)
                            (OFSF_0MK2 'LEQ
                                       (ADDF (MULTF A (EXPTF BB 2))
                                             (ADDF (MULTF (EXPTF AA 2) C)
                                                   (NEGF
                                                    (MULTF AA
                                                           (MULTF B
                                                                  BB))))))))))))) 
(PUT 'VSUB-2-1-LEQ-2 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-LEQ-2 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G631 G632)
        (SETQ G631 (COEFFS G))
        (SETQ G632 G631)
        (SETQ AA (CAR G631))
        (SETQ G631 (CDR G631))
        (SETQ BB (CAR G631))
        (SETQ G631 (CDR G631))
        (RETURN G632))
      (PROG (G633 G634)
        (SETQ G633 (COEFFS F))
        (SETQ G634 G633)
        (SETQ A (CAR G633))
        (SETQ G633 (CDR G633))
        (SETQ B (CAR G633))
        (SETQ G633 (CDR G633))
        (SETQ C (CAR G633))
        (SETQ G633 (CDR G633))
        (RETURN G634))
      (RETURN
       (CONS 'OR
             (LIST
              (RL_MKN 'AND
                      (LIST
                       (OFSF_0MK2 'LEQ
                                  (ADDF (MULTF 2 (MULTF A BB))
                                        (NEGF (MULTF AA B))))
                       (OFSF_0MK2 'GEQ
                                  (ADDF (MULTF A (EXPTF BB 2))
                                        (ADDF (MULTF (EXPTF AA 2) C)
                                              (NEGF
                                               (MULTF AA (MULTF B BB))))))))
              (RL_MKN 'AND
                      (LIST (OFSF_0MK2 'LEQ AA)
                            (OFSF_0MK2 'LEQ
                                       (ADDF (MULTF A (EXPTF BB 2))
                                             (ADDF (MULTF (EXPTF AA 2) C)
                                                   (NEGF
                                                    (MULTF AA
                                                           (MULTF B
                                                                  BB))))))))))))) 
(PUT 'VSUB-2-1-LEQ-3 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-LEQ-3 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G635 G636)
        (SETQ G635 (COEFFS G))
        (SETQ G636 G635)
        (SETQ AA (CAR G635))
        (SETQ G635 (CDR G635))
        (SETQ BB (CAR G635))
        (SETQ G635 (CDR G635))
        (RETURN G636))
      (PROG (G637 G638)
        (SETQ G637 (COEFFS F))
        (SETQ G638 G637)
        (SETQ A (CAR G637))
        (SETQ G637 (CDR G637))
        (SETQ B (CAR G637))
        (SETQ G637 (CDR G637))
        (SETQ C (CAR G637))
        (SETQ G637 (CDR G637))
        (RETURN G638))
      (RETURN
       (LIST 'LEQ (ADDF (MULTF 2 (MULTF A BB)) (NEGF (MULTF AA B))) NIL)))) 
(PUT 'VSUB-2-1-LEQ-4 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-LEQ-4 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G639 G640)
        (SETQ G639 (COEFFS G))
        (SETQ G640 G639)
        (SETQ AA (CAR G639))
        (SETQ G639 (CDR G639))
        (SETQ BB (CAR G639))
        (SETQ G639 (CDR G639))
        (RETURN G640))
      (PROG (G641 G642)
        (SETQ G641 (COEFFS F))
        (SETQ G642 G641)
        (SETQ A (CAR G641))
        (SETQ G641 (CDR G641))
        (SETQ B (CAR G641))
        (SETQ G641 (CDR G641))
        (SETQ C (CAR G641))
        (SETQ G641 (CDR G641))
        (RETURN G642))
      (RETURN
       (CONS 'OR
             (LIST
              (RL_MKN 'AND
                      (LIST
                       (OFSF_0MK2 'LEQ
                                  (ADDF (MULTF 2 (MULTF (EXPTF A 2) BB))
                                        (NEGF (MULTF A (MULTF AA B)))))
                       (OFSF_0MK2 'GEQ
                                  (ADDF (MULTF (EXPTF A 2) (EXPTF BB 2))
                                        (ADDF (MULTF A (MULTF (EXPTF AA 2) C))
                                              (NEGF
                                               (MULTF A
                                                      (MULTF AA
                                                             (MULTF B
                                                                    BB)))))))))
              (RL_MKN 'AND
                      (LIST (OFSF_0MK2 'GEQ (MULTF A AA))
                            (OFSF_0MK2 'LEQ
                                       (ADDF (MULTF (EXPTF A 2) (EXPTF BB 2))
                                             (ADDF
                                              (MULTF A (MULTF (EXPTF AA 2) C))
                                              (NEGF
                                               (MULTF A
                                                      (MULTF AA
                                                             (MULTF B
                                                                    BB)))))))))))))) 
(PUT 'VSUB-2-1-LEQ-5 'NUMBER-OF-ARGS 3) 
(DE VSUB-2-1-LEQ-5 (AT PR THEO)
    (PROG (G F AA BB A B C)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (PROG (G643 G644)
        (SETQ G643 (COEFFS G))
        (SETQ G644 G643)
        (SETQ AA (CAR G643))
        (SETQ G643 (CDR G643))
        (SETQ BB (CAR G643))
        (SETQ G643 (CDR G643))
        (RETURN G644))
      (PROG (G645 G646)
        (SETQ G645 (COEFFS F))
        (SETQ G646 G645)
        (SETQ A (CAR G645))
        (SETQ G645 (CDR G645))
        (SETQ B (CAR G645))
        (SETQ G645 (CDR G645))
        (SETQ C (CAR G645))
        (SETQ G645 (CDR G645))
        (RETURN G646))
      (RETURN
       (CONS 'OR
             (LIST
              (RL_MKN 'AND
                      (LIST
                       (OFSF_0MK2 'LEQ
                                  (ADDF (MULTF 2 (MULTF (EXPTF A 2) BB))
                                        (NEGF (MULTF A (MULTF AA B)))))
                       (OFSF_0MK2 'GEQ
                                  (ADDF (MULTF (EXPTF A 2) (EXPTF BB 2))
                                        (ADDF (MULTF A (MULTF (EXPTF AA 2) C))
                                              (NEGF
                                               (MULTF A
                                                      (MULTF AA
                                                             (MULTF B
                                                                    BB)))))))))
              (RL_MKN 'AND
                      (LIST (OFSF_0MK2 'LEQ (MULTF A AA))
                            (OFSF_0MK2 'LEQ
                                       (ADDF (MULTF (EXPTF A 2) (EXPTF BB 2))
                                             (ADDF
                                              (MULTF A (MULTF (EXPTF AA 2) C))
                                              (NEGF
                                               (MULTF A
                                                      (MULTF AA
                                                             (MULTF B
                                                                    BB)))))))))))))) 
(PUT 'VSUB-3-1-EQUAL-1 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-1 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-EQUAL-1P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-1-EQUAL-1P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-EQUAL-1P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-1P (AT PR THEO)
    (PROG (G F X BETA)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA THEO)))) 
(PUT 'VSUB-3-1-EQUAL-2 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-2 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-EQUAL-2P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-1-EQUAL-2P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-EQUAL-2P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-2P (AT PR THEO)
    (PROG (G F X DF ALPHA1)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL G NIL) ALPHA1 THEO)))) 
(PUT 'VSUB-3-1-EQUAL-3 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-3 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-EQUAL-3P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-1-EQUAL-3P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-EQUAL-3P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-3P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA2 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (RETURN (CONS 'AND (LIST W1 W2))))) 
(PUT 'VSUB-3-1-EQUAL-4 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-4 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-EQUAL-4P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-1-EQUAL-4P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-EQUAL-4P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-4P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ G NIL) ALPHA1 THEO))
      (RETURN (CONS 'AND (LIST W1 W2))))) 
(PUT 'VSUB-3-1-EQUAL-5 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-5 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-EQUAL-5P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-1-EQUAL-5P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-EQUAL-5P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-5P (AT PR THEO)
    (PROG (G F X DF ALPHA2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL G NIL) ALPHA2 THEO)))) 
(PUT 'VSUB-3-1-EQUAL-6 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-6 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-EQUAL-6P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-1-EQUAL-6P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-EQUAL-6P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-6P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ G NIL) ALPHA1 THEO))
      (RETURN (CONS 'AND (LIST W1 W2))))) 
(PUT 'VSUB-3-1-EQUAL-7 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-7 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-EQUAL-7P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-1-EQUAL-7P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-EQUAL-7P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-7P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 ALPHA2 W1 W2 W3)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ G NIL) ALPHA2 THEO))
      (RETURN (CONS 'AND (LIST W1 W2 W3))))) 
(PUT 'VSUB-3-1-EQUAL-8 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-8 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-EQUAL-8P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-1-EQUAL-8P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-EQUAL-8P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-8P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 ALPHA2 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (RETURN (CONS 'AND (LIST W1 W2))))) 
(PUT 'VSUB-3-1-EQUAL-9 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-9 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-EQUAL-9P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-1-EQUAL-9P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-EQUAL-9P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-9P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ G NIL) ALPHA1 THEO))
      (RETURN (CONS 'AND (LIST W1 W2))))) 
(PUT 'VSUB-3-1-EQUAL-10 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-10 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-EQUAL-10P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-1-EQUAL-10P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-EQUAL-10P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-10P (AT PR THEO)
    (PROG (G F X DF BETA W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DF NIL) BETA THEO))
      (RETURN (CONS 'AND (LIST W1 W2))))) 
(PUT 'VSUB-3-1-EQUAL-11 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-11 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-EQUAL-11P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-1-EQUAL-11P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-EQUAL-11P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-EQUAL-11P (AT PR THEO)
    (PROG (G F X DF DDF BETA W1 W2 W3)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DDF NIL) BETA THEO))
      (RETURN (CONS 'AND (LIST W1 W2 W3))))) 
(PUT 'VSUB-3-1-LESSP-1 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-1 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-LESSP-1P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LEQ-1P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LESSP-1P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-1P (AT PR THEO)
    (PROG (G F X BETA)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA THEO)))) 
(PUT 'VSUB-3-1-LESSP-2 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-2 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-LESSP-2P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LEQ-2P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LESSP-2P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-2P (AT PR THEO)
    (PROG (G F X DF ALPHA1)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP G NIL) ALPHA1 THEO)))) 
(PUT 'VSUB-3-1-LESSP-3 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-3 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-LESSP-3P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LEQ-3P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LESSP-3P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-3P (AT PR THEO)
    (PROG (G F X BETA)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA THEO)))) 
(PUT 'VSUB-3-1-LESSP-4 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-4 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-LESSP-4P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LEQ-4P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LESSP-4P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-4P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA2 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL G NIL) ALPHA2 THEO))
      (RETURN (CONS 'OR (LIST W1 W2))))) 
(PUT 'VSUB-3-1-LESSP-5 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-5 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-LESSP-5P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LEQ-5P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LESSP-5P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-5P (AT PR THEO)
    (PROG (G F X DF ALPHA2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP G NIL) ALPHA2 THEO)))) 
(PUT 'VSUB-3-1-LESSP-6 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-6 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-LESSP-6P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LEQ-6P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LESSP-6P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-6P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (RETURN (CONS 'OR (LIST W1 W2))))) 
(PUT 'VSUB-3-1-LESSP-7 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-7 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-LESSP-7P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LEQ-7P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LESSP-7P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-7P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 ALPHA2 W1 W2 W3)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP F NIL) BETA THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (RETURN (CONS 'OR (LIST W1 (RL_MKN 'AND (LIST W2 W3))))))) 
(PUT 'VSUB-3-1-LESSP-8 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-8 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-LESSP-8P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LEQ-8P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LESSP-8P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-8P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA2 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (RETURN (CONS 'AND (LIST W1 W2))))) 
(PUT 'VSUB-3-1-LESSP-9 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-9 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-LESSP-9P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LEQ-9P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LESSP-9P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-9P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP G NIL) ALPHA1 THEO))
      (RETURN (CONS 'OR (LIST W1 W2))))) 
(PUT 'VSUB-3-1-LESSP-10 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-10 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-LESSP-10P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LEQ-10P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LESSP-10P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-10P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 ALPHA2 W1 W2 W3)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP G NIL) ALPHA2 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP F NIL) BETA THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP G NIL) ALPHA1 THEO))
      (RETURN (CONS 'OR (LIST W1 (RL_MKN 'AND (LIST W2 W3))))))) 
(PUT 'VSUB-3-1-LESSP-11 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-11 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-1-LESSP-11P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LEQ-11P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LESSP-11P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LESSP-11P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA2 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP G NIL) ALPHA2 THEO))
      (RETURN (CONS 'AND (LIST W1 W2))))) 
(PUT 'VSUB-3-1-LEQ-1 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-1 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-1-LEQ-1P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LESSP-1P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LEQ-1P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-1P (AT PR THEO)
    (PROG (G F X BETA)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA THEO)))) 
(PUT 'VSUB-3-1-LEQ-2 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-2 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-1-LEQ-2P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LESSP-2P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LEQ-2P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-2P (AT PR THEO)
    (PROG (G F X DF ALPHA1)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO)))) 
(PUT 'VSUB-3-1-LEQ-3 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-3 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-1-LEQ-3P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LESSP-3P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LEQ-3P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-3P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA2 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (RETURN (CONS 'AND (LIST W1 W2))))) 
(PUT 'VSUB-3-1-LEQ-4 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-4 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-1-LEQ-4P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LESSP-4P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LEQ-4P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-4P (AT PR THEO)
    (PROG (G F X BETA)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA THEO)))) 
(PUT 'VSUB-3-1-LEQ-5 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-5 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-1-LEQ-5P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LESSP-5P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LEQ-5P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-5P (AT PR THEO)
    (PROG (G F X DF ALPHA2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO)))) 
(PUT 'VSUB-3-1-LEQ-6 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-6 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-1-LEQ-6P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LESSP-6P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LEQ-6P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-6P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (RETURN (CONS 'OR (LIST W1 W2))))) 
(PUT 'VSUB-3-1-LEQ-7 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-7 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-1-LEQ-7P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LESSP-7P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LEQ-7P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-7P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 ALPHA2 W1 W2 W3)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ F NIL) BETA THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (RETURN (CONS 'OR (LIST W1 (RL_MKN 'AND (LIST W2 W3))))))) 
(PUT 'VSUB-3-1-LEQ-8 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-8 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-1-LEQ-8P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LESSP-8P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LEQ-8P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-8P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA2 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (RETURN (CONS 'AND (LIST W1 W2))))) 
(PUT 'VSUB-3-1-LEQ-9 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-9 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-1-LEQ-9P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LESSP-9P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LEQ-9P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-9P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (RETURN (CONS 'OR (LIST W1 W2))))) 
(PUT 'VSUB-3-1-LEQ-10 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-10 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-1-LEQ-10P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LESSP-10P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LEQ-10P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-10P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA1 ALPHA2 W1 W2 W3)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ F NIL) BETA THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (RETURN (CONS 'OR (LIST W1 (RL_MKN 'AND (LIST W2 W3))))))) 
(PUT 'VSUB-3-1-LEQ-11 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-11 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-1-LEQ-11P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-1-LESSP-11P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-1-LEQ-11P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-1-LEQ-11P (AT PR THEO)
    (PROG (G F X DF BETA ALPHA2 W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (RETURN (CONS 'AND (LIST W1 W2))))) 
(PUT 'VSUB-3-2-EQUAL-1 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-1 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-EQUAL-1P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-2-EQUAL-1P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-EQUAL-1P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-1P (AT PR THEO)
    (PROG (G F X BETA1 BETA2 GUARD W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA2 THEO))
      (RETURN (CONS 'AND (LIST GUARD (RL_MKN 'OR (LIST W1 W2))))))) 
(PUT 'VSUB-3-2-EQUAL-2 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-2 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-EQUAL-2P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-2-EQUAL-2P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-EQUAL-2P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-2P (AT PR THEO)
    (PROG (G F X DF ALPHA1)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL G NIL) ALPHA1 THEO)))) 
(PUT 'VSUB-3-2-EQUAL-3 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-3 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-EQUAL-3P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-2-EQUAL-3P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-EQUAL-3P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-3P (AT PR THEO)
    (PROG (G F X DDF BETA1 BETA2 GUARD W1 W2 W3 W4)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DDF (DIFF (DIFF F X) X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DDF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA2 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DDF NIL) BETA2 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD
                   (RL_MKN 'OR
                           (LIST (RL_MKN 'AND (LIST W1 W2))
                                 (RL_MKN 'AND (LIST W3 W4))))))))) 
(PUT 'VSUB-3-2-EQUAL-4 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-4 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-EQUAL-4P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-2-EQUAL-4P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-EQUAL-4P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-4P (AT PR THEO)
    (PROG (G F X DDF BETA1 BETA2 GUARD W1 W2 W3 W4)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DDF (DIFF (DIFF F X) X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DDF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA2 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DDF NIL) BETA2 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD
                   (RL_MKN 'OR
                           (LIST (RL_MKN 'AND (LIST W1 W2))
                                 (RL_MKN 'AND (LIST W3 W4))))))))) 
(PUT 'VSUB-3-2-EQUAL-5 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-5 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-EQUAL-5P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-2-EQUAL-5P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-EQUAL-5P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-5P (AT PR THEO)
    (PROG (G F X DF ALPHA2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL G NIL) ALPHA2 THEO)))) 
(PUT 'VSUB-3-2-EQUAL-6 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-6 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-EQUAL-6P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-2-EQUAL-6P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-EQUAL-6P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-6P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 GUARD W1 W2 W3 W4 W5 W6)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DDF NIL) BETA1 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA2 THEO))
      (SETQ W6 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DDF NIL) BETA2 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD
                   (RL_MKN 'OR
                           (LIST (RL_MKN 'AND (LIST W1 W2 W3))
                                 (RL_MKN 'AND (LIST W4 W5 W6))))))))) 
(PUT 'VSUB-3-2-EQUAL-7 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-7 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-EQUAL-7P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-2-EQUAL-7P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-EQUAL-7P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-7P (AT PR THEO)
    (PROG (G F X DF BETA1 BETA2 GUARD W1 W2 W3 W4)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA2 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DF NIL) BETA2 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD
                   (RL_MKN 'OR
                           (LIST (RL_MKN 'AND (LIST W1 W2))
                                 (RL_MKN 'AND (LIST W3 W4))))))))) 
(PUT 'VSUB-3-2-EQUAL-8 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-8 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-EQUAL-8P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-2-EQUAL-8P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-EQUAL-8P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-8P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 GUARD W1 W2 W3 W4 W5 W6)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DDF NIL) BETA1 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA2 THEO))
      (SETQ W6 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DDF NIL) BETA2 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD
                   (RL_MKN 'OR
                           (LIST (RL_MKN 'AND (LIST W1 W2 W3))
                                 (RL_MKN 'AND (LIST W4 W5 W6))))))))) 
(PUT 'VSUB-3-2-EQUAL-9 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-9 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-EQUAL-9P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-2-EQUAL-9P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-EQUAL-9P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-9P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 GUARD W1 W2 W3 W4 W5 W6)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DDF NIL) BETA1 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA2 THEO))
      (SETQ W6 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DDF NIL) BETA2 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD
                   (RL_MKN 'OR
                           (LIST (RL_MKN 'AND (LIST W1 W2 W3))
                                 (RL_MKN 'AND (LIST W4 W5 W6))))))))) 
(PUT 'VSUB-3-2-EQUAL-10 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-10 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-EQUAL-10P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-2-EQUAL-10P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-EQUAL-10P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-10P (AT PR THEO)
    (PROG (G F X DF BETA1 BETA2 GUARD W1 W2 W3 W4)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA2 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DF NIL) BETA2 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD
                   (RL_MKN 'OR
                           (LIST (RL_MKN 'AND (LIST W1 W2))
                                 (RL_MKN 'AND (LIST W3 W4))))))))) 
(PUT 'VSUB-3-2-EQUAL-11 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-11 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-EQUAL-11P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (VSUB-3-2-EQUAL-11P (LIST 'EQUAL (NEGF G) NIL) PR
               (CONS (LIST 'LESSP LCG NIL) THEO)))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'EQUAL (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-EQUAL-11P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-EQUAL-11P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 GUARD W1 W2 W3 W4 W5 W6)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DDF NIL) BETA1 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL F NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA2 THEO))
      (SETQ W6 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DDF NIL) BETA2 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD
                   (RL_MKN 'OR
                           (LIST (RL_MKN 'AND (LIST W1 W2 W3))
                                 (RL_MKN 'AND (LIST W4 W5 W6))))))))) 
(PUT 'VSUB-3-2-LESSP-1 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-1 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-LESSP-1P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LEQ-1P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LESSP-1P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-1P (AT PR THEO)
    (PROG (G F X BETA1 BETA2 GUARD W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA2 THEO))
      (RETURN (CONS 'AND (LIST GUARD W1 W2))))) 
(PUT 'VSUB-3-2-LESSP-2 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-2 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-LESSP-2P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LEQ-2P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LESSP-2P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-2P (AT PR THEO)
    (PROG (G F X DF ALPHA1)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP G NIL) ALPHA1 THEO)))) 
(PUT 'VSUB-3-2-LESSP-3 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-3 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-LESSP-3P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LEQ-3P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LESSP-3P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-3P (AT PR THEO)
    (PROG (G F X DF BETA1 BETA2 GUARD W1 W2 W3)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA2 THEO))
      (RETURN (CONS 'AND (LIST GUARD (RL_MKN 'OR (LIST W1 W2)) W3))))) 
(PUT 'VSUB-3-2-LESSP-4 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-4 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-LESSP-4P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LEQ-4P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LESSP-4P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-4P (AT PR THEO)
    (PROG (G F X DF BETA1 BETA2 GUARD W1 W2 W3)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA2 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'EQUAL DF NIL) BETA2 THEO))
      (RETURN (CONS 'AND (LIST GUARD W1 (RL_MKN 'OR (LIST W2 W3))))))) 
(PUT 'VSUB-3-2-LESSP-5 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-5 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-LESSP-5P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LEQ-5P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LESSP-5P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-5P (AT PR THEO)
    (PROG (G F X DF ALPHA2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP G NIL) ALPHA2 THEO)))) 
(PUT 'VSUB-3-2-LESSP-6 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-6 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-LESSP-6P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LEQ-6P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LESSP-6P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-6P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 ALPHA1 GUARD W1 W2 W3 W4 W5)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DDF NIL) BETA1 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (RETURN (CONS 'AND (LIST GUARD W1 W2 W3 (RL_MKN 'OR (LIST W4 W5))))))) 
(PUT 'VSUB-3-2-LESSP-7 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-7 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-LESSP-7P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LEQ-7P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LESSP-7P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-7P (AT PR THEO)
    (PROG (G F X DF BETA1 BETA2 ALPHA1 ALPHA2 GUARD W1 W2 W3 W4)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP F NIL) BETA2 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD (RL_MKN 'OR (LIST W1 W2))
                   (RL_MKN 'OR (LIST W3 W4))))))) 
(PUT 'VSUB-3-2-LESSP-8 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-8 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-LESSP-8P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LEQ-8P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LESSP-8P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-8P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 ALPHA2 GUARD W1 W2 W3 W4 W5)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA2 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DDF NIL) BETA2 THEO))
      (RETURN (CONS 'AND (LIST GUARD (RL_MKN 'OR (LIST W1 W2)) W3 W4 W5))))) 
(PUT 'VSUB-3-2-LESSP-9 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-9 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-LESSP-9P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LEQ-9P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LESSP-9P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-9P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 ALPHA1 GUARD W1 W2 W3 W4 W5)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DDF NIL) BETA1 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP G NIL) ALPHA1 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD (RL_MKN 'AND (LIST W1 W2 W3))
                   (RL_MKN 'OR (LIST W4 W5))))))) 
(PUT 'VSUB-3-2-LESSP-10 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-10 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-LESSP-10P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LEQ-10P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LESSP-10P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-10P (AT PR THEO)
    (PROG (G F X DF BETA1 BETA2 ALPHA1 ALPHA2 GUARD W1 W2 W3 W4)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP G NIL) ALPHA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP F NIL) BETA2 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP G NIL) ALPHA2 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD (RL_MKN 'OR (LIST W1 W2))
                   (RL_MKN 'OR (LIST W3 W4))))))) 
(PUT 'VSUB-3-2-LESSP-11 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-11 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS
              (VSUB-3-2-LESSP-11P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LEQ-11P (LIST 'LEQ (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LESSP (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LESSP-11P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LESSP-11P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 ALPHA2 GUARD W1 W2 W3 W4 W5)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LESSP G NIL) ALPHA2 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'GREATERP F NIL) BETA2 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DDF NIL) BETA2 THEO))
      (RETURN (CONS 'AND (LIST GUARD (RL_MKN 'OR (LIST W1 W2)) W3 W4 W5))))) 
(PUT 'VSUB-3-2-LEQ-1 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-1 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-2-LEQ-1P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LESSP-1P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LEQ-1P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-1P (AT PR THEO)
    (PROG (G F X BETA1 BETA2 GUARD W1 W2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA2 THEO))
      (RETURN (CONS 'AND (LIST GUARD W1 W2))))) 
(PUT 'VSUB-3-2-LEQ-2 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-2 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-2-LEQ-2P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LESSP-2P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LEQ-2P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-2P (AT PR THEO)
    (PROG (G F X DF ALPHA1)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO)))) 
(PUT 'VSUB-3-2-LEQ-3 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-3 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-2-LEQ-3P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LESSP-3P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LEQ-3P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-3P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 GUARD W1 W2 W3)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA2 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DDF NIL) BETA2 THEO))
      (RETURN (CONS 'AND (LIST GUARD W1 W2 W3))))) 
(PUT 'VSUB-3-2-LEQ-4 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-4 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-2-LEQ-4P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LESSP-4P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LEQ-4P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-4P (AT PR THEO)
    (PROG (G F X DDF BETA1 BETA2 GUARD W1 W2 W3)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DDF (DIFF (DIFF F X) X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DDF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA2 THEO))
      (RETURN (CONS 'AND (LIST GUARD W1 W2 W3))))) 
(PUT 'VSUB-3-2-LEQ-5 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-5 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-2-LEQ-5P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LESSP-5P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LEQ-5P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-5P (AT PR THEO)
    (PROG (G F X DF ALPHA2)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (RETURN (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO)))) 
(PUT 'VSUB-3-2-LEQ-6 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-6 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-2-LEQ-6P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LESSP-6P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LEQ-6P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-6P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 ALPHA1 GUARD W1 W2 W3 W4 W5)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DDF NIL) BETA1 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (RETURN (CONS 'AND (LIST GUARD W1 W2 W3 (RL_MKN 'OR (LIST W4 W5))))))) 
(PUT 'VSUB-3-2-LEQ-7 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-7 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-2-LEQ-7P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LESSP-7P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LEQ-7P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-7P (AT PR THEO)
    (PROG (G F X DF BETA1 BETA2 ALPHA1 ALPHA2 GUARD W1 W2 W3 W4 W5)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ F NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD (RL_MKN 'OR (LIST (RL_MKN 'AND (LIST W1 W2)) W3))
                   (RL_MKN 'OR (LIST W4 W5))))))) 
(PUT 'VSUB-3-2-LEQ-8 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-8 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-2-LEQ-8P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LESSP-8P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LEQ-8P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-8P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 ALPHA2 GUARD W1 W2 W3 W4 W5)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA2 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DDF NIL) BETA2 THEO))
      (RETURN (CONS 'AND (LIST GUARD (RL_MKN 'OR (LIST W1 W2)) W3 W4 W5))))) 
(PUT 'VSUB-3-2-LEQ-9 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-9 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-2-LEQ-9P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LESSP-9P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LEQ-9P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-9P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 ALPHA1 GUARD W1 W2 W3 W4 W5)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DDF NIL) BETA1 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (RETURN (CONS 'AND (LIST GUARD W1 W2 W3 (RL_MKN 'OR (LIST W4 W5))))))) 
(PUT 'VSUB-3-2-LEQ-10 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-10 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-2-LEQ-10P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LESSP-10P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LEQ-10P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-10P (AT PR THEO)
    (PROG (G F X DF BETA1 BETA2 ALPHA1 ALPHA2 GUARD W1 W2 W3 W4 W5)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ ALPHA1 (VSPR_MK DF X (LIST (CONS 1 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ DF NIL) BETA1 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA1 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ F NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (RETURN
       (CONS 'AND
             (LIST GUARD (RL_MKN 'OR (LIST (RL_MKN 'AND (LIST W1 W2)) W3))
                   (RL_MKN 'OR (LIST W4 W5))))))) 
(PUT 'VSUB-3-2-LEQ-11 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-11 (AT PR THEO)
    (PROG (G LCG WPOS WNEG WNULL)
      (SETQ G (CADR AT))
      (SETQ LCG (CDAR G))
      (SETQ WPOS (VSUB-3-2-LEQ-11P AT PR (CONS (LIST 'GREATERP LCG NIL) THEO)))
      (SETQ WNEG
              (LIST 'NOT
                    (VSUB-3-2-LESSP-11P (LIST 'LESSP (NEGF G) NIL) PR
                     (CONS (LIST 'LESSP LCG NIL) THEO))))
      (SETQ WNULL
              (VSUB_VSUB (LIST 'LEQ (CDR G) NIL) PR
               (CONS (LIST 'EQUAL LCG NIL) THEO)))
      (RETURN
       (CONS 'OR
             (LIST (RL_MKN 'AND (LIST (OFSF_0MK2 'GREATERP LCG) WPOS))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'LESSP LCG) WNEG))
                   (RL_MKN 'AND (LIST (OFSF_0MK2 'EQUAL LCG) WNULL))))))) 
(PUT 'VSUB-3-2-LEQ-11P 'NUMBER-OF-ARGS 3) 
(DE VSUB-3-2-LEQ-11P (AT PR THEO)
    (PROG (G F X DF DDF BETA1 BETA2 ALPHA2 GUARD W1 W2 W3 W4 W5)
      (SETQ G (CADR AT))
      (SETQ F (VSPR_F PR))
      (SETQ X (VSPR_V PR))
      (SETQ DF (DIFF F X))
      (SETQ DDF (DIFF DF X))
      (SETQ BETA1 (VSPR_MK G X (LIST (CONS 1 1) (CONS 2 1)) 'TRUE))
      (SETQ BETA2 (VSPR_MK G X (LIST (CONS 1 2) (CONS 2 1)) 'TRUE))
      (SETQ ALPHA2 (VSPR_MK DF X (LIST (CONS 1 2)) 'TRUE))
      (SETQ GUARD (VSUB_GUARD BETA1))
      (SETQ W1 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ F NIL) BETA1 THEO))
      (SETQ W2 (VSDS_APPLYVSTS-AT-PR (LIST 'LEQ G NIL) ALPHA2 THEO))
      (SETQ W3 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ F NIL) BETA2 THEO))
      (SETQ W4 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DF NIL) BETA2 THEO))
      (SETQ W5 (VSDS_APPLYVSTS-AT-PR (LIST 'GEQ DDF NIL) BETA2 THEO))
      (RETURN (CONS 'AND (LIST GUARD (RL_MKN 'OR (LIST W1 W2)) W3 W4 W5))))) 
(PUT 'VSUB_NRSL 'NUMBER-OF-ARGS 1) 
(DE VSUB_NRSL (RSL)
    (PROG (RS FORALL-RESULT FORALL-ENDPTR)
      (SETQ RS RSL)
      (COND ((NULL RS) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (RS) (CONS (MINUS (CAR RS)) (CDR RS)))
                        (CAR RS))
                       NIL)))
     LOOPLABEL
      (SETQ RS (CDR RS))
      (COND ((NULL RS) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (RS) (CONS (MINUS (CAR RS)) (CDR RS))) (CAR RS))
                    NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'OFSF_NEGATEOP 'NUMBER-OF-ARGS 1) 
(DE OFSF_NEGATEOP (OP)
    (COND ((EQ OP 'EQUAL) 'NEQ) ((EQ OP 'NEQ) 'EQUAL) ((EQ OP 'LESSP) 'GEQ)
          ((EQ OP 'LEQ) 'GREATERP) ((EQ OP 'GEQ) 'LESSP)
          ((EQ OP 'GREATERP) 'LEQ))) 
(PUT 'VSUB_COMPUTE-RSL 'NUMBER-OF-ARGS 4) 
(DE VSUB_COMPUTE-RSL (OP F X S)
    (PROG (W)
      (COND (NIL NIL))
      (COND ((GREATERP (CDAAR F) RLMAXDEG*) (RETURN 'FAILED)))
      (SETQ W
              (COND
               (RLCLUSTERING*
                (ASSOC (LIST (CDAAR F) S OP) RSL-ALIST-CLUSTERING*))
               (T (ASSOC (LIST (CDAAR F) S OP) RSL-ALIST*))))
      (COND (W (RETURN (CDR W))))
      (RETURN 'FAILED))) 
(PUT 'VSUB_GUARD 'NUMBER-OF-ARGS 1) 
(DE VSUB_GUARD (PR)
    (PROG (F RTL W)
      (SETQ F (VSPR_F PR))
      (COND (NIL NIL))
      (SETQ RTL (VSPR_RTL PR))
      (SETQ W (ASSOC (LIST (CDAAR F) RTL) GUARD-FNALIST*))
      (COND (W (RETURN (APPLY (CDR W) (LIST F)))))
      (REDERR "no appropriate entry in guard!-fnalist!*"))) 
(PUT 'VSUB_VSUB 'NUMBER-OF-ARGS 3) 
(DE VSUB_VSUB (AT PR THEO)
    (PROG (G X F OP RSL W)
      (SETQ G (CADR AT))
      (SETQ X (VSPR_V PR))
      (COND ((NOT (SFTO_MVARTEST G X)) (RETURN AT)))
      (SETQ F (VSPR_F PR))
      (SETQ OP (COND ((ATOM AT) AT) (T (CAR AT))))
      (SETQ RSL (VSPR_RSL PR))
      (SETQ W (ASSOC (LIST (CDAAR F) (CDAAR G) OP RSL) VSUB-FNALIST*))
      (COND (W (RETURN (CL_NNF (APPLY (CDR W) (LIST AT PR THEO))))))
      (SETQ W
              (ASSOC (LIST (CDAAR F) (CDAAR G) OP (VSUB_NRSL RSL))
                     VSUB-FNALIST*))
      (COND
       (W
        (PROGN
         (SETQ PR
                 (VSPR_MK (NEGF (VSPR_F PR)) (VSPR_V PR) (VSUB_NRSL RSL)
                  (VSPR_RC PR)))
         (RETURN (CL_NNF (APPLY (CDR W) (LIST AT PR THEO)))))))
      (SETQ W
              (ASSOC (LIST (CDAAR F) (CDAAR G) (OFSF_NEGATEOP OP) RSL)
                     VSUB-FNALIST*))
      (COND
       (W
        (PROGN
         (SETQ AT (LIST (OFSF_NEGATEOP OP) G NIL))
         (RETURN (CL_NNF (LIST 'NOT (APPLY (CDR W) (LIST AT PR THEO))))))))
      (SETQ W
              (ASSOC
               (LIST (CDAAR F) (CDAAR G) (OFSF_NEGATEOP OP) (VSUB_NRSL RSL))
               VSUB-FNALIST*))
      (COND
       (W
        (PROGN
         (SETQ AT (LIST (OFSF_NEGATEOP OP) G NIL))
         (SETQ PR
                 (VSPR_MK (NEGF (VSPR_F PR)) (VSPR_V PR) (VSUB_NRSL RSL)
                  (VSPR_RC PR)))
         (RETURN (CL_NNF (LIST 'NOT (APPLY (CDR W) (LIST AT PR THEO))))))))
      (REDERR "no appropriate entry in vsub!-fnalist!*"))) 
(ENDMODULE) 