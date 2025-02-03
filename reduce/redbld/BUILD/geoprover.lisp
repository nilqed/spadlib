(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GEOPROVER)) 
(PUT 'GEOPROVER 'NAME " GeoProver ") 
(PUT 'GEOPROVER 'VERSION " 1.3a ") 
(PUT 'GEOPROVER 'DATE " December 30, 2002") 
(ASSGNPRI (AEVAL " Geoprover ") NIL 'FIRST) 
(ASSGNPRI (AEVAL (GET 'GEOPROVER 'VERSION)) NIL NIL) 
(ASSGNPRI (AEVAL " Last update ") NIL NIL) 
(ASSGNPRI (AEVAL (GET 'GEOPROVER 'DATE)) NIL 'LAST) 
(PUT 'CLEAR_NDG 'NUMBER-OF-ARGS 0) 
(FLAG '(CLEAR_NDG) 'OPFN) 
(PUT 'CLEAR_NDG 'DEFINED-ON-LINE '53) 
(PUT 'CLEAR_NDG 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'CLEAR_NDG 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CLEAR_NDG NIL (SETK '*NDG* (AEVAL (LIST 'LIST)))) 
(PUT 'PRINT_NDG 'NUMBER-OF-ARGS 0) 
(FLAG '(PRINT_NDG) 'OPFN) 
(PUT 'PRINT_NDG 'DEFINED-ON-LINE '54) 
(PUT 'PRINT_NDG 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'PRINT_NDG 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT_NDG NIL '*NDG*) 
(PUT 'ADD_NDG 'NUMBER-OF-ARGS 1) 
(FLAG '(ADD_NDG) 'OPFN) 
(PUT 'ADD_NDG 'DEFINED-ON-LINE '55) 
(PUT 'ADD_NDG 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'ADD_NDG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADD_NDG (D)
    (COND
     ((NOT (MEMBER (REVALX D) (REVALX '*NDG*)))
      (SETK '*NDG* (AEVAL (LIST 'CONS D '*NDG*)))))) 
(CLEAR_NDG) 
(PUT 'IS_EQUAL 'NUMBER-OF-ARGS 2) 
(FLAG '(IS_EQUAL) 'OPFN) 
(PUT 'IS_EQUAL 'DEFINED-ON-LINE '64) 
(PUT 'IS_EQUAL 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'IS_EQUAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IS_EQUAL (A B) (LIST 'DIFFERENCE A B)) 
(PUT 'POINT 'NUMBER-OF-ARGS 2) 
(FLAG '(POINT) 'OPFN) 
(PUT 'POINT 'DEFINED-ON-LINE '66) 
(PUT 'POINT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'POINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POINT (A B) (LIST 'LIST A B)) 
(PUT 'LINE 'NUMBER-OF-ARGS 3) 
(FLAG '(LINE) 'OPFN) 
(PUT 'LINE 'DEFINED-ON-LINE '67) 
(PUT 'LINE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'LINE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LINE (A B C) (LIST 'REDUCE_COORDS (LIST 'LIST A B C))) 
(PUT 'PAR_POINT 'NUMBER-OF-ARGS 3) 
(FLAG '(PAR_POINT) 'OPFN) 
(PUT 'PAR_POINT 'DEFINED-ON-LINE '69) 
(PUT 'PAR_POINT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'PAR_POINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PAR_POINT (A B C)
    (LIST 'POINT
          (LIST 'PLUS (LIST 'DIFFERENCE (LIST 'PART A 1) (LIST 'PART B 1))
                (LIST 'PART C 1))
          (LIST 'PLUS (LIST 'DIFFERENCE (LIST 'PART A 2) (LIST 'PART B 2))
                (LIST 'PART C 2)))) 
(PUT 'PP_LINE 'NUMBER-OF-ARGS 2) 
(FLAG '(PP_LINE) 'OPFN) 
(PUT 'PP_LINE 'DEFINED-ON-LINE '73) 
(PUT 'PP_LINE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'PP_LINE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PP_LINE (A B)
    (LIST 'LINE (LIST 'DIFFERENCE (LIST 'PART B 2) (LIST 'PART A 2))
          (LIST 'DIFFERENCE (LIST 'PART A 1) (LIST 'PART B 1))
          (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'PART A 2) (LIST 'PART B 1))
                (LIST 'TIMES (LIST 'PART A 1) (LIST 'PART B 2))))) 
(PUT 'INTERSECTION_POINT 'NUMBER-OF-ARGS 2) 
(FLAG '(INTERSECTION_POINT) 'OPFN) 
(PUT 'INTERSECTION_POINT 'DEFINED-ON-LINE '78) 
(PUT 'INTERSECTION_POINT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'INTERSECTION_POINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTERSECTION_POINT (A B)
    (PROG (D D1 D2)
      (SETQ D
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'TIMES (LIST 'PART A 1) (LIST 'PART B 2))
                     (LIST 'TIMES (LIST 'PART B 1) (LIST 'PART A 2)))))
      (SETQ D1
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'TIMES (LIST 'PART A 3) (LIST 'PART B 2))
                     (LIST 'TIMES (LIST 'PART B 3) (LIST 'PART A 2)))))
      (SETQ D2
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'TIMES (LIST 'PART A 1) (LIST 'PART B 3))
                     (LIST 'TIMES (LIST 'PART B 1) (LIST 'PART A 3)))))
      (COND
       ((EVALEQUAL (AEVAL D) 0)
        (AEVAL (REDERR (REVALX "Lines are parallel")))))
      (AEVAL (LIST 'ADD_NDG (LIST 'NUM D)))
      (RETURN
       (AEVAL
        (LIST 'POINT (LIST 'MINUS (LIST 'QUOTIENT D1 D))
              (LIST 'MINUS (LIST 'QUOTIENT D2 D))))))) 
(PUT 'ORTHO_LINE 'NUMBER-OF-ARGS 2) 
(FLAG '(ORTHO_LINE) 'OPFN) 
(PUT 'ORTHO_LINE 'DEFINED-ON-LINE '89) 
(PUT 'ORTHO_LINE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'ORTHO_LINE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ORTHO_LINE (P A)
    (PROG (U V)
      (SETQ U (AEVAL (LIST 'FIRST A)))
      (SETQ V (AEVAL (LIST 'SECOND A)))
      (RETURN
       (AEVAL
        (LIST 'LINE V (LIST 'MINUS U)
              (LIST 'DIFFERENCE (LIST 'TIMES U (LIST 'SECOND P))
                    (LIST 'TIMES V (LIST 'FIRST P)))))))) 
(PUT 'PAR_LINE 'NUMBER-OF-ARGS 2) 
(FLAG '(PAR_LINE) 'OPFN) 
(PUT 'PAR_LINE 'DEFINED-ON-LINE '95) 
(PUT 'PAR_LINE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'PAR_LINE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PAR_LINE (P A)
    (LIST 'LINE (LIST 'PART A 1) (LIST 'PART A 2)
          (LIST 'MINUS
                (LIST 'PLUS (LIST 'TIMES (LIST 'PART A 1) (LIST 'PART P 1))
                      (LIST 'TIMES (LIST 'PART A 2) (LIST 'PART P 2)))))) 
(PUT 'VARPOINT 'NUMBER-OF-ARGS 3) 
(FLAG '(VARPOINT) 'OPFN) 
(PUT 'VARPOINT 'DEFINED-ON-LINE '100) 
(PUT 'VARPOINT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'VARPOINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VARPOINT (B A L)
    (LIST 'POINT
          (LIST 'PLUS (LIST 'TIMES L (LIST 'PART A 1))
                (LIST 'TIMES (LIST 'DIFFERENCE 1 L) (LIST 'PART B 1)))
          (LIST 'PLUS (LIST 'TIMES L (LIST 'PART A 2))
                (LIST 'TIMES (LIST 'DIFFERENCE 1 L) (LIST 'PART B 2))))) 
(PUT 'LINE_SLIDER 'NUMBER-OF-ARGS 2) 
(FLAG '(LINE_SLIDER) 'OPFN) 
(PUT 'LINE_SLIDER 'DEFINED-ON-LINE '104) 
(PUT 'LINE_SLIDER 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'LINE_SLIDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LINE_SLIDER (A U)
    (PROG (P D)
      (COND
       ((EVALEQUAL (AEVAL (LIST 'PART A 2)) 0)
        (PROGN
         (SETQ P
                 (AEVAL
                  (LIST 'POINT
                        (LIST 'MINUS
                              (LIST 'QUOTIENT (LIST 'PART A 3)
                                    (LIST 'PART A 1)))
                        U)))
         (SETQ D (AEVAL (LIST 'PART A 1)))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ P
                 (AEVAL
                  (LIST 'POINT U
                        (LIST 'MINUS
                              (LIST 'QUOTIENT
                                    (LIST 'PLUS (LIST 'PART A 3)
                                          (LIST 'TIMES (LIST 'PART A 1) U))
                                    (LIST 'PART A 2))))))
         (SETQ D (AEVAL (LIST 'PART A 2)))
         (AEVAL 'NIL))))
      (AEVAL (LIST 'ADD_NDG (LIST 'NUM D)))
      (RETURN (AEVAL P)))) 
(PUT 'CIRCLE_SLIDER 'NUMBER-OF-ARGS 3) 
(FLAG '(CIRCLE_SLIDER) 'OPFN) 
(PUT 'CIRCLE_SLIDER 'DEFINED-ON-LINE '117) 
(PUT 'CIRCLE_SLIDER 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'CIRCLE_SLIDER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CIRCLE_SLIDER (M A U)
    (PROG (A1 A2 M1 M2 D)
      (SETQ A1 (AEVAL (LIST 'PART A 1)))
      (SETQ A2 (AEVAL (LIST 'PART A 2)))
      (SETQ D (AEVAL (LIST 'PLUS (LIST 'EXPT U 2) 1)))
      (SETQ M1 (AEVAL (LIST 'PART M 1)))
      (SETQ M2 (AEVAL (LIST 'PART M 2)))
      (AEVAL (LIST 'ADD_NDG (LIST 'NUM D)))
      (RETURN
       (AEVAL
        (LIST 'POINT
              (LIST 'QUOTIENT
                    (LIST 'PLUS
                          (LIST 'TIMES A1
                                (LIST 'DIFFERENCE (LIST 'EXPT U 2) 1))
                          (LIST 'TIMES 2 M1)
                          (LIST 'TIMES 2 (LIST 'DIFFERENCE M2 A2) U))
                    D)
              (LIST 'QUOTIENT
                    (LIST 'PLUS A2 (LIST 'TIMES 2 (LIST 'DIFFERENCE M1 A1) U)
                          (LIST 'TIMES (LIST 'DIFFERENCE (LIST 'TIMES 2 M2) A2)
                                (LIST 'EXPT U 2)))
                    D)))))) 
(PUT 'SQRDIST 'NUMBER-OF-ARGS 2) 
(FLAG '(SQRDIST) 'OPFN) 
(PUT 'SQRDIST 'DEFINED-ON-LINE '128) 
(PUT 'SQRDIST 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'SQRDIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SQRDIST (A B)
    (LIST 'PLUS
          (LIST 'EXPT (LIST 'DIFFERENCE (LIST 'PART B 1) (LIST 'PART A 1)) 2)
          (LIST 'EXPT (LIST 'DIFFERENCE (LIST 'PART B 2) (LIST 'PART A 2)) 2))) 
(PUT 'IS_COLLINEAR 'NUMBER-OF-ARGS 3) 
(FLAG '(IS_COLLINEAR) 'OPFN) 
(PUT 'IS_COLLINEAR 'DEFINED-ON-LINE '134) 
(PUT 'IS_COLLINEAR 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'IS_COLLINEAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE IS_COLLINEAR (A B C)
    (LIST 'DET
          (LIST 'MAT (LIST (LIST 'PART A 1) (LIST 'PART A 2) 1)
                (LIST (LIST 'PART B 1) (LIST 'PART B 2) 1)
                (LIST (LIST 'PART C 1) (LIST 'PART C 2) 1)))) 
(PUT 'IS_CONCURRENT 'NUMBER-OF-ARGS 3) 
(FLAG '(IS_CONCURRENT) 'OPFN) 
(PUT 'IS_CONCURRENT 'DEFINED-ON-LINE '140) 
(PUT 'IS_CONCURRENT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'IS_CONCURRENT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE IS_CONCURRENT (A B C)
    (LIST 'DET
          (LIST 'MAT (LIST (LIST 'PART A 1) (LIST 'PART A 2) (LIST 'PART A 3))
                (LIST (LIST 'PART B 1) (LIST 'PART B 2) (LIST 'PART B 3))
                (LIST (LIST 'PART C 1) (LIST 'PART C 2) (LIST 'PART C 3))))) 
(PUT 'IS_PARALLEL 'NUMBER-OF-ARGS 2) 
(FLAG '(IS_PARALLEL) 'OPFN) 
(PUT 'IS_PARALLEL 'DEFINED-ON-LINE '146) 
(PUT 'IS_PARALLEL 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'IS_PARALLEL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IS_PARALLEL (A B)
    (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'PART A 1) (LIST 'PART B 2))
          (LIST 'TIMES (LIST 'PART B 1) (LIST 'PART A 2)))) 
(PUT 'IS_ORTHOGONAL 'NUMBER-OF-ARGS 2) 
(FLAG '(IS_ORTHOGONAL) 'OPFN) 
(PUT 'IS_ORTHOGONAL 'DEFINED-ON-LINE '150) 
(PUT 'IS_ORTHOGONAL 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'IS_ORTHOGONAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IS_ORTHOGONAL (A B)
    (LIST 'PLUS (LIST 'TIMES (LIST 'PART A 1) (LIST 'PART B 1))
          (LIST 'TIMES (LIST 'PART A 2) (LIST 'PART B 2)))) 
(PUT 'ON_LINE 'NUMBER-OF-ARGS 2) 
(FLAG '(ON_LINE) 'OPFN) 
(PUT 'ON_LINE 'DEFINED-ON-LINE '154) 
(PUT 'ON_LINE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'ON_LINE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ON_LINE (P A)
    (LIST 'PLUS (LIST 'TIMES (LIST 'PART P 1) (LIST 'PART A 1))
          (LIST 'TIMES (LIST 'PART P 2) (LIST 'PART A 2)) (LIST 'PART A 3))) 
(PUT 'EQ_DIST 'NUMBER-OF-ARGS 4) 
(FLAG '(EQ_DIST) 'OPFN) 
(PUT 'EQ_DIST 'DEFINED-ON-LINE '158) 
(PUT 'EQ_DIST 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'EQ_DIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EQ_DIST (A B C D)
    (LIST 'DIFFERENCE (LIST 'SQRDIST A B) (LIST 'SQRDIST C D))) 
(PUT 'L2_ANGLE 'NUMBER-OF-ARGS 2) 
(FLAG '(L2_ANGLE) 'OPFN) 
(PUT 'L2_ANGLE 'DEFINED-ON-LINE '169) 
(PUT 'L2_ANGLE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'L2_ANGLE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE L2_ANGLE (A B)
    (PROG (D)
      (SETQ D
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES (LIST 'PART A 1) (LIST 'PART B 1))
                     (LIST 'TIMES (LIST 'PART A 2) (LIST 'PART B 2)))))
      (AEVAL (LIST 'ADD_NDG (LIST 'NUM D)))
      (RETURN
       (AEVAL
        (LIST 'QUOTIENT
              (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'PART A 2) (LIST 'PART B 1))
                    (LIST 'TIMES (LIST 'PART B 2) (LIST 'PART A 1)))
              D))))) 
(PUT 'ANGLE_SUM 'NUMBER-OF-ARGS 2) 
(FLAG '(ANGLE_SUM) 'OPFN) 
(PUT 'ANGLE_SUM 'DEFINED-ON-LINE '176) 
(PUT 'ANGLE_SUM 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'ANGLE_SUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ANGLE_SUM (A B)
    (PROG (D)
      (SETQ D (AEVAL (LIST 'DIFFERENCE 1 (LIST 'TIMES A B))))
      (AEVAL (LIST 'ADD_NDG (LIST 'NUM D)))
      (RETURN (AEVAL (LIST 'QUOTIENT (LIST 'PLUS A B) D))))) 
(PUT 'EQ_ANGLE 'NUMBER-OF-ARGS 6) 
(FLAG '(EQ_ANGLE) 'OPFN) 
(PUT 'EQ_ANGLE 'DEFINED-ON-LINE '182) 
(PUT 'EQ_ANGLE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'EQ_ANGLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EQ_ANGLE (A B C D E F)
    (LIST 'DIFFERENCE (LIST 'P3_ANGLE A B C) (LIST 'P3_ANGLE D E F))) 
(PUT 'ON_BISECTOR 'NUMBER-OF-ARGS 4) 
(FLAG '(ON_BISECTOR) 'OPFN) 
(PUT 'ON_BISECTOR 'DEFINED-ON-LINE '185) 
(PUT 'ON_BISECTOR 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'ON_BISECTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ON_BISECTOR (P A B C)
    (PROG (A1 A2 B1 B2 C1 C2 P1 P2)
      (SETQ A1 (AEVAL (LIST 'PART A 1)))
      (SETQ A2 (AEVAL (LIST 'PART A 2)))
      (SETQ B1 (AEVAL (LIST 'PART B 1)))
      (SETQ B2 (AEVAL (LIST 'PART B 2)))
      (SETQ C1 (AEVAL (LIST 'PART C 1)))
      (SETQ C2 (AEVAL (LIST 'PART C 2)))
      (SETQ P1 (AEVAL (LIST 'PART P 1)))
      (SETQ P2 (AEVAL (LIST 'PART P 2)))
      (RETURN
       (AEVAL
        (LIST 'DIFFERENCE
              (LIST 'TIMES
                    (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES A1 B2))
                          (LIST 'TIMES A1 P2)
                          (LIST 'DIFFERENCE
                                (LIST 'DIFFERENCE (LIST 'TIMES A2 B1)
                                      (LIST 'TIMES A2 P1))
                                (LIST 'TIMES B1 P2))
                          (LIST 'TIMES B2 P1))
                    (LIST 'PLUS
                          (LIST 'DIFFERENCE
                                (LIST 'DIFFERENCE (LIST 'EXPT B1 2)
                                      (LIST 'TIMES B1 C1))
                                (LIST 'TIMES B1 P1))
                          (LIST 'DIFFERENCE
                                (LIST 'DIFFERENCE (LIST 'EXPT B2 2)
                                      (LIST 'TIMES B2 C2))
                                (LIST 'TIMES B2 P2))
                          (LIST 'TIMES C1 P1) (LIST 'TIMES C2 P2)))
              (LIST 'TIMES
                    (LIST 'PLUS
                          (LIST 'DIFFERENCE (LIST 'TIMES A1 B1)
                                (LIST 'TIMES A1 P1))
                          (LIST 'DIFFERENCE
                                (LIST 'DIFFERENCE (LIST 'TIMES A2 B2)
                                      (LIST 'TIMES A2 P2))
                                (LIST 'EXPT B1 2))
                          (LIST 'DIFFERENCE (LIST 'TIMES B1 P1)
                                (LIST 'EXPT B2 2))
                          (LIST 'TIMES B2 P2))
                    (LIST 'PLUS
                          (LIST 'DIFFERENCE
                                (LIST 'DIFFERENCE (LIST 'TIMES B1 C2)
                                      (LIST 'TIMES B1 P2))
                                (LIST 'TIMES B2 C1))
                          (LIST 'TIMES B2 P1)
                          (LIST 'DIFFERENCE (LIST 'TIMES C1 P2)
                                (LIST 'TIMES C2 P1))))))))) 
(PUT 'ROTATE 'NUMBER-OF-ARGS 3) 
(FLAG '(ROTATE) 'OPFN) 
(PUT 'ROTATE 'DEFINED-ON-LINE '201) 
(PUT 'ROTATE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'ROTATE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ROTATE (C A ANGLE)
    (PROG (AC1 AC2)
      (SETQ AC1 (AEVAL (LIST 'DIFFERENCE (LIST 'PART A 1) (LIST 'PART C 1))))
      (SETQ AC2 (AEVAL (LIST 'DIFFERENCE (LIST 'PART A 2) (LIST 'PART C 2))))
      (RETURN
       (AEVAL
        (LIST 'POINT
              (LIST 'PLUS (LIST 'PART C 1)
                    (LIST 'DIFFERENCE
                          (LIST 'TIMES AC1 (LIST 'COS (LIST 'TIMES ANGLE 'PI)))
                          (LIST 'TIMES AC2
                                (LIST 'SIN (LIST 'TIMES ANGLE 'PI)))))
              (LIST 'PLUS (LIST 'PART C 2)
                    (LIST 'TIMES AC1 (LIST 'SIN (LIST 'TIMES ANGLE 'PI)))
                    (LIST 'TIMES AC2 (LIST 'COS (LIST 'TIMES ANGLE 'PI))))))))) 
(PUT 'SYM_LINE 'NUMBER-OF-ARGS 2) 
(FLAG '(SYM_LINE) 'OPFN) 
(PUT 'SYM_LINE 'DEFINED-ON-LINE '210) 
(PUT 'SYM_LINE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'SYM_LINE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SYM_LINE (A L)
    (PROG (A1 A2 A3 L1 L2 L3 U)
      (SETQ A1 (AEVAL (LIST 'PART A 1)))
      (SETQ A2 (AEVAL (LIST 'PART A 2)))
      (SETQ A3 (AEVAL (LIST 'PART A 3)))
      (SETQ L1 (AEVAL (LIST 'PART L 1)))
      (SETQ L2 (AEVAL (LIST 'PART L 2)))
      (SETQ L3 (AEVAL (LIST 'PART L 3)))
      (SETQ U (AEVAL (LIST 'DIFFERENCE (LIST 'EXPT L1 2) (LIST 'EXPT L2 2))))
      (RETURN
       (AEVAL
        (LIST 'LINE
              (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'TIMES A1 U))
                    (LIST 'TIMES 2 A2 L1 L2))
              (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES 2 A1 L1 L2))
                    (LIST 'TIMES A2 U))
              (LIST 'PLUS
                    (LIST 'MINUS
                          (LIST 'TIMES 2
                                (LIST 'PLUS (LIST 'TIMES A1 L1)
                                      (LIST 'TIMES A2 L2))
                                L3))
                    (LIST 'TIMES A3
                          (LIST 'PLUS (LIST 'EXPT L1 2)
                                (LIST 'EXPT L2 2))))))))) 
(PUT 'CIRCLE 'NUMBER-OF-ARGS 4) 
(FLAG '(CIRCLE) 'OPFN) 
(PUT 'CIRCLE 'DEFINED-ON-LINE '222) 
(PUT 'CIRCLE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'CIRCLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CIRCLE (C1 C2 C3 C4) (LIST 'REDUCE_COORDS (LIST 'LIST C1 C2 C3 C4))) 
(PUT 'PC_CIRCLE 'NUMBER-OF-ARGS 2) 
(FLAG '(PC_CIRCLE) 'OPFN) 
(PUT 'PC_CIRCLE 'DEFINED-ON-LINE '224) 
(PUT 'PC_CIRCLE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'PC_CIRCLE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PC_CIRCLE (M A)
    (LIST 'CIRCLE 1 (LIST 'MINUS (LIST 'TIMES 2 (LIST 'PART M 1)))
          (LIST 'MINUS (LIST 'TIMES 2 (LIST 'PART M 2)))
          (LIST 'PLUS
                (LIST 'TIMES (LIST 'PART A 1)
                      (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'PART M 1))
                            (LIST 'PART A 1)))
                (LIST 'TIMES (LIST 'PART A 2)
                      (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'PART M 2))
                            (LIST 'PART A 2)))))) 
(PUT 'CIRCLE_CENTER 'NUMBER-OF-ARGS 1) 
(FLAG '(CIRCLE_CENTER) 'OPFN) 
(PUT 'CIRCLE_CENTER 'DEFINED-ON-LINE '230) 
(PUT 'CIRCLE_CENTER 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'CIRCLE_CENTER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CIRCLE_CENTER (C)
    (PROG ()
      (AEVAL (LIST 'ADD_NDG (LIST 'NUM (LIST 'PART C 1))))
      (RETURN
       (AEVAL
        (LIST 'POINT
              (LIST 'MINUS
                    (LIST 'QUOTIENT (LIST 'QUOTIENT (LIST 'PART C 2) 2)
                          (LIST 'PART C 1)))
              (LIST 'MINUS
                    (LIST 'QUOTIENT (LIST 'PART C 3)
                          (LIST 'TIMES 2 (LIST 'PART C 1))))))))) 
(PUT 'CIRCLE_SQRADIUS 'NUMBER-OF-ARGS 1) 
(FLAG '(CIRCLE_SQRADIUS) 'OPFN) 
(PUT 'CIRCLE_SQRADIUS 'DEFINED-ON-LINE '236) 
(PUT 'CIRCLE_SQRADIUS 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'CIRCLE_SQRADIUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CIRCLE_SQRADIUS (C)
    (PROG ()
      (AEVAL (LIST 'ADD_NDG (LIST 'NUM (LIST 'PART C 1))))
      (RETURN
       (AEVAL
        (LIST 'QUOTIENT
              (LIST 'DIFFERENCE
                    (LIST 'PLUS (LIST 'EXPT (LIST 'PART C 2) 2)
                          (LIST 'EXPT (LIST 'PART C 3) 2))
                    (LIST 'TIMES 4 (LIST 'PART C 4) (LIST 'PART C 1)))
              (LIST 'EXPT (LIST 'TIMES 2 (LIST 'PART C 1)) 2)))))) 
(PUT 'P3_CIRCLE 'NUMBER-OF-ARGS 3) 
(FLAG '(P3_CIRCLE) 'OPFN) 
(PUT 'P3_CIRCLE 'DEFINED-ON-LINE '244) 
(PUT 'P3_CIRCLE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'P3_CIRCLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE P3_CIRCLE (A B C)
    (PROG (A1 A2 A3 B1 B2 B3 C1 C2 C3)
      (SETQ A1 (AEVAL (LIST 'PART A 1)))
      (SETQ A2 (AEVAL (LIST 'PART A 2)))
      (SETQ A3 (AEVAL (LIST 'PLUS (LIST 'EXPT A1 2) (LIST 'EXPT A2 2))))
      (SETQ B1 (AEVAL (LIST 'PART B 1)))
      (SETQ B2 (AEVAL (LIST 'PART B 2)))
      (SETQ B3 (AEVAL (LIST 'PLUS (LIST 'EXPT B1 2) (LIST 'EXPT B2 2))))
      (SETQ C1 (AEVAL (LIST 'PART C 1)))
      (SETQ C2 (AEVAL (LIST 'PART C 2)))
      (SETQ C3 (AEVAL (LIST 'PLUS (LIST 'EXPT C1 2) (LIST 'EXPT C2 2))))
      (RETURN
       (AEVAL
        (LIST 'CIRCLE
              (LIST 'PLUS (LIST 'TIMES A1 (LIST 'DIFFERENCE B2 C2))
                    (LIST 'TIMES (LIST 'DIFFERENCE A2 B2) C1)
                    (LIST 'TIMES B1 (LIST 'DIFFERENCE C2 A2)))
              (LIST 'PLUS (LIST 'TIMES A3 (LIST 'DIFFERENCE C2 B2))
                    (LIST 'TIMES (LIST 'DIFFERENCE A2 C2) B3)
                    (LIST 'TIMES (LIST 'DIFFERENCE B2 A2) C3))
              (LIST 'PLUS (LIST 'TIMES A3 (LIST 'DIFFERENCE B1 C1))
                    (LIST 'TIMES (LIST 'DIFFERENCE C1 A1) B3)
                    (LIST 'TIMES (LIST 'DIFFERENCE A1 B1) C3))
              (LIST 'PLUS
                    (LIST 'TIMES A3
                          (LIST 'DIFFERENCE (LIST 'TIMES B2 C1)
                                (LIST 'TIMES B1 C2)))
                    (LIST 'TIMES
                          (LIST 'DIFFERENCE (LIST 'TIMES A1 C2)
                                (LIST 'TIMES A2 C1))
                          B3)
                    (LIST 'TIMES
                          (LIST 'DIFFERENCE (LIST 'TIMES A2 B1)
                                (LIST 'TIMES A1 B2))
                          C3))))))) 
(PUT 'ON_CIRCLE 'NUMBER-OF-ARGS 2) 
(FLAG '(ON_CIRCLE) 'OPFN) 
(PUT 'ON_CIRCLE 'DEFINED-ON-LINE '256) 
(PUT 'ON_CIRCLE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'ON_CIRCLE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ON_CIRCLE (P C)
    (PROG (P1 P2)
      (SETQ P1 (AEVAL (LIST 'PART P 1)))
      (SETQ P2 (AEVAL (LIST 'PART P 2)))
      (RETURN
       (AEVAL
        (LIST 'PLUS
              (LIST 'TIMES (LIST 'PART C 1)
                    (LIST 'PLUS (LIST 'EXPT P1 2) (LIST 'EXPT P2 2)))
              (LIST 'TIMES (LIST 'PART C 2) P1)
              (LIST 'TIMES (LIST 'PART C 3) P2) (LIST 'PART C 4)))))) 
(PUT 'OTHER_CL_POINT 'NUMBER-OF-ARGS 3) 
(FLAG '(OTHER_CL_POINT) 'OPFN) 
(PUT 'OTHER_CL_POINT 'DEFINED-ON-LINE '263) 
(PUT 'OTHER_CL_POINT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'OTHER_CL_POINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OTHER_CL_POINT (P C L)
    (COND
     ((EVALNEQ (AEVAL (LIST 'ON_LINE P L)) 0)
      (AEVAL (REDERR (REVALX "Point not on the line"))))
     ((EVALNEQ (AEVAL (LIST 'ON_CIRCLE P C)) 0)
      (AEVAL (REDERR (REVALX "Point not on the circle"))))
     (T
      (PROG (C1 C2 C3 L1 L2 D D1 P1 P2)
        (SETQ C1 (AEVAL (LIST 'PART C 1)))
        (SETQ C2 (AEVAL (LIST 'PART C 2)))
        (SETQ C3 (AEVAL (LIST 'PART C 3)))
        (SETQ L1 (AEVAL (LIST 'PART L 1)))
        (SETQ L2 (AEVAL (LIST 'PART L 2)))
        (SETQ P1 (AEVAL (LIST 'PART P 1)))
        (SETQ P2 (AEVAL (LIST 'PART P 2)))
        (SETQ D
                (AEVAL
                 (LIST 'TIMES C1
                       (LIST 'PLUS (LIST 'EXPT L1 2) (LIST 'EXPT L2 2)))))
        (AEVAL (LIST 'ADD_NDG (LIST 'NUM D)))
        (SETQ D1
                (AEVAL
                 (LIST 'TIMES C1
                       (LIST 'DIFFERENCE (LIST 'EXPT L1 2)
                             (LIST 'EXPT L2 2)))))
        (RETURN
         (AEVAL
          (LIST 'LIST
                (LIST 'QUOTIENT
                      (LIST 'PLUS (LIST 'TIMES D1 P1)
                            (LIST 'TIMES
                                  (LIST 'DIFFERENCE
                                        (LIST 'TIMES
                                              (LIST 'PLUS (LIST 'TIMES 2 C1 P2)
                                                    C3)
                                              L1)
                                        (LIST 'TIMES C2 L2))
                                  L2))
                      D)
                (LIST 'QUOTIENT
                      (LIST 'PLUS (LIST 'MINUS (LIST 'TIMES D1 P2))
                            (LIST 'TIMES
                                  (LIST 'DIFFERENCE
                                        (LIST 'TIMES
                                              (LIST 'PLUS (LIST 'TIMES 2 C1 P1)
                                                    C2)
                                              L2)
                                        (LIST 'TIMES C3 L1))
                                  L1))
                      D)))))))) 
(PUT 'RADICAL_AXIS 'NUMBER-OF-ARGS 2) 
(FLAG '(RADICAL_AXIS) 'OPFN) 
(PUT 'RADICAL_AXIS 'DEFINED-ON-LINE '277) 
(PUT 'RADICAL_AXIS 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'RADICAL_AXIS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RADICAL_AXIS (C1 C2)
    (PROG (I FORALL-RESULT FORALL-ENDPTR)
      (SETQ I 2)
      (COND ((MINUSP (DIFFERENCE 4 I)) (RETURN (MAKELIST NIL))))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       (AEVAL*
                        (LIST 'DIFFERENCE
                              (LIST 'TIMES (LIST 'PART C1 1) (LIST 'PART C2 I))
                              (LIST 'TIMES (LIST 'PART C1 I)
                                    (LIST 'PART C2 1))))
                       NIL)))
     LOOPLABEL
      (SETQ I (PLUS2 I 1))
      (COND ((MINUSP (DIFFERENCE 4 I)) (RETURN (CONS 'LIST FORALL-RESULT))))
      (RPLACD FORALL-ENDPTR
              (CONS
               (AEVAL*
                (LIST 'DIFFERENCE
                      (LIST 'TIMES (LIST 'PART C1 1) (LIST 'PART C2 I))
                      (LIST 'TIMES (LIST 'PART C1 I) (LIST 'PART C2 1))))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'OTHER_CC_POINT 'NUMBER-OF-ARGS 3) 
(FLAG '(OTHER_CC_POINT) 'OPFN) 
(PUT 'OTHER_CC_POINT 'DEFINED-ON-LINE '283) 
(PUT 'OTHER_CC_POINT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'OTHER_CC_POINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OTHER_CC_POINT (P C1 C2)
    (LIST 'OTHER_CL_POINT P C1 (LIST 'RADICAL_AXIS C1 C2))) 
(PUT 'IS_CL_TANGENT 'NUMBER-OF-ARGS 2) 
(FLAG '(IS_CL_TANGENT) 'OPFN) 
(PUT 'IS_CL_TANGENT 'DEFINED-ON-LINE '288) 
(PUT 'IS_CL_TANGENT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'IS_CL_TANGENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IS_CL_TANGENT (C L)
    (PROG (C1 C2 C3 C4 L1 L2 L3)
      (SETQ C1 (AEVAL (LIST 'PART C 1)))
      (SETQ C2 (AEVAL (LIST 'PART C 2)))
      (SETQ C3 (AEVAL (LIST 'PART C 3)))
      (SETQ C4 (AEVAL (LIST 'PART C 4)))
      (SETQ L1 (AEVAL (LIST 'PART L 1)))
      (SETQ L2 (AEVAL (LIST 'PART L 2)))
      (SETQ L3 (AEVAL (LIST 'PART L 3)))
      (RETURN
       (AEVAL
        (LIST 'PLUS
              (LIST 'MINUS (LIST 'TIMES 4 (LIST 'EXPT C1 2) (LIST 'EXPT L3 2)))
              (LIST 'TIMES 4 C1 C2 L1 L3)
              (LIST 'DIFFERENCE
                    (LIST 'DIFFERENCE (LIST 'TIMES 4 C1 C3 L2 L3)
                          (LIST 'TIMES 4 C1 C4 (LIST 'EXPT L1 2)))
                    (LIST 'TIMES 4 C1 C4 (LIST 'EXPT L2 2)))
              (LIST 'DIFFERENCE
                    (LIST 'TIMES (LIST 'EXPT C2 2) (LIST 'EXPT L2 2))
                    (LIST 'TIMES 2 C2 C3 L1 L2))
              (LIST 'TIMES (LIST 'EXPT C3 2) (LIST 'EXPT L1 2))))))) 
(PUT 'IS_CC_TANGENT 'NUMBER-OF-ARGS 2) 
(FLAG '(IS_CC_TANGENT) 'OPFN) 
(PUT 'IS_CC_TANGENT 'DEFINED-ON-LINE '298) 
(PUT 'IS_CC_TANGENT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'IS_CC_TANGENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IS_CC_TANGENT (C D)
    (PROG (C1 C2 C3 C4 D1 D2 D3 D4)
      (SETQ C1 (AEVAL (LIST 'PART C 1)))
      (SETQ C2 (AEVAL (LIST 'PART C 2)))
      (SETQ C3 (AEVAL (LIST 'PART C 3)))
      (SETQ C4 (AEVAL (LIST 'PART C 4)))
      (SETQ D1 (AEVAL (LIST 'PART D 1)))
      (SETQ D2 (AEVAL (LIST 'PART D 2)))
      (SETQ D3 (AEVAL (LIST 'PART D 3)))
      (SETQ D4 (AEVAL (LIST 'PART D 4)))
      (RETURN
       (AEVAL
        (LIST 'PLUS
              (LIST 'DIFFERENCE
                    (LIST 'DIFFERENCE
                          (LIST 'DIFFERENCE
                                (LIST 'TIMES 4 (LIST 'EXPT C1 2)
                                      (LIST 'EXPT D4 2))
                                (LIST 'TIMES 4 C1 C2 D2 D4))
                          (LIST 'TIMES 4 C1 C3 D3 D4))
                    (LIST 'TIMES 8 C1 C4 D1 D4))
              (LIST 'TIMES 4 C1 C4 (LIST 'EXPT D2 2))
              (LIST 'TIMES 4 C1 C4 (LIST 'EXPT D3 2))
              (LIST 'DIFFERENCE (LIST 'TIMES 4 (LIST 'EXPT C2 2) D1 D4)
                    (LIST 'TIMES (LIST 'EXPT C2 2) (LIST 'EXPT D3 2)))
              (LIST 'DIFFERENCE (LIST 'TIMES 2 C2 C3 D2 D3)
                    (LIST 'TIMES 4 C2 C4 D1 D2))
              (LIST 'DIFFERENCE
                    (LIST 'DIFFERENCE (LIST 'TIMES 4 (LIST 'EXPT C3 2) D1 D4)
                          (LIST 'TIMES (LIST 'EXPT C3 2) (LIST 'EXPT D2 2)))
                    (LIST 'TIMES 4 C3 C4 D1 D3))
              (LIST 'TIMES 4 (LIST 'EXPT C4 2) (LIST 'EXPT D1 2))))))) 
(FLAG '(LIST2MAT) 'OPFN) 
(PUT 'LIST2MAT 'NUMBER-OF-ARGS 1) 
(PUT 'LIST2MAT 'DEFINED-ON-LINE '313) 
(PUT 'LIST2MAT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'LIST2MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIST2MAT (U)
    (CONS 'MAT
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X (CDR (REVAL1 U T)))
            (COND ((NULL X) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL)))
           LOOPLABEL
            (SETQ X (CDR X))
            (COND ((NULL X) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'EXTRACTMAT 'NUMBER-OF-ARGS 2) 
(FLAG '(EXTRACTMAT) 'OPFN) 
(PUT 'EXTRACTMAT 'DEFINED-ON-LINE '316) 
(PUT 'EXTRACTMAT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'EXTRACTMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXTRACTMAT (POLYS VARS)
    (PROG ()
      (COND
       ((EVALNEQ (AEVAL (LIST 'LENGTH POLYS)) (AEVAL (LIST 'LENGTH VARS)))
        (AEVAL (REDERR (REVALX "Number of variables doesn't match")))))
      (PROG (P)
        (SETQ P (GETRLIST (AEVAL POLYS)))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROG (X)
             (SETQ X (GETRLIST (AEVAL VARS)))
            LAB
             (COND ((NULL X) (RETURN NIL)))
             ((LAMBDA (X)
                (COND
                 ((EVALGREATERP (AEVAL (LIST 'DEG P X)) 1)
                  (AEVAL (REDERR (REVALX "Equations not of linear type"))))))
              (CAR X))
             (SETQ X (CDR X))
             (GO LAB)))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN
       (AEVAL
        (LIST 'LIST2MAT
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (GETRLIST (AEVAL VARS)))
                (COND ((NULL X) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X)
                                    (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ P (GETRLIST (AEVAL POLYS)))
                                      (COND ((NULL P) (RETURN (MAKELIST NIL))))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (P)
                                                          (AEVAL
                                                           (LIST 'COEFFN P X
                                                                 1)))
                                                        (CAR P))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ P (CDR P))
                                      (COND
                                       ((NULL P)
                                        (RETURN (CONS 'LIST FORALL-RESULT))))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (P)
                                                  (AEVAL (LIST 'COEFFN P X 1)))
                                                (CAR P))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X)
                            (PROG (P FORALL-RESULT FORALL-ENDPTR)
                              (SETQ P (GETRLIST (AEVAL POLYS)))
                              (COND ((NULL P) (RETURN (MAKELIST NIL))))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (P)
                                                  (AEVAL (LIST 'COEFFN P X 1)))
                                                (CAR P))
                                               NIL)))
                             LOOPLABEL
                              (SETQ P (CDR P))
                              (COND
                               ((NULL P) (RETURN (CONS 'LIST FORALL-RESULT))))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (P)
                                          (AEVAL (LIST 'COEFFN P X 1)))
                                        (CAR P))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'REDUCE_COORDS 'NUMBER-OF-ARGS 1) 
(FLAG '(REDUCE_COORDS) 'OPFN) 
(PUT 'REDUCE_COORDS 'DEFINED-ON-LINE '328) 
(PUT 'REDUCE_COORDS 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'REDUCE_COORDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDUCE_COORDS (U)
    (PROG (L G)
      (SETQ L (AEVAL (LIST 'DEN (LIST 'FIRST U))))
      (SETQ G (AEVAL (LIST 'NUM (LIST 'FIRST U))))
      (PROG (X)
        (SETQ X (GETRLIST (AEVAL (LIST 'REST U))))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ L (AEVAL (LIST 'LCM L (LIST 'DEN X))))
            (SETQ G (AEVAL (LIST 'GCD G (LIST 'NUM X))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (AEVAL (LIST 'ADD_NDG G))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (GETRLIST (AEVAL U)))
         (COND ((NULL X) (RETURN (MAKELIST NIL))))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (X)
                             (AEVAL (LIST 'TIMES X (LIST 'QUOTIENT L G))))
                           (CAR X))
                          NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN (CONS 'LIST FORALL-RESULT))))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (X) (AEVAL (LIST 'TIMES X (LIST 'QUOTIENT L G))))
                   (CAR X))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'CIRCLE_INVERSE 'NUMBER-OF-ARGS 3) 
(FLAG '(CIRCLE_INVERSE) 'OPFN) 
(PUT 'CIRCLE_INVERSE 'DEFINED-ON-LINE '339) 
(PUT 'CIRCLE_INVERSE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'CIRCLE_INVERSE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CIRCLE_INVERSE (M R P)
    (PROG (M1 M2 R1 R2 P1 P2 D)
      (SETQ M1 (AEVAL (LIST 'PART M 1)))
      (SETQ M2 (AEVAL (LIST 'PART M 2)))
      (SETQ R1 (AEVAL (LIST 'PART R 1)))
      (SETQ R2 (AEVAL (LIST 'PART R 2)))
      (SETQ P1 (AEVAL (LIST 'PART P 1)))
      (SETQ P2 (AEVAL (LIST 'PART P 2)))
      (SETQ D
              (AEVAL
               (LIST 'PLUS (LIST 'EXPT (LIST 'DIFFERENCE M1 P1) 2)
                     (LIST 'EXPT (LIST 'DIFFERENCE M2 P2) 2))))
      (AEVAL (LIST 'ADD_NDG D))
      (RETURN
       (AEVAL
        (LIST 'QUOTIENT
              (LIST 'PLUS (LIST 'EXPT (LIST 'DIFFERENCE M1 P1) 2)
                    (LIST 'EXPT (LIST 'DIFFERENCE M2 P2) 2)
                    (LIST 'EXPT (LIST 'DIFFERENCE M1 R1) 2)
                    (LIST 'EXPT (LIST 'DIFFERENCE M2 R2) 2))
              D))))) 
(PUT 'ALTITUDE 'NUMBER-OF-ARGS 3) 
(FLAG '(ALTITUDE) 'OPFN) 
(PUT 'ALTITUDE 'DEFINED-ON-LINE '352) 
(PUT 'ALTITUDE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'ALTITUDE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ALTITUDE (A__ B__ C__) (LIST 'ORTHO_LINE A__ (LIST 'PP_LINE B__ C__))) 
(PUT 'CENTROID 'NUMBER-OF-ARGS 3) 
(FLAG '(CENTROID) 'OPFN) 
(PUT 'CENTROID 'DEFINED-ON-LINE '355) 
(PUT 'CENTROID 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'CENTROID 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CENTROID (A__ B__ C__)
    (LIST 'INTERSECTION_POINT (LIST 'MEDIAN A__ B__ C__)
          (LIST 'MEDIAN B__ C__ A__))) 
(PUT 'CIRCUMCENTER 'NUMBER-OF-ARGS 3) 
(FLAG '(CIRCUMCENTER) 'OPFN) 
(PUT 'CIRCUMCENTER 'DEFINED-ON-LINE '358) 
(PUT 'CIRCUMCENTER 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'CIRCUMCENTER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CIRCUMCENTER (A__ B__ C__)
    (LIST 'INTERSECTION_POINT (LIST 'P_BISECTOR A__ B__)
          (LIST 'P_BISECTOR B__ C__))) 
(PUT 'CSYM_POINT 'NUMBER-OF-ARGS 2) 
(FLAG '(CSYM_POINT) 'OPFN) 
(PUT 'CSYM_POINT 'DEFINED-ON-LINE '361) 
(PUT 'CSYM_POINT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'CSYM_POINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CSYM_POINT (P__ Q__) (LIST 'VARPOINT Q__ P__ (MINUS 1))) 
(PUT 'FIXEDPOINT 'NUMBER-OF-ARGS 3) 
(FLAG '(FIXEDPOINT) 'OPFN) 
(PUT 'FIXEDPOINT 'DEFINED-ON-LINE '364) 
(PUT 'FIXEDPOINT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'FIXEDPOINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FIXEDPOINT (A__ B__ U_) (LIST 'VARPOINT A__ B__ U_)) 
(PUT 'IS_CONCYCLIC 'NUMBER-OF-ARGS 4) 
(FLAG '(IS_CONCYCLIC) 'OPFN) 
(PUT 'IS_CONCYCLIC 'DEFINED-ON-LINE '367) 
(PUT 'IS_CONCYCLIC 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'IS_CONCYCLIC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE IS_CONCYCLIC (A__ B__ C__ D__)
    (LIST 'ON_CIRCLE D__ (LIST 'P3_CIRCLE A__ B__ C__))) 
(PUT 'MEDIAN 'NUMBER-OF-ARGS 3) 
(FLAG '(MEDIAN) 'OPFN) 
(PUT 'MEDIAN 'DEFINED-ON-LINE '370) 
(PUT 'MEDIAN 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'MEDIAN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MEDIAN (A__ B__ C__) (LIST 'PP_LINE A__ (LIST 'MIDPOINT B__ C__))) 
(PUT 'MIDPOINT 'NUMBER-OF-ARGS 2) 
(FLAG '(MIDPOINT) 'OPFN) 
(PUT 'MIDPOINT 'DEFINED-ON-LINE '373) 
(PUT 'MIDPOINT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'MIDPOINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MIDPOINT (A__ B__) (LIST 'FIXEDPOINT A__ B__ (LIST 'QUOTIENT 1 2))) 
(PUT 'ORTHOCENTER 'NUMBER-OF-ARGS 3) 
(FLAG '(ORTHOCENTER) 'OPFN) 
(PUT 'ORTHOCENTER 'DEFINED-ON-LINE '376) 
(PUT 'ORTHOCENTER 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'ORTHOCENTER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ORTHOCENTER (A__ B__ C__)
    (LIST 'INTERSECTION_POINT (LIST 'ALTITUDE A__ B__ C__)
          (LIST 'ALTITUDE B__ C__ A__))) 
(PUT 'OTHER_INCENTER 'NUMBER-OF-ARGS 3) 
(FLAG '(OTHER_INCENTER) 'OPFN) 
(PUT 'OTHER_INCENTER 'DEFINED-ON-LINE '379) 
(PUT 'OTHER_INCENTER 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'OTHER_INCENTER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OTHER_INCENTER (M__ A__ B__)
    (LIST 'INTERSECTION_POINT (LIST 'ORTHO_LINE A__ (LIST 'PP_LINE M__ A__))
          (LIST 'ORTHO_LINE B__ (LIST 'PP_LINE M__ B__)))) 
(PUT 'P3_ANGLE 'NUMBER-OF-ARGS 3) 
(FLAG '(P3_ANGLE) 'OPFN) 
(PUT 'P3_ANGLE 'DEFINED-ON-LINE '383) 
(PUT 'P3_ANGLE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'P3_ANGLE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE P3_ANGLE (A__ B__ C__)
    (LIST 'L2_ANGLE (LIST 'PP_LINE B__ A__) (LIST 'PP_LINE B__ C__))) 
(PUT 'P9_CENTER 'NUMBER-OF-ARGS 3) 
(FLAG '(P9_CENTER) 'OPFN) 
(PUT 'P9_CENTER 'DEFINED-ON-LINE '386) 
(PUT 'P9_CENTER 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'P9_CENTER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE P9_CENTER (A__ B__ C__) (LIST 'CIRCLE_CENTER (LIST 'P9_CIRCLE A__ B__ C__))) 
(PUT 'P9_CIRCLE 'NUMBER-OF-ARGS 3) 
(FLAG '(P9_CIRCLE) 'OPFN) 
(PUT 'P9_CIRCLE 'DEFINED-ON-LINE '389) 
(PUT 'P9_CIRCLE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'P9_CIRCLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE P9_CIRCLE (A__ B__ C__)
    (LIST 'P3_CIRCLE (LIST 'MIDPOINT A__ B__) (LIST 'MIDPOINT A__ C__)
          (LIST 'MIDPOINT B__ C__))) 
(PUT 'P_BISECTOR 'NUMBER-OF-ARGS 2) 
(FLAG '(P_BISECTOR) 'OPFN) 
(PUT 'P_BISECTOR 'DEFINED-ON-LINE '392) 
(PUT 'P_BISECTOR 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'P_BISECTOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE P_BISECTOR (B__ C__)
    (LIST 'ORTHO_LINE (LIST 'MIDPOINT B__ C__) (LIST 'PP_LINE B__ C__))) 
(PUT 'PAPPUS_LINE 'NUMBER-OF-ARGS 6) 
(FLAG '(PAPPUS_LINE) 'OPFN) 
(PUT 'PAPPUS_LINE 'DEFINED-ON-LINE '395) 
(PUT 'PAPPUS_LINE 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'PAPPUS_LINE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PAPPUS_LINE (A__ B__ C__ D__ E__ F__)
    (LIST 'PP_LINE
          (LIST 'INTERSECTION_POINT (LIST 'PP_LINE A__ E__)
                (LIST 'PP_LINE B__ D__))
          (LIST 'INTERSECTION_POINT (LIST 'PP_LINE A__ F__)
                (LIST 'PP_LINE C__ D__)))) 
(PUT 'PEDALPOINT 'NUMBER-OF-ARGS 2) 
(FLAG '(PEDALPOINT) 'OPFN) 
(PUT 'PEDALPOINT 'DEFINED-ON-LINE '399) 
(PUT 'PEDALPOINT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'PEDALPOINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PEDALPOINT (P__ A_) (LIST 'INTERSECTION_POINT (LIST 'ORTHO_LINE P__ A_) A_)) 
(PUT 'SQRDIST_PL 'NUMBER-OF-ARGS 2) 
(FLAG '(SQRDIST_PL) 'OPFN) 
(PUT 'SQRDIST_PL 'DEFINED-ON-LINE '402) 
(PUT 'SQRDIST_PL 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'SQRDIST_PL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SQRDIST_PL (A__ L_) (LIST 'SQRDIST A__ (LIST 'PEDALPOINT A__ L_))) 
(PUT 'SYM_POINT 'NUMBER-OF-ARGS 2) 
(FLAG '(SYM_POINT) 'OPFN) 
(PUT 'SYM_POINT 'DEFINED-ON-LINE '405) 
(PUT 'SYM_POINT 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'SYM_POINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SYM_POINT (P__ L_) (LIST 'FIXEDPOINT P__ (LIST 'PEDALPOINT P__ L_) 2)) 
(PUT 'TRIANGLE_AREA 'NUMBER-OF-ARGS 3) 
(FLAG '(TRIANGLE_AREA) 'OPFN) 
(PUT 'TRIANGLE_AREA 'DEFINED-ON-LINE '408) 
(PUT 'TRIANGLE_AREA 'DEFINED-IN-FILE 'GEOMETRY/GEOPROVER.RED) 
(PUT 'TRIANGLE_AREA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRIANGLE_AREA (A__ B__ C__)
    (LIST 'TIMES (LIST 'QUOTIENT 1 2) (LIST 'IS_COLLINEAR A__ B__ C__))) 
(ENDMODULE) 