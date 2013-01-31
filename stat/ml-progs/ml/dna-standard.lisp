;;; -*- Mode:Common-Lisp; Package:USER; Base:10 -*-

(setf *FEATURE-NAMES*
  '(P-50 P-49 P-48 P-47 P-46 P-45 P-44 P-43 P-42 P-41 P-40 P-39 P-38 P-37 P-36 P-35 P-34 P-33 P-32
  P-31 P-30 P-29 P-28 P-27 P-26 P-25 P-24 P-23 P-22 P-21 P-20 P-19 P-18 P-17 P-16 P-15 P-14 P-13
  P-12 P-11 P-10 P-9 P-8 P-7 P-6 P-5 P-4 P-3 P-2 P-1 P1 P2 P3 P4 P5 P6 P7))

(setf *DOMAINS*
  '((A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C)
  (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C)
  (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C)
  (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C)
  (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C)
  (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C) (A G T C)
  (A G T C) (A G T C) (A G T C)))

(setf *CATEGORIES*
  '(PROMOTER NEGATIVE))

(setf *THEORY*
  '((<- (PROMOTER) (CONTACT) (CONFORMATION)) (<- (CONTACT) (MINUS_35) (MINUS_10))
  (<- (MINUS_35) (P-36 T) (P-35 T) (P-34 G) (P-33 A) (P-32 C))
  (<- (MINUS_35) (P-36 T) (P-35 T) (P-34 G) (P-32 C) (P-31 A))
  (<- (MINUS_10) (P-14 T) (P-13 A) (P-12 T) (P-11 A) (P-10 A) (P-9 T))
  (<- (MINUS_10) (P-13 T) (P-12 A) (P-10 A) (P-8 T)) (<- (MINUS_10) (P-12 T) (P-11 A) (P-7 T))
  (<- (CONFORMATION) (P-47 C) (P-46 A) (P-45 A) (P-43 T) (P-42 T) (P-40 A) (P-39 C) (P-22 G)
   (P-18 T) (P-16 C) (P-8 G) (P-7 C) (P-6 G) (P-5 C) (P-4 C) (P-2 C) (P-1 C))
  (<- (CONFORMATION) (P-45 A) (P-44 A) (P-41 A))
  (<- (CONFORMATION) (P-49 A) (P-44 T) (P-27 T) (P-22 A) (P-18 T) (P-16 T) (P-15 G) (P-1 A))
  (<- (CONFORMATION) (P-45 A) (P-41 A) (P-28 T) (P-27 T) (P-23 T) (P-21 A) (P-20 A) (P-17 T)
   (P-15 T) (P-4 T))))

(setf *raw-examples* '(
 (PROMOTER
  #(T A C T A G C A A T A C G C T T G C G T T C G G T G G T T A A G T A T G T A
    T A A T G C G C G G G C T T G T C G T))
 (PROMOTER
  #(T G C T A T C C T G A C A G T T G T C A C G C T G A T T G G T G T C G T T A
    C A A T C T A A C G C A T C G C C A A))
 (PROMOTER
  #(G T A C T A G A G A A C T A G T G C A T T A G C T T A T T T T T T T G T T A
    T C A T G C T A A C C A C C C G G C G))
 (PROMOTER
  #(A A T T G T G A T G T G T A T C G A A G T G T G T T G C G G A G T A G A T G
    T T A G A A T A C T A A C A A A C T C))
 (PROMOTER
  #(T C G A T A A T T A A C T A T T G A C G A A A A G C T G A A A A C C A C T A
    G A A T G C G C C T C C G T G G T A G))
 (PROMOTER
  #(A G G G G C A A G G A G G A T G G A A A G A G G T T G C C G T A T A A A G A
    A A C T A G A G T C C G T T T A G G T))
 (PROMOTER
  #(C A G G G G G T G G A G G A T T T A A G C C A T C T C C T G A T G A C G C A
    T A G T C A G C C C A T C A T G A A T))
 (PROMOTER
  #(T T T C T A C A A A A C A C T T G A T A C T G T A T G A G C A T A C A G T A
    T A A T T G C T T C A A C A G A A C A))
 (PROMOTER
  #(C G A C T T A A T A T A C T G C G A C A G G A C G T C C G T T C T G T G T A
    A A T C G C A A T G A A A T G G T T T))
 (PROMOTER
  #(T T T T A A A T T T C C T C T T G T C A G G C C G G A A T A A C T C C C T A
    T A A T G C G C C A C C A C T G A C A))
 (PROMOTER
  #(G C A A A A A T A A A T G C T T G A C T C T G T A G C G G G A A G G C G T A
    T T A T G C A C A C C C C G C G C C G))
 (PROMOTER
  #(C C T G A A A T T C A G G G T T G A C T C T G A A A G A G G A A A G C G T A
    A T A T A C G C C A C C T C G C G A C))
 (PROMOTER
  #(G A T C A A A A A A A T A C T T G T G C A A A A A A T T G G G A T C C C T A
    T A A T G C G C C T C C G T T G A G A))
 (PROMOTER
  #(C T G C A A T T T T T C T A T T G C G G C C T G C G G A G A A C T C C C T A
    T A A T G C G C C T C C A T C G A C A))
 (PROMOTER
  #(T T T A T A T T T T T C G C T T G T C A G G C C G G A A T A A C T C C C T A
    T A A T G C G C C A C C A C T G A C A))
 (PROMOTER
  #(A A G C A A A G A A A T G C T T G A C T C T G T A G C G G G A A G G C G T A
    T T A T G C A C A C C G C C G C G C C))
 (PROMOTER
  #(A T G C A T T T T T C C G C T T G T C T T C C T G A G C C G A C T C C C T A
    T A A T G C G C C T C C A T C G A C A))
 (PROMOTER
  #(A A A C A A T T T C A G A A T A G A C A A A A A C T C T G A G T G T A A T A
    A T G T A G C C T C G T G T C T T G C))
 (PROMOTER
  #(T C T C A A C G T A A C A C T T T A C A G C G G C G C G T C A T T T G A T A
    T G A T G C G C C C C G C T T C C C G))
 (PROMOTER
  #(G C A A A T A A T C A A T G T G G A C T T T T C T G C C G T G A T T A T A G
    A C A C T T T T G T T A C G C G T T T))
 (PROMOTER
  #(G A C A C C A T C G A A T G G C G C A A A A C C T T T C G C G G T A T G G C
    A T G A T A G C G C C C G G A A G A G))
 (PROMOTER
  #(A A A A A C G T C A T C G C T T G C A T T A G A A A G G T T T C T G G C C G
    A C C T T A T A A C C A T T A A T T A))
 (PROMOTER
  #(T C T G A A A T G A G C T G T T G A C A A T T A A T C A T C G A A C T A G T
    T A A C T A G T A C G C A A G T T C A))
 (PROMOTER
  #(A C C G G A A G A A A A C C G T G A C A T T T T A A C A C G T T T G T T A C
    A A G G T A A A G G C G A C G C C G C))
 (PROMOTER
  #(A A A T T A A A A T T T T A T T G A C T T A G G T C A C T A A A T A C T T T
    A A C C A A T A T A G G C A T A G C G))
 (PROMOTER
  #(T T G T C A T A A T C G A C T T G T A A A C C A A A T T G A A A A G A T T T
    A G G T T T A C A A G T C T A C A C C))
 (PROMOTER
  #(C A T C C T C G C A C C A G T C G A C G A C G G T T T A C G C T T T A C G T
    A T A G T G G C G A C A A T T T T T T))
 (PROMOTER
  #(T C C A G T A T A A T T T G T T G G C A T A A T T A A G T A C G A C G A G T
    A A A A T T A C A T A C C T G C C C G))
 (PROMOTER
  #(A C A G T T A T C C A C T A T T C C T G T G G A T A A C C A T G T G T A T T
    A G A G T T A G A A A A C A C G A G G))
 (PROMOTER
  #(T G T G C A G T T T A T G G T T C C A A A A T C G C C T T T T G C T G T A T
    A T A C T C A C A G C A T A A C T G T))
 (PROMOTER
  #(C T G T T G T T C A G T T T T T G A G T T G T G T A T A A C C C C T C A T T
    C T G A T C C C A G C T T A T A C G G))
 (PROMOTER
  #(A T T A C A A A A A G T G C T T T C T G A A C T G A A C A A A A A A G A G T
    A A A G T T A G T C G C G T A G G G T))
 (PROMOTER
  #(A T G C G C A A C G C G G G G T G A C A A G G G C G C G C A A A C C C T C T
    A T A C T G C G C G C C G A A G C T G))
 (PROMOTER
  #(T A A A A A A C T A A C A G T T G T C A G C C T G T C C C G C T T A T A A G
    A T C A T A C G C C G T T A T A C G T))
 (PROMOTER
  #(A T G C A A T T T T T T A G T T G C A T G A A C T C G C A T G T C T C C A T
    A G A A T G C G C G C T A C T T G A T))
 (PROMOTER
  #(C C T T G A A A A A G A G G T T G A C G C T G C A A G G C T C T A T A C G C
    A T A A T G C G C C C C G C A A C G C))
 (PROMOTER
  #(T C G T T G T A T A T T T C T T G A C A C C T T T T C G G C A T C G C C C T
    A A A A T T C G G C G T C C T C A T A))
 (PROMOTER
  #(C C G T T T A T T T T T T C T A C C C A T A T C C T T G A A G C G G T G T T
    A T A A T G C C G C G C C C T C G A T))
 (PROMOTER
  #(T T C G C A T A T T T T T C T T G C A A A G T T G G G T T G A G C T G G C T
    A G A T T A G C C A G C C A A T C T T))
 (PROMOTER
  #(T G T A A A C T A A T G C C T T T A C G T G G G C G G T G A T T T T G T C T
    A C A A T C T T A C C C C C A C G T A))
 (PROMOTER
  #(G A T C G C A C G A T C T G T A T A C T T A T T T G A G T A A A T T A A C C
    C A C G A T C C C A G C C A T T C T T))
 (PROMOTER
  #(A A C G C A T A C G G T A T T T T A C C T T C C C A G T C A A G A A A A C T
    T A T C T T A T T C C C A C T T T T C))
 (PROMOTER
  #(T T A G C G G A T C C T A C C T G A C G C T T T T T A T C G C A A C T C T C
    T A C T G T T T C T C C A T A C C C G))
 (PROMOTER
  #(G C C T T C T C C A A A A C G T G T T T T T T G T T G T T A A T T C G G T G
    T A G A C T T G T A A A C C T A A A T))
 (PROMOTER
  #(C A G A A A C G T T T T A T T C G A A C A T C G A T C T C G T C T T G T G T
    T A G A A T T C T A A C A T A C G G T))
 (PROMOTER
  #(C A C T A A T T T A T T C C A T G T C A C A C T T T T C G C A T C T T T G T
    T A T G C T A T G G T T A T T T C A T))
 (PROMOTER
  #(A T A T A A A A A A G T T C T T G C T T T C T A A C G T G A A A G T G G T T
    T A G G T T A A A A G A C A T C A G T))
 (PROMOTER
  #(C A A G G T A G A A T G C T T T G C C T T G T C G G C C T G A T T A A T G G
    C A C G A T A G T C G C A T C G G A T))
 (PROMOTER
  #(G G C C A A A A A A T A T C T T G T A C T A T T T A C A A A A C C T A T G G
    T A A C T C T T T A G G C A T T C C T))
 (PROMOTER
  #(T A G G C A C C C C A G G C T T T A C A C T T T A T G C T T C C G G C T C G
    T A T G T T G T G T G G A A T T G T G))
 (PROMOTER
  #(C C A T C A A A A A A A T A T T C T C A A C A T A A A A A A C T T T G T G T
    A A T A C T T G T A A C G C T A C A T))
 (PROMOTER
  #(T G G G G A C G T C G T T A C T G A T C C G C A C G T T T A T G A T A T G C
    T A T C G T A C T C T T T A G C G A G))
 (PROMOTER
  #(T C A G A A A T A T T A T G G T G A T G A A C T G T T T T T T T A T C C A G
    T A T A A T T T G T T G G C A T A A T))
 (NEGATIVE
  #(A T A T G A A C G T T G A G A C T G C C G C T G A G T T A T C A G C T G T G
    A A C G A C A T T C T G G C G T C T A))
 (NEGATIVE
  #(C G A A C G A G T C A A T C A G A C C G C T T T G A C T C T G G T A T T A C
    T G T G A A C A T T A T T C G T C T C))
 (NEGATIVE
  #(C A A T G G C C T C T A A A C G G G T C T T G A G G G G T T T T T T G C T G
    A A A G G A G G A A C T A T A T G C G))
 (NEGATIVE
  #(T T G A C C T A C T A C G C C A G C A T T T T G G C G G T G T A A G C T A A
    C C A T T C C G G T T G A C T C A A T))
 (NEGATIVE
  #(C G T C T A T C G G T G A A C C T C C G G T A T C A A C G C T G G A A G G T
    G A C G C T A A C G C A G A T G C A G))
 (NEGATIVE
  #(G C C A A T C A A T C A A G A A C T T G A A G G G T G G T A T C A G C C A A
    C A G C C T G A C A T C C T T C G T T))
 (NEGATIVE
  #(T G G A T G G A C G T T C A A C A T T G A G G A A G G C A T A A C G C T A C
    T A C C T G A T G T T T A C T C C A A))
 (NEGATIVE
  #(G A G G T G G C T A T G T G T A T G A C C G A A C G A G T C A A T C A G A C
    C G C T T T G A C T C T G G T A T T A))
 (NEGATIVE
  #(C G T A G C G C A T C A G T G C T T T C T T A C T G T G A G T A C G C A C C
    A G C G C C A G A G G A C G A C G A C))
 (NEGATIVE
  #(C G A C C G A A G C G A G C C T C G T C C T C A A T G G C C T C T A A A C G
    G G T C T T G A G G G G T T T T T T G))
 (NEGATIVE
  #(C T A C G G T G G G T A C A A T A T G C T G G A T G G A G A T G C G T T C A
    C T T C T G G T C T A C T G A C T C G))
 (NEGATIVE
  #(A T A G T C T C A G A G T C T T G A C C T A C T A C G C C A G C A T T T T G
    G C G G T G T A A G C T A A C C A T T))
 (NEGATIVE
  #(A A C T C A A G G C T G A T A C G G C G A G A C T T G C G A G C C T T G T C
    C T T G C G G T A C A C A G C A G C G))
 (NEGATIVE
  #(T T A C T G T G A A C A T T A T T C G T C T C C G C G A C T A C G A T G A G
    A T G C C T G A G T G C T T C C G T T))
 (NEGATIVE
  #(T A T T C T C A A C A A G A T T A A C C G A C A G A T T C A A T C T C G T G
    G A T G G A C G T T C A A C A T T G A))
 (NEGATIVE
  #(A A C G A G T C A A T C A G A C C G C T T T G A C T C T G G T A T T A C T G
    T G A A C A T T A T T C G T C T C C G))
 (NEGATIVE
  #(A A G T G C T T A G C T T C A A G G T C A C G G A T A C G A C C G A A G C G
    A G C C T C G T C C T C A A T G G C C))
 (NEGATIVE
  #(G A A G A C C A C G C C T C G C C A C C G A G T A G A C C C T T A G A G A G
    C A T G T C A G C C T C G A C A A C T))
 (NEGATIVE
  #(T T A G A G A G C A T G T C A G C C T C G A C A A C T T G C A T A A A T G C
    T T T C T T G T A G A C G T G C C C T))
 (NEGATIVE
  #(T A T T C G T C T C C G C G A C T A C G A T G A G A T G C C T G A G T G C T
    T C C G T T A C T G G A T T G T C A C))
 (NEGATIVE
  #(T G C T G A A A G G A G G A A C T A T A T G C G C T C A T A C G A T A T G A
    A C G T T G A G A C T G C C G C T G A))
 (NEGATIVE
  #(C A T G A A C T C A A G G C T G A T A C G G C G A G A C T T G C G A G C C T
    T G T C C T T G C G G T A C A C A G C))
 (NEGATIVE
  #(T T C G T C T C C G C G A C T A C G A T G A G A T G C C T G A G T G C T T C
    C G T T A C T G G A T T G T C A C C A))
 (NEGATIVE
  #(C A T G T C A G C C T C G A C A A C T T G C A T A A A T G C T T T C T T G T
    A G A C G T G C C C T A C G C G C T T))
 (NEGATIVE
  #(A G G A G G A A C T A C G C A A G G T T G G A A C A T C G G A G A G A T G C
    C A G C C A G C G C A C C T G C A C G))
 (NEGATIVE
  #(T C T C A A C A A G A T T A A C C G A C A G A T T C A A T C T C G T G G A T
    G G A C G T T C A A C A T T G A G G A))
 (NEGATIVE
  #(T G A A G T G C T T A G C T T C A A G G T C A C G G A T A C G A C C G A A G
    C G A G C C T C G T C C T C A A T G G))
 (NEGATIVE
  #(C T A T A T G C G C T C A T A C G A T A T G A A C G T T G A G A C T G C C G
    C T G A G T T A T C A G C T G T G A A))
 (NEGATIVE
  #(G C G G C A G C A C G T T T C C A C G C G G T G A G A G C C T C A G G A T T
    C A T G T C G A T G T C T T C C G G T))
 (NEGATIVE
  #(A T C C C T A A T G T C T A C T T C C G G T C A A T C C A T C T A C G T T A
    A C C G A G G T G G C T A T G T G T A))
 (NEGATIVE
  #(T G G C G T C T A T C G G T G A A C C T C C G G T A T C A A C G C T G G A A
    G G T G A C G C T A A C G C A G A T G))
 (NEGATIVE
  #(T C T C G T G G A T G G A C G T T C A A C A T T G A G G A A G G C A T A A C
    G C T A C T A C C T G A T G T T T A C))
 (NEGATIVE
  #(T A T T G G C T T G C T C A A G C A T G A A C T C A A G G C T G A T A C G G
    C G A G A C T T G C G A G C C T T G T))
 (NEGATIVE
  #(T A G A G G G T G T A C T C C A A G A A G A G G A A G A T G A G G C T A G A
    C G T C T C T G C A T G G A G T A T G))
 (NEGATIVE
  #(C A G C G G C A G C A C G T T T C C A C G C G G T G A G A G C C T C A G G A
    T T C A T G T C G A T G T C T T C C G))
 (NEGATIVE
  #(T T A C G T T G G C G A C C G C T A G G A C T T T C T T G T T G A T T T T C
    C A T G C G G T G T T T T G C G C A A))
 (NEGATIVE
  #(A C G C T A A C G C A G A T G C A G C G A A C G C T C G G C G T A T T C T C
    A A C A A G A T T A A C C G A C A G A))
 (NEGATIVE
  #(G G T G T T T T G C G C A A T G T T A A T C G C T T T G T A C A C C T C A G
    G C A T G T A A A C G T C T T C G T A))
 (NEGATIVE
  #(A A C C A T T C C G G T T G A C T C A A T G A G C A T C T C G A T G C A G C
    G T A C T C C T A C A T G A A T A G A))
 (NEGATIVE
  #(A G A C G T C T C T G C A T G G A G T A T G A G A T G G A C T A C G G T G G
    G T A C A A T A T G C T G G A T G G A))
 (NEGATIVE
  #(T G T T G A T T T T C C A T G C G G T G T T T T G C G C A A T G T T A A T C
    G C T T T G T A C A C C T C A G G C A))
 (NEGATIVE
  #(T G C A C G G G T T G C G A T A G C C T C A G C G T A T T C A G G T G C G A
    G T T C G A T A G T C T C A G A G T C))
 (NEGATIVE
  #(A G G C A T G T A A A C G T C T T C G T A G C G C A T C A G T G C T T T C T
    T A C T G T G A G T A C G C A C C A G))
 (NEGATIVE
  #(C C G A G T A G A C C C T T A G A G A G C A T G T C A G C C T C G A C A A C
    T T G C A T A A A T G C T T T C T T G))
 (NEGATIVE
  #(C G C T A G G A C T T T C T T G T T G A T T T T C C A T G C G G T G T T T T
    G C G C A A T G T T A A T C G C T T T))
 (NEGATIVE
  #(T A T G A C C G A A C G A G T C A A T C A G A C C G C T T T G A C T C T G G
    T A T T A C T G T G A A C A T T A T T))
 (NEGATIVE
  #(A G A G G G T G T A C T C C A A G A A G A G G A A G A T G A G G C T A G A C
    G T C T C T G C A T G G A G T A T G A))
 (NEGATIVE
  #(G A G A G C A T G T C A G C C T C G A C A A C T T G C A T A A A T G C T T T
    C T T G T A G A C G T G C C C T A C G))
 (NEGATIVE
  #(C C T C A A T G G C C T C T A A A C G G G T C T T G A G G G G T T T T T T G
    C T G A A A G G A G G A A C T A T A T))
 (NEGATIVE
  #(G T A T T C T C A A C A A G A T T A A C C G A C A G A T T C A A T C T C G T
    G G A T G G A C G T T C A A C A T T G))
 (NEGATIVE
  #(C G C G A C T A C G A T G A G A T G C C T G A G T G C T T C C G T T A C T G
    G A T T G T C A C C A A G G C T T C C))
 (NEGATIVE
  #(C T C G T C C T C A A T G G C C T C T A A A C G G G T C T T G A G G G G T T
    T T T T G C T G A A A G G A G G A A C))
 (NEGATIVE
  #(T A A C A T T A A T A A A T A A G G A G G C T C T A A T G G C A C T C A T T
    A G C C A A T C A A T C A A G A A C T))
 ))
