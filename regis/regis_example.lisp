(defvar regis-ex1 (concatenate 'string
"S(E)W(I(M))P[600,200]V[][-200,+200]V[][400,100]W(I(G))P[700,100]"
"V(B)[+050,][,+050][-050,](E)V(W(S1))(B)[-100,][,-050][+100,](E)V(W(S1,E))"
"(B)[-050,][,-025][+050,](E)W(I(C))P[200,100]C(A-180)[+100]C(A+180)[+050]"
"W(I(B))P[200,300]C(W(S1))[+100]C(W(S1,E))[+050]W(I(W))T(S02)\"hello world\""))

(defvar regis-ex2 
"S(E)(C1)P[100,440]V(B),[+100,+0],[+0,-10],[-100,+0],(E)P[500,300],F(C[+100])")

(defun rbeg () (format t "~C~C~C~C" #\Esc #\P #\1 #\p))
(defun rend () (format t "~C~C" #\Esc #\\))

(defun ex1 ()
  (progn (rbeg)(format t "~A" regis-ex1)(rend)))

(defun ex2 ()
  (progn (rbeg)(format t "~A" regis-ex2)(rend)))