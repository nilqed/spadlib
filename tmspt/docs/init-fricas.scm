
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-fricas.scm
;; DESCRIPTION : Initialize fricas plugin
;; COPYRIGHT   : (C) 1999, 2012  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Changed So 27 Okt 2024 14:56:13 CET /kfp

;; If fricas-tm-math? is true (#t) then (if in-math?) the special serializer
;; defined below is activated and all math input will be process by the 
;; SPAD function tmMath. The variable fricas-tm-math? can be switched by:
;; command "(define fricas-tm-math? #t/#f)" when tmspt.spad is loaded.
(if (not (defined? 'fricas-tm-math?))
  (define fricas-tm-math? #f))

;; Prefix the math-input string by "tmMath", so that it may be processed
;; by a spad function (to be defined) tmMath(s:String):? instead of going
;; through directly.
(define (fricas-verbatim-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (string-append "(tmMath \"" (escape-verbatim (texmacs->code u)) "\")\n")))

;;; tmMath(s) == interpretString(s)$TemplateUtilities
;;; Use "?" as first character for debugging (the string is output without
;;; change).


;; Define fricas-serialize so that when in math-mode, the input will be
;; handled by fricas-verbatim-serialize.
(define (fricas-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (import-from (utils plugins plugin-convert))
  (with s (if (and fricas-tm-math? (in-math?))
     (fricas-verbatim-serialize lan t)
        (verbatim-serialize lan t))
        s))


;; serializer item added
(plugin-configure fricas
  (:macpath "FriCAS*" "Contents/Resources/bin")
  (:require (url-exists-in-path? "fricas"))
  (:launch "fricas -texmacs")
  (:session "FriCAS0")
  (:serializer ,fricas-serialize)
  (:scripts "FriCAS"))

;; enhanced menu added
(when (supports-fricas?)
  (import-from (fricas-kbd))
  (import-from (fricas-menus))
  (lazy-input-converter (fricas-input) fricas))

