;; Copyright (c) 2004 Marijn Haverbeke
;;
;; This software is provided 'as-is', without any express or implied
;; warranty. In no event will the authors be held liable for any
;; damages arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any
;; purpose, including commercial applications, and to alter it and
;; redistribute it freely, subject to the following restrictions:
;;
;; 1. The origin of this software must not be misrepresented; you must
;;    not claim that you wrote the original software. If you use this
;;    software in a product, an acknowledgment in the product
;;    documentation would be appreciated but is not required.
;;
;; 2. Altered source versions must be plainly marked as such, and must
;;    not be misrepresented as being the original software.
;;
;; 3. This notice may not be removed or altered from any source
;;    distribution.
;;
;; Marijn Haverbeke
;; marijn(at)haverbeke.nl

; Init file for Unlikely Scheme

; By default this is called at the start of every session, it
; initializes functions and syntax. Not everything in here is very
; neat or efficient yet.

; Standard macros. Mostly just stolen from the 5th Revised Report.

(define-syntax let
  (syntax-rules ()
    ((_ ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...) val ...))
    ((_ tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...) body1 body2 ...))) tag)
      val ...))))

(define-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ test) test)
    ((_ test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ test) test)
    ((_ test1 test2 ...)
     (let ((or-temp-var test1))
       (if or-temp-var or-temp-var (or test2 ...))))))

(define-syntax cond
  (syntax-rules (else =>)
    ((_ (else result1 result2 ...))
     (begin result1 result2 ...))
    ((_ (test => result))
     (let ((temp test))
	       (if temp (result temp))))
    ((_ (test => result) clause1 clause2 ...)
     (let ((temp test))
	       (if temp
			(result temp)
			(cond clause1 clause2 ...))))
    ((_ (test)) test)
    ((_ (test) clause1 clause2 ...)
     (let ((temp test))
	       (if temp
			temp
			(cond clause1 clause2 ...))))
    ((_ (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((_ (test result1 result2 ...)
	clause1 clause2 ...)
     (if test
	      (begin result1 result2 ...)
	      (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else)
    ((_ (key ...) clauses ...)
     (let ((atom-key (key ...)))
	       (case atom-key clauses ...)))
    ((_ key (else result1 result2 ...))
     (begin result1 result2 ...))
    ((_ key ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
	      (begin result1 result2 ...)))
    ((_ key ((atoms ...) result1 result2 ...)
	clause clauses ...)
     (if (memv key '(atoms ...))
	      (begin result1 result2 ...)
	      (case key clause clauses ...)))))

(define-syntax let*
  (syntax-rules ()
    ((_ () body1 body2 ...)
     ((lambda () body1 body2 ...)))
    ((_ ((name1 val1) (name2 val2) ...)
	body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
	 body1 body2 ...)))))

(define-syntax letrec
  (syntax-rules ()
    ((_ ((var1 init1) ...) body ...)
     (letrec #\G (var1 ...) () ((var1 init1) ...) body ...))
    ((_ #\G () (temp1 ...) ((var1 init1) ...) body ...)
     (let ((var1 #v) ...)
       (let ((temp1 init1) ...)
         (set! var1 temp1)
         ...
	 (begin
	   body ...))))
    ((_ #\G (x y ...) (temp ...) ((var1 init1) ...) body ...)
     (letrec #\G (y ...) (newtemp temp ...) ((var1 init1) ...) body ...))))

(define-syntax do
  (syntax-rules ()
    ((_ ((var init step ...) ...)
	(test expr ...)
	command ...)
     (letrec
      ((loop
	(lambda (var ...)
	  (if test
	    (begin #v expr ...)
	      (begin command ...
		(loop (do #\S var step ...) ...))))))
      (loop init ...)))
    ((_ #\S x) x)
    ((_ #\S x y) y)))

; From here stuff has to be initialized to report_env instead of null_env
'goto-report-env

; Check to see whether init was already loaded (the error handling and
; dynamic-wind goes haywire if it is loaded twice)
(if (defined? '*init-loaded* (impl:current-env))
    (raise "init file already loaded"))

; Apply is implemented using the #%as_arguments instruction. It is
; possible to write closures using instructions. The error checking on
; instructions is minimal though, stuff is likely to crash if you make
; a mistake.
(define (apply function . args)
  (define (paste-args list)
    (if (null? (cdr list))
	(car list)
	(cons (car list) (paste-args (cdr list)))))
  (define simple-apply (make-closure #(#%deref_env (0 . 1) #%as_arguments #%deref_env (0 . 0) #%tail) 2))
  (simple-apply function (paste-args args)))

; impl:current-env is a special form that compiles to the interaction
; environment.
(define (interaction-environment)
  (impl:current-env))

(define (eval expression environment)
  ((compile expression environment 'top-level)))

; Dynamic-wind system taken from http://www.cs.hmc.edu/~fleck/envision/scheme48/meeting/node7.html

(define call-with-current-continuation #f)
(define dynamic-wind #f)

((lambda ()
  (define *wind-state* (cons #f '()))
  
  (define (wind-reroot! there)
    (if (not (eq? *wind-state* there))
	(begin 
	  (wind-reroot! (cdr there))
	  (let ((before (caar there)) (after (cdar there)))
	    (set-car! *wind-state* (cons after before))
	    (set-cdr! *wind-state* there)
	    (set-car! there #f)
	    (set-cdr! there '())
	    (set! *wind-state* there) 
	    (before)))))

  (define simple-call-with-current-continuation
    (make-closure 
     #(#%grow_env 2 #%current_continuation #%set_env (0 . 1) #%setup_arg_list 1 #%literal #(#(#%deref_env (1 . 1) #%set_continuation 
       #%deref_env (0 . 0) #%return) 1 continuation-wrapper) #%finish_lambda #%add_arg 0 #%deref_env (0 . 0) #%tail) 1))
  
  (define (local-call-with-current-continuation proc)
    (let ((here *wind-state*))
      (simple-call-with-current-continuation (lambda (cont) 
					       (proc (lambda (result)
						       (wind-reroot! here)
						       (cont result)))))))
  (define (local-dynamic-wind before during after)
    (let ((here *wind-state*))
      (wind-reroot! (cons (cons before after) here))
      (let ((result (during)))
	(wind-reroot! here)
	result)))

  (set! call-with-current-continuation local-call-with-current-continuation)
  (set! dynamic-wind local-dynamic-wind)))

(define call/cc call-with-current-continuation)

; This is called when a quasiquoted expression is evaluated.
(define (impl:fill-in-quasiquoted expression . values)
  (let loop ((expression expression) (depth 0))
    (cond ((pair? expression)
	   (cond ((eq? (car expression) 'unquote)
		  (if (= depth 0)
		      (let ((temp (car values)))
			(set! values (cdr values))
			temp)
		      (list 'unquote (loop (cadr expression) (- depth 1)))))
		 ((eq? (car expression) 'quasiquote)
		  (list 'quasiquote (loop (cadr expression) (+ depth 1))))
		 ((and (pair? (car expression)) (eq? (caar expression) 'unquote-splicing))
		  (if (= depth 0)
		      (let ((temp (car values)))
			(set! values (cdr values))
			(append temp (loop (cdr expression) depth)))
		      (cons (list 'unquote-splicing (loop (cadar expression) (- depth 1))) (loop (cdr expression) depth))))
		 (else (cons (loop (car expression) depth) (loop (cdr expression) depth)))))
	  ((vector? expression)
	   (list->vector (loop (vector->list expression) depth)))
	  (else expression))))

; Promises

(define (make-promise proc)
  (let ((result-ready? #f) (result #f))
    (lambda ()
      (if result-ready?
	  result
	  (let ((x (proc)))
	    (if result-ready?
		result
		(begin (set! result-ready? #t)
		       (set! result x)
		       result)))))))

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (make-promise (lambda () expression)))))

(define (force object)
  (object))

; Numbers

(define (max x . nums)
  (let loop ((current x) (lst nums) (exact #t))
    (cond ((null? lst)
	   (if exact current (exact->inexact current)))
	  ((_> current (car lst))
	   (loop current (cdr lst) (and exact (exact? (car lst)))))
	  (else
	   (loop (car lst) (cdr lst) (and exact (exact? current)))))))
(define (min x . nums)
  (let loop ((current x) (lst nums) (exact #t))
    (cond ((null? lst) (if exact current (exact->inexact current)))
	  ((_< current (car lst))
	   (loop current (cdr lst) (and exact (exact? (car lst)))))
	  (else
	   (loop (car lst) (cdr lst) (and exact (exact? current)))))))
(define (= one two . lst)
  (cond ((null? lst) (_= one two))
	((_= one two) (apply = two lst))
	(else #f)))
(define (< one two . lst)
  (cond ((null? lst) (_< one two))
	((_< one two) (apply < two lst))
	(else #f)))
(define (> one two . lst)
  (cond ((null? lst) (_> one two))
	((_> one two) (apply > two lst))
	(else #f)))
(define (<= one two . lst)
  (cond ((null? lst) (_<= one two))
	((_<= one two) (apply <= two lst))
	(else #f)))
(define (>= one two . lst)
  (cond ((null? lst) (_>= one two))
	((_>= one two) (apply >= two lst))
	(else #f)))

(define (+ . lst)
  (if (null? lst)
      0
      (let add-loop ((cur (car lst)) (rest (cdr lst)))
	(if (null? rest)
	    cur
	    (add-loop (_+ cur (car rest)) (cdr rest))))))
(define (* . lst)
  (if (null? lst)
      1
      (let mult-loop ((cur (car lst)) (rest (cdr lst)))
	(if (null? rest)
	    cur
	    (mult-loop (_* cur (car rest)) (cdr rest))))))
(define (- first . lst)
  (if (null? lst)
      (_- 0 first)
      (let minus-loop ((cur first) (rest lst))
	(if (null? rest)
	    cur
	    (minus-loop (_- cur (car rest)) (cdr rest))))))
(define (/ first . lst)
  (if (null? lst)
      (_/ 1 first)
      (let div-loop ((cur first) (rest lst))
	(if (null? rest)
	    cur
	    (div-loop (_/ cur (car rest)) (cdr rest))))))

(define (zero? num)
  (_= 0 num))
(define (positive? num)
  (_> num 0))
(define (negative? num)
  (_< num 0))
(define (odd? num)
  (_= (modulo num 2) 1))
(define (even? num)
  (_= (modulo num 2) 0))

(define (abs num)
  (if (negative? num)
      (- num)
      num))

(define (gcd . num)
  (define (internal-gcd a b)
    (if (_= a 0)
	b
	(gcd (remainder b a) a)))
  (cond
   ((null? num) 0)
   ((null? (cdr num)) (car num))
   (else
    (let ((a (car num)) (b (apply gcd (cdr num))))
      (internal-gcd (min (abs a) (abs b)) (max (abs a) (abs b)))))))

(define (lcm . num)
  (define (internal-lcm a b)
    (* (quotient a (gcd a b)) b))
  (cond
   ((null? num) 1)
   ((null? (cdr num)) (abs (car num)))
   (else (internal-lcm (abs (car num)) (apply lcm (cdr num))))))

; Booleans

(define (not x) (if x #f #t))
(define (boolean? x) (or (eq? x #t) (eq? x #f)))

; Pairs

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (list . x) x)
(define (length x) (if (null? x) 0 (+ 1 (length (cdr x)))))

(define (append . x)
  (define (add-to-list new-part old-part)
    (if (null? new-part)
	old-part
	(add-to-list (cdr new-part) (cons (car new-part) old-part))))
  (let loop ((remain x) (result '()))
    (cond ((null? remain) '())
	  ((null? (cdr remain))
	   (if (null? result)
	       (car remain)
	       (let ((reversed (reverse-in-place result)))
		 (set-cdr! result (car remain))
		 reversed)))
	  (else (loop (cdr remain) (add-to-list (car remain) result))))))

(define (reverse lst)
  (define (reverse-helper lst result)
    (if (null? lst)
	result
	(reverse-helper (cdr lst) (cons (car lst) result))))
  (reverse-helper lst '()))

(define (reverse-in-place lst)
  (let loop ((remain lst) (result '()))
    (if (null? remain)
	result
	(let ((new-remain (cdr remain)))
	  (set-cdr! remain result)
	  (loop new-remain remain)))))

(define (list-tail lst n)
  (if (_= 0 n)
      lst
      (list-tail (cdr lst) (- n 1))))

(define (list-ref lst n)
  (car (list-tail lst n)))

(define (memq value lst)
  (cond ((null? lst) #f)
	((eq? value (car lst)) lst)
	(else (memq value (cdr lst)))))
(define memv memq)

(define (member value lst)
  (cond ((null? lst) #f)
	((equal? value (car lst)) lst)
	(else (member value (cdr lst)))))

(define (assq value lst)
  (cond ((null? lst) #f)
	((eq? value (caar lst)) (car lst))
	(else (assq value (cdr lst)))))
(define assv assq)

(define (assoc value lst)
  (cond ((null? lst) #f)
	((equal? value (caar lst)) (car lst))
	(else (assoc value (cdr lst)))))

; Characters

(define (char=? one two . lst)
  (cond ((null? lst) (char_=? one two))
	((char_=? one two) (apply char=? two list))
	(else #f)))
(define (char<? one two . lst)
  (cond ((null? lst) (char_<? one two))
	((char_<? one two) (apply char<? two list))
	(else #f)))
(define (char>? one two . lst)
  (cond ((null? lst) (char_>? one two))
	((char_>? one two) (apply char>? two list))
	(else #f)))
(define (char<=? one two . lst)
  (cond ((null? lst) (char_<=? one two))
	((char_<=? one two) (apply char<=? two list))
	(else #f)))
(define (char>=? one two . lst)
  (cond ((null? lst) (char_>=? one two))
	((char_>=? one two) (apply char>=? two list))
	(else #f)))

(define (char-ci=? char1 char2 . lst) (apply char=? (char-downcase char1) (char-downcase char2) (map char-downcase lst)))
(define (char-ci<? char1 char2 . lst) (apply char<? (char-downcase char1) (char-downcase char2) (map char-downcase lst)))
(define (char-ci>? char1 char2 . lst) (apply char>? (char-downcase char1) (char-downcase char2) (map char-downcase lst)))
(define (char-ci<=? char1 char2 . lst) (apply char<=? (char-downcase char1) (char-downcase char2) (map char-downcase lst)))
(define (char-ci>=? char1 char2 . lst) (apply char>=? (char-downcase char1) (char-downcase char2) (map char-downcase lst)))

(define (char-upper-case? char) (and (char-alphabetic? char) (eq? char (char-upcase char))))
(define (char-lower-case? char) (and (char-alphabetic? char) (eq? char (char-downcase char))))

; Strings

(define (string . chars)
  (define size (length chars))
  (define str (make-string size))
  (let loop ((n 0) (list chars))
    (cond ((null? list) str)
	  (else (string-set! str n (car list))
		(loop (+ n 1) (cdr list))))))

(define (generic-string-compare-equal str1 str2 predicate)
  (define length (string-length str1))
  (define (compare-strings n)
    (cond ((_= n length) #t)
	  ((predicate (string-ref str1 n) (string-ref str2 n))
	   (compare-strings (+ n 1)))
	  (else #f)))
  (and (_= length (string-length str2)) (compare-strings 0)))

(define (string=? str1 str2) (generic-string-compare-equal str1 str2 eq?))
(define (string-ci=? str1 str2) (generic-string-compare-equal str1 str2 char-ci=?))

(define (generic-string-compare-diff str1 str2 char-eq char-diff length-pred)
  (define length-1 (string-length str1))
  (define length-2 (string-length str2))
  (define length-min (min length-1 length-2))
  (define (compare-strings n)
    (cond ((_= n length-min) (length-pred length-1 length-2))
	  ((char-eq (string-ref str1 n) (string-ref str2 n)) (compare-strings (+ n 1)))
	  (else (char-diff (string-ref str1 n) (string-ref str2 n)))))
  (compare-strings 0))

(define (string<? str1 str2) (generic-string-compare-diff str1 str2 char_=? char_<? <))
(define (string>? str1 str2) (generic-string-compare-diff str1 str2 char_=? char_>? >))
(define (string<=? str1 str2) (generic-string-compare-diff str1 str2 char_=? char_<=? <=))
(define (string>=? str1 str2) (generic-string-compare-diff str1 str2 char_=? char_>=? >=))

(define (string-ci<? str1 str2) (generic-string-compare-diff str1 str2 char-ci=? char-ci<? <))
(define (string-ci>? str1 str2) (generic-string-compare-diff str1 str2 char-ci=? char-ci>? >))
(define (string-ci<=? str1 str2) (generic-string-compare-diff str1 str2 char-ci=? char-ci<=? <=))
(define (string-ci>=? str1 str2) (generic-string-compare-diff str1 str2 char-ci=? char-ci>=? >=))

(define (substring str a b)
  (define new-str (make-string (- b a)))
  (define (copy n)
    (cond ((_= n b) new-str)
	  (else (string-set! new-str (- n a) (string-ref str n))
		(copy (+ n 1)))))
  (copy a))

(define (string-append . args)
  (if (null? args)
      ""
      (let loop ((str (car args)) (lst (cdr args)))
	(if (null? lst)
	    str
	    (begin
	      (define length-1 (string-length str))
	      (define length-2 (string-length (car lst)))
	      (define total-length (+ length-1 length-2))
	      (define new-str (make-string total-length))
	      (let loop ((n 0))
		(cond ((_= n length-1) #t)
		      (else (string-set! new-str n (string-ref str n))
			    (loop (+ n 1)))))
	      (let loop ((n length-1))
		(cond ((_= n total-length) #t)
		      (else (string-set! new-str n (string-ref (car lst) (- n length-1)))
			    (loop (+ n 1)))))
	      (apply string-append new-str (cdr lst)))))))

(define (string->list str)
  (define length (string-length str))
  (let loop ((n 0))
    (if (_= n length)
	'()
	(cons (string-ref str n) (loop (+ n 1))))))

(define (list->string lst)
  (apply string lst))

(define (string-copy str)
  (substring str 0 (string-length str)))

(define (string-fill! str value)
  (define size (string-length str))
  (let loop ((n 0))
    (cond ((_= n size) #v)
	  (else (string-set! str n value)
		(loop (+ n 1))))))

; Vectors

(define (vector . lst)
  (define size (length lst))
  (define vec (make-vector size))
  (let loop ((n 0) (list lst))
    (cond ((null? list) vec)
	  (else (vector-set! vec n (car list))
		(loop (+ n 1) (cdr list))))))

(define (vector=? vec1 vec2)
  (define (compare-vectors n)
    (cond ((_< n 0) #t)
	  ((equal? (vector-ref vec1 n) (vector-ref vec2 n))
	   (compare-vectors (- n 1)))
	  (else #f)))
  (and (_= (vector-length vec1) (vector-length vec2))
       (compare-vectors (- (vector-length vec1) 1))))

(define (vector->list vec)
  (define length (vector-length vec))
  (let loop ((n 0))
    (if (_= n length)
	'()
	(cons (vector-ref vec n) (loop (+ n 1))))))

(define (list->vector lst)
  (apply vector lst))

(define (vector-fill! vec value)
  (define size (vector-length vec))
  (let loop ((n 0))
    (cond ((_= n size) #v)
	  (else (vector-set! vec n value)
		(loop (+ n 1))))))

; Misc

(define (_map proc list)
  (if (null? list)
      '()
      (cons (proc (car list)) (_map proc (cdr list)))))

(define (map proc . lists)
  (if (null? (car lists))
      '()
      (let ()
	(define cars (_map car lists))
	(define cdrs (_map cdr lists))
	(cons (apply proc cars) (apply map proc cdrs)))))

(define (for-each proc . lists)
  (if (null? (car lists))
      #v
      (let ()
	(define cars (_map car lists))
	(define cdrs (_map cdr lists))
	(apply proc cars)
	(apply for-each proc cdrs))))

(define (filter predicate list)
  (cond ((null? list) '())
	((predicate (car list)) (cons (car list) (filter predicate (cdr list))))
	(else (filter predicate (cdr list)))))      

(define (accumulate operation value list)
  (if (null? list)
      value
      (accumulate operation (operation value (car list)) (cdr list))))

; multiple return values

(define values #f)
(define call-with-values #f)
(let ((unique ""))
  (set! values (lambda vals
		 (if (and (not (null? vals)) (null? (cdr vals)))
		     (car vals)
		     (cons unique vals))))
  (set! call-with-values (lambda (producer receiver)
			   (define returned (producer))
			   (if (and (pair? returned) (eq? (car returned) unique))
			       (apply receiver (cdr returned))
			       (receiver returned)))))

; I/O

; When called on a closed file reopen-...put-file will reopen the file
; and restore the position to what it was before it got closed.
(define (call-with-input-file string proc)
  (let ((file (open-input-file string)))
    (dynamic-wind
	(lambda () (reopen-input-file file))
	(lambda () (proc file))
	(lambda () (close-input-port file)))))
(define (call-with-output-file string proc)
  (let ((file (open-output-file string)))
    (dynamic-wind
	(lambda () (reopen-output-file file))
	(lambda () (proc file))
	(lambda () (close-output-port file)))))

; Interaction

; The error handling is not great yet - because of proper tail
; recursion and the unnamed-ness of most closures the stack traces are
; almost worthless. But the system is flexible and can be used to do
; most kinds of error handling. When something goes wrong the virtual
; machine is reset and impl:handle-error is called, which calls the
; top error handler on the impl:error handler stack. You can use
; continuations to return to some state after an error, if you don't
; the scheme process will finish after the handler returns.
; Use push-error-handler and pop-error-handler to change the current
; error handler. If show-instructions is called with a non-false value
; the instruction vector of the current function will be shown when an
; error occurs.
(define (print-error message stack)
  (define (translate-function-name name)
    (if name
	name
	"unnamed function"))
  (if (not (null? stack))
      (begin
	(display "In function:")(newline)
	(let loop ((lst stack))
	  (if (not (null? lst))
	      (begin
		(display "  ")(display (translate-function-name (car lst)))(newline)
		(loop (cdr lst)))))))
  (display "Error: ")(display message)(newline))
(define impl:*error-handler*
  (list print-error))
(define impl:show-instructions #f)
(define (show-instructions option)
  (set! impl:show-instructions option))
(define (impl:handle-error message stack cur-instruction instructions)
  (if impl:show-instructions
      (begin
	(display "In code:")(newline)
	(let loop ((n 0))
	  (if (_< n (vector-length instructions))
	      (begin
		(if (_= n cur-instruction)
		    (display "* ")
		    (display "  "))
		(display n)(display " ")(display (vector-ref instructions n))(newline)
		(loop (+ n 1)))))))
  ((car impl:*error-handler*) message stack))

(define (push-error-handler handler)
  (set! impl:*error-handler* (cons handler impl:*error-handler*)))
(define (pop-error-handler)
  (if (null? (cdr impl:*error-handler*))
      (raise "can not pop off last error handler")
      (set! impl:*error-handler* (cdr impl:*error-handler*))))
(define (current-error-handler)
  (car impl:*error-handler*))

; Load a file.
(define (_load filename environment)
  (define file (open-input-file filename))
  (dynamic-wind
      (lambda ()
	(reopen-input-file file)
	(let ((old-handler (current-error-handler)))
	  (push-error-handler
	   (lambda (message stack)
	     (display "In file '")(display filename)(display "' near line ")
	     (display (input-port-line file))(display ":")(newline)
	     (old-handler message stack)))))
      (lambda ()
	(let loop ((expr (read file)))
	  (if (not (eof-object? expr))
	      (begin
		(eval expr environment)
		(loop (read file))))))
      (lambda ()
	(pop-error-handler)
	(close-input-port file))))

; Read-eval-print-loop
(define (run-repl environment)
  (define continuation #f)
  (define (local-handler message stack)
    (print-error message (reverse (cddr (reverse stack))))
    (continuation #f))
  (define (try thunk)
    (dynamic-wind
	(lambda () (push-error-handler local-handler))
	thunk
	(lambda () (pop-error-handler))))
  (call/cc (lambda (c) (set! continuation c)))
  (let loop ()
    (display "> ")
    (let ((val (try read)))
      (if (not (eof-object? val))
	  (let ((result (try (lambda () (eval val environment)))))
	    (if (not (eq? result #v))
		(begin
		  (write result)
		  (newline)))
	    (loop))))))

(define *init-loaded* #t)
