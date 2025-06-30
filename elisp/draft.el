(require 'calc-arith)
(require 'cl-lib)

;; See https://www.gnu.org/software/emacs/manual/html_mono/calc.html#Internals
(setq lexical-binding t)

(defun lt (a b) (< a b))

;; Euler 2
(let ((fib (lambda (n)
	     (round
	      (-
	       (* (/ 1 (sqrt 5))
		  (expt (/ (+ 1 (sqrt 5)) 2) n))
	       (* (/ 1 (sqrt 5))
		  (expt (/ (- 1 (sqrt 5)) 2) n))))))

      (fibs-below (lambda (n x)
		    (let ((a (fib n)))
		      (if (lt a x)
			  (cons a (funcall fibs-below (+ 1 n) x))
			nil)))))
  (seq-reduce '+
	      (seq-filter 'evenp (fibs-below 1 1000099))
	    0))

;; Euler 48
(let ((m 10000000000))
  (seq-reduce (lambda (a b) (mod (+ a b) m))
	      (mapcar (lambda (a) (math-pow-mod a a m))
		    (number-sequence 1 1000))
	    0))

;; Euler 891
(letrec ((ts-to-hms (lambda (x) (cons (mod (calcFunc-idiv x 3600) 12)
				      (cons (mod (calcFunc-idiv x 60) 60)
					    (cons (mod x 60) nil)))))
	 (hms-to-angles (lambda (hms)
		       (letrec (
				(h (car hms))
				(m (cadr hms))
				(s (caddr hms))
				(h-angle (+ (* h (/ 360 12))
					    (* m (/ (/ 360 (float 12)) 60))
					    (* s (/ (/ 360 (float 12)) 3600))))
				(m-angle (+ (* m (/ 360 60))
					    (* s (/ (/ 360 (float 60)) 60))))
				(s-angle (* s (/ 360 (float 60)))))
			 (sort (cons h-angle (cons m-angle (cons s-angle nil))) 'lt))))


	 (timestamps (mapcar (lambda (n) (funcall hms-to-angles (funcall ts-to-hms n)))
			     (number-sequence 1 (* 24 3600))
			     ))

	 (sorted-timestamps (sort timestamps '(lambda (a b) (lt (seq-reduce '+ a 0) (seq-reduce '+ b 0)))))

	 (reducer (lambda (accumulator next)
		    (if (equal accumulator '()) (cons (cons next 1) nil)
		      (let ((left (caar accumulator)) (count (cdar accumulator)))
			(if (equal left next)
			    (cons (cons left (+ 1 count)) (cdr accumulator))
			  (cons (cons next 1) accumulator))))))

	 (position-counts (seq-reduce reducer sorted-timestamps nil))

	 )

  ;(funcall reducer '( ((1 2) . 1)) '(1 2))
  (seq-filter (lambda (n) (not (equal 1 (cdr n)))) position-counts)
  )

(cl-defstruct tick index)
(defun make-tick

(let ((toc (make-tick :index 2 :angles '(3 . 4))))
  (tick-index toc))



; (  ((1 2 3) . 2) (1 2 4) . 3)
(caar '(((1 2 3) . 2) (2 3 4 . 3)))

(cdr '( (1 2 3) . 4))

;'((1 2 3) (1 2 3) (1 2 3) (4 5 6) (4 5 6) (7 8 9)) '())

(let ((ts (string-to-number (shell-command-to-string "echo $(( $(date +%s) - $(date -d $(date +%Y-%m-%d) +%s) ))"))))
  ts)
