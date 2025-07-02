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
(defun hmstos (h m s) (mod (+ (* 3600 h) (* 60 m) s) 43200))
(defun tstohms (ts) (list
		     (calcFunc-idiv (mod ts 43200) 3600)
		     (calcFunc-idiv (mod ts 3600) 60)
		     (mod ts 60)))

(defun rotate-until (func seq)
  (if (not (funcall func seq))
      (rotate-until func (append (cdr seq) (cons (car seq) '())))
    seq))

(defun normalize (ts)
  (letrec ((clock-time (mod ts 43200))
	   (h-angle (* 360 (/ clock-time 43200.0)))
	   (m-angle (* 360 (/ (mod ts 3600) 3600.0)))
	   (s-angle (* 360 (/ (mod ts 60) 60.0)))
	   (sorted (sort (list h-angle m-angle s-angle) '<))
	   (a (- (nth 1 sorted) (nth 0 sorted)))
	   (b (- (nth 2 sorted) (nth 1 sorted)))
	   (c (- 360 (+ a b)))
	   (low (seq-reduce 'min (list a b c) 360))
	   (normalized (rotate-until (lambda (xs) (equal (car xs) low)) (list a b c))))
    (list normalized (tstohms ts))))

(normalize (hmstos 1 30 0))

(mapcar 'normalize (list (hmstos 1 30 0) (hmstos 7 30 0) (hmstos 3 0 0) (hmstos 9 0 0)))

(defun group (xs equality-predicate)
  (seq-reduce (lambda (acc x)
		(if (funcall equality-predicate x (caar acc))
		    (cons (cons x (car acc)) (cdr acc))
		  (cons (cons x nil) acc))) xs '()))

(setq ticks (sort
	     (mapcar 'normalize (number-sequence 0 41199))
	     (lambda (a b) (version-list-< (car a) (car b)))))

(setq result (seq-filter (lambda (n) (> (length n) 1))
			 (group ticks (lambda (a b) (equal (car a) (car b))))))

(length result)
