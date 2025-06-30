(require 'calc-arith)

(defun canonicalize (ts)
  (letrec ((clock-time (mod ts 43200))
	   (h-angle (* 360 (/ clock-time 43200.0)))
	   (m-angle (* 360 (/ (mod ts 3600) 3600.0)))
	   (s-angle (* 360 (/ (mod ts 60) 60.0)))
	   (absolute-angles (sort (list h-angle m-angle s-angle) '>)))
	   (list
	    (- (nth 0 absolute-angles) (nth 1 absolute-angles))
	    (- (nth 1 absolute-angles) (nth 2 absolute-angles))
	    clock-time)))



(canonicalize (time-convert nil 'integer))

(defun ticks-apply (a b func)
  (funcall func (+ (car a) (cadr a)) (+ (car b) (cadr b))))

(defun ticks-compare (a b)
  (ticks-apply a b '<))

(defun ticks-equal (a b)
  (and (equal (car a) (car b))
       (equal (cadr a) (cadr b))))

(setq faketicks
'(
  (1.1 2.2 234)
  (1.1 2.3 4)
  (1.1 2.2 4590)
  (1.1 2.3 98443)
  (4.4 5.5 323)
 ))

(ticks-eq (nth 0 faketicks) (nth 2 faketicks))
(cadr '(1.1 2.2 234))
(sort faketicks 'ticks-compare)

(setq ticks
      (sort
       (mapcar 'canonicalize (number-sequence 0 43199))
       'ticks-compare))

;(seq-reduce ticks (lambda (acc x) (if (null acc)
;				      (cons x nil)
;				    (if (ticks-equal (car acc) x)
;					(let ((t car
