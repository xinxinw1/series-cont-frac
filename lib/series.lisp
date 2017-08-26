(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind 
              (result exists)
            (gethash args cache)
          (if exists
              result
              (setf (gethash args cache)
                    (apply fn args)))))))

(defun fact-k (n k)
  (if (<= n 1) 1
      (* n (fact-k (- n k) k))))

(setf (fdefinition 'fact-k) (memoize #'fact-k))

(defun fact (n)
  (fact-k n 1))

; rising factorial, (poch x n) = x(x+1)...(x+n-1)  (n terms)
(defun poch (x n)
  (if (<= n 0) 1
      (* x (poch (+ x 1) (- n 1)))))

; falling factorial, (fact-fall x n) = x(x-1)...(x-(n-1)) (n terms)
(defun fact-fall (x n)
	(if (<= n 0) 1
      (* x (fact-fall (- x 1) (- n 1)))))

(setf (fdefinition 'fact-fall) (memoize #'fact-fall))

(defun bin (n k)
	(/ (fact-fall n k) (fact k)))

(setf (fdefinition 'bin) (memoize #'bin))

; Bernoulli numbers
(defun Bn (n)
	(- 1 (loop for k from 0 to (- n 1) sum (/ (* (bin n k) (Bn k)) (+ (- n k) 1)))))

(setf (fdefinition 'Bn) (memoize #'Bn))

; (-1)^n
(defun sign-pow (n)
  (if (oddp n) -1 1))

; Euler numbers
(defun En (n)
  (if (oddp n) 0
      (* #C(0 1)
         (loop for k from 1 to (+ n 1) sum
           (loop for j from 0 to k sum
             (/ (* (bin k j) (sign-pow j) (expt (- k (* 2 j)) (+ n 1)))
                (* (expt 2 k) (expt #C(0 1) k) k)))))))

; alternating permutations
(defun An (n)
  (cond ((eq n 0) 1)
        ((eq n 1) 1)
        (t (/ (loop for k from 0 to (- n 1) sum
                (* (bin (- n 1) k) (An k) (An (- n 1 k))))
              2))))

(setf (fdefinition 'An) (memoize #'An))

(defun Bell (n)
  (if (eq n 0) 1
      (loop for k from 0 to (- n 1) sum
        (* (bin (- n 1) k) (Bell k)))))

(setf (fdefinition 'Bell) (memoize #'Bell))

; ordered Bell numbers
(defun aBell (n)
  (if (eq n 0) 1
      (loop for i from 1 to n sum
        (* (bin n i) (aBell (- n i))))))

(setf (fdefinition 'aBell) (memoize #'aBell))

; arrangement numbers
(defun a (n)
  (if (eq n 0) 1
      (+ (* n (a (- n 1))) 1)))

(setf (fdefinition 'a) (memoize #'a))

(defun deran (n)
  (if (eq n 0) 1
      (+ (* n (deran (- n 1))) (sign-pow n))))

(setf (fdefinition 'deran) (memoize #'deran))

; Harmonic numbers
(defun Hn (n)
  (loop for k from 1 to n sum (/ k)))

; Telephone numbers
(defun Tn (n)
  (cond ((eq n 0) 1)
        ((eq n 1) 1)
        ((+ (Tn (- n 1)) (* (- n 1) (Tn (- n 2)))))))

(setf (fdefinition 'Tn) (memoize #'Tn))

; Sylvester's sequence
(defun sn (n)
  (if (eq n 0) 2
      (+ (* (sn (- n 1)) (- (sn (- n 1)) 1)) 1)))

(setf (fdefinition 'sn) (memoize #'sn))

; partition function
(defun p (n)
  (cond ((< n 0) 0)
        ((eq n 0) 1)
        (t (flet ((g (k) (/ (* k (- (* 3 k) 1)) 2)))
             (loop for k from 1 until (< (- n (g k)) 0)
               sum (* (sign-pow (- k 1)) (+ (p (- n (g k))) (p (- n (g (- k)))))))))))

(setf (fdefinition 'p) (memoize #'p))

; fibonacci
(defun F (n &optional (F0 0) (F1 1))
  (cond ((eq n 0) F0)
        ((eq n 1) F1)
        (t (+ (F (- n 1) F0 F1) (F (- n 2) F0 F1)))))

(setf (fdefinition 'F) (memoize #'F))

; Pell numbers
(defun Pn (n)
  (cond ((eq n 0) 0)
        ((eq n 1) 1)
        (t (+ (* 2 (Pn (- n 1))) (Pn (- n 2))))))

(setf (fdefinition 'Pn) (memoize #'Pn))

; Motzkin number
(defun Mn (n)
  (cond ((eq n 0) 1)
        ((eq n 1) 1)
        (t (+ (* (/ (+ (* 2 n) 1) (+ n 2)) (Mn (- n 1)))
              (* (/ (- (* 3 n) 3) (+ n 2)) (Mn (- n 2)))))))

(setf (fdefinition 'Mn) (memoize #'Mn))

; Gregory coefficients
(defun Gn (n)
  (if (eq n 1) 1/2
      (* (sign-pow (- n 1))
         (+ (/ (+ n 1))
            (loop for k from 1 to (- n 1) sum (/ (* (sign-pow k) (Gn k)) (+ (- n k) 1)))))))

(setf (fdefinition 'Gn) (memoize #'Gn))

; https://oeis.org/A000994
; Exp gen fn is a solution of y'' = e^x*y
(defun A000994 (n &optional (n0 1) (n1 0))
  (cond ((eq n 0) n0)
        ((eq n 1) n1)
        (t (loop for k from 0 to (- n 2) sum (* (bin (- n 2) k) (A000994 k n0 n1))))))

(setf (fdefinition 'A000994) (memoize #'A000994))

; sum of powers
; https://en.wikipedia.org/wiki/Bernoulli_number#Sum_of_powers
(defun Sm (m n)
	(/ (loop for k from 0 to m sum (* (bin (+ m 1) k) (Bn k) (expt n (+ (- m k) 1)))) (+ m 1)))

; https://en.wikipedia.org/wiki/Schr%C3%B6der%E2%80%93Hipparchus_number
; Schroder-Hipparchus number
(defun xn (n k)
  (if (eq n 0) 1
      (loop for i from 1 to n sum (* (/ n) (bin n i) (bin n (- i 1)) (expt k (- i 1))))))
	
(defun conv (f g)
  (lambda (n) (loop for i from 0 to n sum (* (funcall f i) (funcall g (- n i))))))

(defun sum (f)
  (conv f (lambda (n) (progn n 1))))

(defun get-first-unmarked-after (p arr)
  (let ((start (if p p 2)))
    (loop for i from start below (length arr) do
      (if (elt arr i) (return-from get-first-unmarked-after i)))))

(defun mark-all-multiples (p arr)
  (loop for i from p below (length arr) by p do
    (setf (elt arr i) nil)))

; indexes 2 to n -> 0 to n-2, len n-1
(defun prime-sieve (n)
  (let ((primes nil) (arr (make-array n :initial-element t)))
    (loop
      (let ((p (get-first-unmarked-after (car primes) arr)))
        (if (not p) (return-from prime-sieve (nreverse primes))
            (progn (push p primes)
                   (mark-all-multiples p arr)))))))

(defparameter *primes* (prime-sieve 1000))

(defun count-divisors (n)
  (let ((arr (make-array (+ n 1) :initial-element 0)))
    (loop for i from 1 to n do
      (loop for j from 1 to n do
        (let ((num (* i j)))
          (if (<= num n) (incf (elt arr num))))))
    arr))

(defparameter *divisors* (count-divisors 1000))

;; Random number functions

; this is to deal with the random state being saved in the executable
; making the generated "random" numbers being the same every time the executable is run
(defparameter *random-state-init* nil)
(defparameter *fresh-random-state* nil)

(defun random-num (max)
  (when (not *random-state-init*)
    (setq *fresh-random-state* (make-random-state t))
    (setq *random-state-init* t))
  (random max *fresh-random-state*))

; generates a random number in [min, max)
(defun random-range (min max)
  (+ min (random-num (- max min))))

(defun random-size (s)
  (random-range (expt 10 (- s 1)) (expt 10 s)))

(defun make-b (c)
  (let ((cm (memoize c)))
    (defun h (m k)
      (case m (1 (funcall cm (+ k 1)))
              (2 (/ (funcall cm (+ k 2)) (funcall cm 1)))
              (t (- (/ (h (- m 1) (+ k 1)) (h (- m 1) 0)) (/ (h (- m 2) (+ k 1)) (h (- m 2) 0))))))

    (setf (fdefinition 'h) (memoize #'h))

    (defun b (m)
      (if (eql m 1) (h m 0)
          (- (h m 0))))
  
    #'b))

(defmacro fn (&rest rest)
  `(lambda ,@rest))

(defmacro show (expr end)
  `(loop for n from 1 to ,end collect (progn n ,expr)))

(defun gen-fn (c end)
  (let ((prev 1) (b (make-b c)))
    (loop for n from 1 to end until (eq prev 0) collect
      (setf prev (funcall b n)))))

; use progn to suppress style-warning if n is not used
(defmacro gen (expr end)
  `(gen-fn (fn (n) (progn n ,expr)) ,end))
