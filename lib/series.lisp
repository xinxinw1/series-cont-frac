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

(defun gen-fn (c end)
  (map 'cons (make-b c) (loop for n from 1 to end collect n)))

; use progn to suppress style-warning if n is not used
(defmacro gen (expr end)
  `(gen-fn (fn (n) (progn n ,expr)) ,end))
