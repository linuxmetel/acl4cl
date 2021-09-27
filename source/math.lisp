(defpackage acl4cl/math
  (:use :cl))

(in-package :acl4cl/math)


(defun pow-mod (x n m)
  "Return x^n mod m"
  (assert (and (<= 0 n) (<= 1 m)))
  (when (= m 1)
    (return-from pow-mod 0))
  (loop :with r = 1
        :with y = (mod x m)
        :until (zerop n)
        :unless (zerop (logand n 1))
          :do (setq r (mod (* r y) m))
        :do (setq y (mod (* y y) m))
            (setq n (ash n -1))
        :finally (return r)))


(defun %mod-inverse (integer modulus)
  (declare (optimize (speed 3) (safety 0))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (macrolet ((frob (stype)
               `(let ((a integer)
                      (b modulus)
                      (u 1)
                      (v 0))
                  (declare (,stype a b u v))
                  (loop until (zerop b)
                        for quot = (floor a b)
                        do (decf a (the ,stype (* quot b)))
                           (rotatef a b)
                           (decf u (the ,stype (* quot v)))
                           (rotatef u v))
                  (if (< u 0)
                      (+ u modulus)
                      u))))
    (typecase modulus
      ((unsigned-byte 31) (frob (signed-byte 32)))
      ((unsigned-byte 62) (frob (signed-byte 63)))
      (otherwise (frob integer)))))

(declaim (inline mod-inverse))
(defun inv-mod (integer modulus)
  "an integer y such that 0 ≤ y < m and x * y ≡ 1 (mod m)"
  (let* ((integer (mod integer modulus))
         (result (%mod-inverse integer modulus)))
    (unless (or (= 1 (mod (* integer result) modulus)) (= 1 modulus))
      (error 'division-by-zero
             :operands (list integer modulus)
             :operation 'mod-inverse))
    result))
