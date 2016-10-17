#|(in-package :cl-user)
(defpackage kazami.scanner
  (:use :cl)
  (:export :token 
           :make-token
           :token-key
           :token-value
           :defrule
           :scan
           ))
(in-package :kazami.scanner)|#

(defstruct token
  (key :any :type keyword)
  (value "" :type string))

(defmacro defrule (key rule)
  "Defines rule to tokenize.
   This is a syntactic sugar to clarify semantics."
  `(defparameter ,key ,rule))

(defun |nonascii| (c)
  (<= 1113974 (char-code c) 1114111)) 

(defun |unicode| (c)
  (not (typep c 'standard-char))) 

(defrule <ident>
  '((? #\-) |nmstart| (* |nmchar|) (eop :ident)))

(defun |nmstart| (c)
  (let ((code (char-code c)))
    (or (= code 95)
        (<= 65 code 90)
        (<= 97 code 122)
        (|nonascii| c)
        (|unicode| c))))

(defun |nmchar| (c)
  (let ((code (char-code c)))
    (or (<= #.(char-code #\0) code #.(char-code #\9))
        (<= #.(char-code #\a) code #.(char-code #\z))
        (<= #.(char-code #\A) code #.(char-code #\Z))
        (|nonascii| c)
        (|unicode| c))))

(defun umatch (u c)
  "Matches the unit expression.
   Returns a boolean."
  (if (symbolp u)
    (funcall u c)
    (eq u c)))

(defmacro uqmatch (uq c)
  "Internal syntax to filter a quantifier or an unit.
   Returns a boolean and a control keyword optional."
  `(if (listp ,uq)
    (qmatch ,uq ,c)
    (umatch ,uq ,c))) 

(defun qmatch (q c)
  "Matches the quantifier expression.
   Returns a boolean and a control keyword."
  (let ((quantifier (first q)))
    (case quantifier
      (* (if (uqmatch (second q) c)
            (values t :not-elimination)
            (values nil :match-next)))
      (+ (if (uqmatch (second q) c)
            (values t :trans-asterisk)
            (values nil :remove-pattern)))
      (? (if (uqmatch (second q) c)
            t
            (values nil :match-next)))
      (or (if (dolist (u (rest q))
                 (when (umatch u c)
                   (return t)))
             t
             (values nil :remove-pattern)))
      (eop (values (second q) :end-of-pattern)))))

(defun pmatch (p c)
  "Matches the pattern expression.
   Returns the rest pattern." 
    (multiple-value-bind (result action)
      (uqmatch (first p) c)
      (case action
        (:not-elimination p)
        (:match-next (pmatch (rest p) c))
        (:trans-asterisk (progn
                           (setf (caar p) '*)
                           p))
        (:remove-pattern nil)
        (:end-of-pattern (values nil result))
        ((nil) (if result
                 (rest p)
                 nil))))   
   )

(defun validate (plist c)
  "Attempts to match the all expressions.
   Returns the rest pattern and a matched token-key."
  (let ((prest (list))
        (matched nil))
    (dolist (p plist)
      (multiple-value-bind (rp m)
        (pmatch p c)
        (if (null m)
          (if (and (listp (car rp)) (eq (caar rp) 'eop))
            (setf matched (cadar rp))
            (unless (null rp)
              (push rp prest)))
          (setf matched m))))
    (values prest matched)))

(defun check-satisfied (p)
  (when (listp (car p))
    (let ((q (caar p)))
      (if (and (or (eq q '*)
                   (eq q '?))
               (listp (second p))
               (eq (caadr p) 'eop))
        (cadadr p)))))

(defun scan (src patterns start)
  "Scans and attempts to tokenize from start position of src.
   Returns a token."
  (let ((src-length (length src)))
    (do ((pos start (1+ pos))
         (last-matched :any)
         (last-pos start))
      ((null patterns) (make-token :key last-matched
                                   :value (subseq src start last-pos)))
      ;(format t "~A ~A~%" pos patterns)
      (when (>= pos src-length)
        (dolist (p patterns)
          (let ((k (check-satisfied p)))
            (unless (null k)
              (setf last-matched k
                    last-pos (1- pos)))))
        (return (make-token :key last-matched
                            :value (subseq src start (1+ last-pos)))))
      (multiple-value-bind (rest-pattern matched)
        (validate patterns (char src pos))
        (unless (null matched)
          (setf last-matched matched
                last-pos pos)
                )
        (setf patterns rest-pattern)))))
