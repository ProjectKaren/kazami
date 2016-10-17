;;;;
;;;; A CSS Parser
;;;; CSS3
;;;;

(in-package :cl-user)
(defpackage kazami
  (:use :cl)
  (:import-from :kazami.token
    :+css-token-pattern+
  )
  (:import-from :cl-ppcre
    :scan
    :parse-string
    :create-scanner
    ))
(in-package :kazami)

(cl-annot:enable-annot-syntax)
(setf cl-ppcre:*use-bmh-matchers* t)
(setf cl-ppcre:*optimize-char-classes* :charset)

(defparameter +h+ "[0-9a-f]")
(defparameter +num+ "[+-]?([0-9]+|[0-9]*\.[0-9]+)(e[+-]?[0-9]+)?")
(defparameter +nonascii+ "[^\\0-\\177]")
(defparameter +unicode+ "\\\\[0-9a-f]{1,6}(\\r\\n|[ \\n\\r\\t\\f])?")
(defparameter +escape+
  (format nil "(~A)|\\\\[^\\r\\n\\f0-9a-f]" +unicode+))
(defparameter +nmstart+
  (format nil "[_a-z]|(~A)|(~A)" +nonascii+ +escape+))
(defparameter +nmchar+
  (format nil "[_a-z0-9-]|(~A)|(~A)" +nonascii+ +escape+))
(defparameter +ident+
  (format nil "[-]?(~A)(~A)*" +nmstart+ +nmchar+))
(defparameter +name+
  (format nil "(~A)+" +nmchar+))
(defparameter +nl+ "\\n|\\r\\n|\\r|\\f")
(defparameter +string1+
  (format nil "\\\"([^\\n\\r\\f\\\\\"]|\\\\(~A)|(~A))*\\\"" +nl+ +escape+))
(defparameter +string2+
  (format nil "\\'([^\\n\\r\\f\\\\']|\\\\(~A)|(~A))*\\'" +nl+ +escape+))
(defparameter +string+
  (format nil "(~A)|(~A)" +string1+ +string2+))
(defparameter +badstring1+
  (format nil "\\\"([^\\n\\r\\f\\\\\"]|\\\\(~A)|(~A))*\\\\?" +nl+ +escape+))
(defparameter +badstring2+
  (format nil "\'([^\\n\\r\\f\\\\']|\\\\(~A)|(~A))*\\\\?" +nl+ +escape+))
(defparameter +badstring+
  (format nil "(~A)|(~A)" +badstring1+ +badstring2+))
(defparameter +badcomment1+ "\\/\\*[^*]*\\*+([^/*][^*]*\\*+)*")
(defparameter +badcomment2+ "\\/\\*[^*]*(\\*+[^/*][^*]*)*")
(defparameter +badcomment+
  (format nil "(~A)|(~A)" +badcomment1+ +badcomment2+))
(defparameter +U+ "u|\\\\0{0,4}(55|75)(\\r\\n|[ \\t\\r\\n\\f])?|\\\\u")
(defparameter +R+ "r|\\\\0{0,4}(52|72)(\\r\\n|[ \\t\\r\\n\\f])?|\\\\r")
(defparameter +L+ "l|\\\\0{0,4}(4c|6c)(\\r\\n|[ \\t\\r\\n\\f])?|\\\\l")
(defparameter +w+ "[ \\t\\r\\n\\f]*")
(defparameter +baduri1+
  (format nil "(~A)(~A)(~A)\\((~A)([!#$%&*-~~]|(~A)|(~A))*(~A)"
    +U+ +R+ +L+ +w+ +nonascii+ +escape+ +w+))
(defparameter +baduri2+
  (format nil "(~A)(~A)(~A)\\((~A)(~A)(~A)"
    +U+ +R+ +L+ +w+ +string+ +w+))
(defparameter +baduri3+
  (format nil "(~A)(~A)(~A)\\((~A)(~A)"
    +U+ +R+ +L+ +w+ +badstring+))
(defparameter +baduri+
  (format nil "(~A)|(~A)|(~A)" +baduri1+ +baduri2+ +baduri3+))

(defparameter <ident> +ident+)
(defparameter <atkeyword> (format nil "@(~A)" +ident+))
(defparameter <string> +string+)
(defparameter <bad-string> +badstring+)
(defparameter <bad-uri> +baduri+)
(defparameter <bad-comment> +badcomment+)
(defparameter <hash> (format nil "#(~A)" +name+))
(defparameter <number> +num+)
(defparameter <percentage> (format nil "(~A)%" +num+))
(defparameter <dimension> (format nil "(~A)(~A)" +num+ +ident+))
(defparameter <uri> (format nil
  "(~A)(~A)(~A)\\((~A)(~A)(~A)\\)|(~A)(~A)(~A)\\((~A)([!#$%&*-\\[\\]-~~]|(~A)|(~A))*(~A)\\)"
  +U+ +R+ +L+ +w+ +string+ +w+ +U+ +R+ +L+ +w+ +nonascii+ +escape+ +w+))
(defparameter <unicode-range>
  "u\\+[?]{1,6}|u\\+[0-9a-f]{1}[?]{0,5}|u\\+[0-9a-f]{2}[?]{0,4}|u\\+[0-9a-f]{3}[?]{0,3}|u\\+[0-9a-f]{4}[?]{0,2}|u\\+[0-9a-f]{5}[?]{0,1}|u\\+[0-9a-f]{6}|u\\+[0-9a-f]{1,6}-[0-9a-f]{1,6}")
(defparameter <cdo> "<!--")
(defparameter <cdc> "-->")
(defparameter <colon> ":")
(defparameter <semicolon> ";")
(defparameter <bstart> "\\{")
(defparameter <bend> "\\}")
(defparameter <pstart> "\\(")
(defparameter <pend> "\\)")
(defparameter <bbstart> "\\[")
(defparameter <bbend> "\\]")
(defparameter <s> "[ \\t\\r\\n\\f]+")
(defparameter <comment> "\\/\\*[^*]*\\*+([^/*][^*]*\\*+)*\\/")
(defparameter <function> (format nil "(~A)\\(" +ident+))
(defparameter <includes> "~=")
(defparameter <dashmatch> "\\\\|=")

(defstruct token-hash
  (key :any :type keyword)
  (pattern nil))
(defparameter +all-token-pattern+
  (let* ((patterns '(
          (:ident . <ident>)
          (:atkeyword . <atkeyword>)
          (:string . <string>)
          (:bad-string . <bad-string>)
          (:bad-url . <bad-uri>)
          (:bad-comment . <bad-comment>)
          (:hash . <hash>)
          (:number . <number>)
          (:percentage . <percentage>)
          (:dimension . <dimension>)
          (:url . <uri>)
          (:unicode-range . <unicode-range>)
          (:cdo . <cdo>)
          (:cdc . <cdc>)
          (:colon . <colon>)
          (:semicolon . <semicolon>)
          (:bstart . <bstart>)
          (:bend . <bend>)
          (:pstart . <pstart>)
          (:pend . <pend>)
          (:bbstart . <bbstart>)
          (:bbend . <bbend>)
          (:s . <s>)
          (:comment . <comment>)
          (:function . <function>)
          (:includes . <includes>)
          (:dashmatch . <dashmatch>)))
        (compiled-patterns (make-array (length patterns) :element-type 'token-hash)))
    (loop for i from 0 below (length +css-token-pattern+)
          do (setf (aref compiled-patterns i)
                   (make-token-hash :key (car (elt +css-token-pattern+ i))
                                    :pattern (create-scanner (symbol-value (cdr (elt +css-token-pattern+ i)))))))
    compiled-patterns))

(defparameter +token-pattern-length+
  (length +all-token-pattern+))

(defstruct token
  (id :any :type keyword)
  (string "" :type string))

(defmacro match-length (start end)
  `(if (or (null ,start) (> ,start 0))
     -1
     ,end))


(defvar +test+
"* {
    position: absolute;
    left: 0;
    top: 0px;}
div, section  {
    margin: 0 auto;
}")

(defun search-max-position (vector)
  (let ((max-position nil))
    (loop for i from 0 below (length vector)
          for current = (aref vector 0) then (aref vector i)
          with max = -1
          do (when (< max current)
               (setf max-position i
                     max current)))
    max-position))

(defun tokenize (str)
  @type string str
  @optimize (speed 3)
  @optimize (safety 0)
  (let ((marr (make-array +token-pattern-length+
                          :element-type 'fixnum
                          :initial-element -1)))
    (loop for i from 0 below (length marr)
          ;do (print (aref +all-token-pattern+ i 1))
          do (multiple-value-bind (start end)
               (scan (token-hash-pattern (aref +all-token-pattern+ i)) str)
               (setf (aref marr i)
                     (match-length start end))))
    (let ((max (search-max-position marr)))
      (if (null max)
        (values (make-token :string (subseq str 0 1))
                (subseq str 1))
        (values (make-token :id (token-hash-key (aref +all-token-pattern+ max))
                            :string (subseq str 0 (aref marr max)))
                (subseq str (aref marr max)))))))

(defun tokenize-all (str)
(let ((tokens (make-array 4096 :initial-element nil
                              :fill-pointer 0
                              :adjustable t)))
  (loop with source = str
        until (string= source "")
        do (multiple-value-bind (token rest-source)
             (tokenize source)
             (vector-push-extend token tokens)
             (setf source rest-source)))
  (print tokens)))
