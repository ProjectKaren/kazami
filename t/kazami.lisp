(in-package :cl-user)
(defpackage kazami-test
  (:use :cl
        :kazami
        :prove))
(in-package :kazami-test)

;; NOTE: To run this test file, execute `(asdf:test-system :kazami)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
