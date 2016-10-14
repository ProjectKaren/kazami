#|
  This file is a part of kazami project.
  Copyright (c) 2016 Tamamu
|#

#|
  Author: Tamamu
|#

(in-package :cl-user)
(defpackage kazami-asd
  (:use :cl :asdf))
(in-package :kazami-asd)

(defsystem kazami
  :version "0.1"
  :author "Tamamu"
  :license "LLGPL"
  :depends-on (:cl-annot :cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "kazami"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op kazami-test))))
