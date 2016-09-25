#|
  This file is a part of kazami project.
  Copyright (c) 2016 Tamamu
|#

(in-package :cl-user)
(defpackage kazami-test-asd
  (:use :cl :asdf))
(in-package :kazami-test-asd)

(defsystem kazami-test
  :author "Tamamu"
  :license "LLGPL"
  :depends-on (:kazami
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "kazami"))))
  :description "Test system for kazami"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
