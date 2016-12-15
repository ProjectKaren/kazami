(in-package :cl-user)
(defpackage kazami-test
  (:use :cl
        :kazami
        :prove))
(in-package :kazami-test)

;; NOTE: To run this test file, execute `(asdf:test-system :kazami)' in your Lisp.

(defmacro ok-token (src target len)
  `(destructuring-bind (token length) (kazami:lex ,src 0)
    (ok (and (eq token ,target)
             (= length ,len)))))

(plan nil)


(ok-token "-ident-test" <ident> 11)
(ok-token "@atkeyword" <atkeyword> 9)
(ok-token "\"string1\"" <string> 9)
(ok-token "'string2'" <string> 9)
(ok-token "\"bad1" <badstring> 5)
(ok-token "'bad2" <badstring> 5)
(ok-token "url(\"string1\"" <bad-uri> 13)
(ok-token "url('string2'" <bad-uri> 13)
(ok-token "url(http://url.com!#$%&*-~)" <bad-uri> 27)
(ok-token "url(\"bad1" <bad-uri> 9)
(ok-token "url('bad2" <bad-uri> 9)


;; blah blah blah.

(finalize)
