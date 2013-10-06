(ql:quickload "html-template")
(ql:quickload "cl-who")
(ql:quickload "hunchentoot")

(defpackage :webserver
  (:use :common-lisp :hunchentoot :cl-who))

(in-package :webserver)

(setf *dispatch-table*
      (list #'dispatch-easy-handlers))
(setf *show-lisp-errors-p* t
       *show-lisp-backtraces-p* t)

(load "kmapper.lisp")
;;(setf hunchentoot:*catch-errors-p* nil) 

(define-easy-handler (easy-demop :uri "/"
				 :default-request-type :both)
    ((truthtableinput :request-type :post :init-form *truth-table*) (numinputs :request-type :post))

  ;;set numinputs and truthtableinput to correct values for post and get scenarios
  (let* ((numinputs (if numinputs (parse-integer numinputs) 4)) 
	 (truthtableinput (if truthtableinput (escape-string truthtableinput) *truth-table*)))
  (with-html-output-to-string (*standard-output*) (getKMaps truthtableinput numinputs))))


(defvar *macceptor* (make-instance 'hunchentoot:easy-acceptor :port 8080 
                                   :document-root #p"/home/vancan1ty/Desktop/lisp/kmapweb/"))
(hunchentoot:start *macceptor*)

(defun getKMaps (truthtable numinputs)
  (let* ((kmapoutput (html-create-k-maps truthtable numinputs))
	 (generated-html (list :content (if (eql (length kmapoutput) 0) "Invalid truth table input!" kmapoutput)))
	 (dbgout (list :content truthtable))
	 (html-template:*string-modifier* #'identity))
    (html-template:fill-and-print-template #p"./index.tmpl" generated-html :stream *standard-output*)))

(defmacro break-transparent (exp)
  `(let ((x ,exp)) (break "argument to break: ~:S" x) x))
