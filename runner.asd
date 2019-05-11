;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:runner-asd
  (:use :cl :asdf))

(in-package :runner-asd)

(asdf:defsystem "runner"
  :name "runner"
  :version "0.0.1"
  :author "Maris Orbidans"
  :licence "Public Domain"
  :serial t
  :components ((:module "src"
		:serial t
		:components ((:file "runner"))))
  :in-order-to ((test-op (test-op "runner/tests"))))

(asdf:defsystem "runner/tests"
  :licence "Public Domain"
  :depends-on (:runner
	       :alexandria
	       :check-it
	       :fiasco)
  :serial t
  :components ((:module "tests"
		:components ((:file "runner-tests"))))
  :perform (test-op (o c) (uiop:symbol-call 'fiasco 'all-tests)))
