(defsystem "runner"
  :name "runner"
  :version "0.0.1"
  :author "Maris Orbidans"
  :licence "Public Domain"
  :serial t
  :components ((:module "src"
		:serial t
		:components ((:file "runner"))))
  :in-order-to ((test-op (test-op "runner/tests"))))

(defsystem "runner/tests"
  :licence "Public Domain"
  :depends-on (:runner
	       :alexandria
	       :check-it
	       :fiasco)
  :serial t
  :components ((:module "tests"
		:components ((:file "runner-tests"))))
  :perform (test-op (o c) (symbol-call 'fiasco 'all-tests)))
