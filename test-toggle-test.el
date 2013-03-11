(load-file "toggle-test.el")
(require 'toggle-test)

(ert-deftest should-find-best-matching-project ()
  "Should handle sub-projects and return subproject for files inside subproject"
  (setq tgt-projects '(((:root-dir "/my/proj"))
					   ((:root-dir "/my/proj/module1"))
					   ((:root-dir "/my/proj/module1/submodule"))
					   ((:root-dir "/my/proj/module2"))))
  (should (equal '((:root-dir "/my/proj")) 
				 (tgt-proj-for "/my/proj/src/foo.el")))
  (should (equal '((:root-dir "/my/proj")) 
				 (tgt-proj-for "/my/proj/module3/test/test-foo.el")))
  (should (equal '((:root-dir "/my/proj")) 
				 (tgt-proj-for "/my/proj/module1.yml")))
  (should (equal '((:root-dir "/my/proj/module2")) 
				 (tgt-proj-for "/my/proj/module2/foo.el")))
  (should (equal '((:root-dir "/my/proj/module1/submodule")) 
				 (tgt-proj-for "/my/proj/module1/submodule/src/blah")))
  (should (equal '((:root-dir "/my/proj/module1")) 
				 (tgt-proj-for "/my/proj/module1/tests/foo-test.el"))))

(defun setup-test-projects ()
  (setq tgt-projects '())
  (add-to-list 'tgt-projects 
			   '((:root-dir "/tmp/projects/python-proj") 
				 (:src-dirs "src") (:test-dirs "tests") 
				 (:test-prefixes "test_")))
  (add-to-list 'tgt-projects 
			   '((:root-dir "/tmp/projects/scala-proj") 
				 (:src-dirs "play/app" "foo-module/src") 
				 (:test-dirs "integration-specs" "play/specs" "foo-module/tests") 
				 (:test-suffixes "$Spec" "Spec" "Test" "$Test")))
  tgt-projects)


(ert-deftest should-identify-relative-file-paths ()
  (multiple-value-bind (scala-proj py-proj) (setup-test-projects)
    (should (string= "Foo.scala" 
		     (tgt-relative-file-path 
		      (file-truename "/tmp/projects/scala-proj/foo-module/src/Foo.scala") 
		      scala-proj :src-dirs)))
    (should (string= "bar/" (tgt-relative-file-path 
			     (file-truename "/tmp/projects/scala-proj/foo-module/src/bar/") 
			     scala-proj :src-dirs)))
    (should (string= "Blah.scala" (tgt-relative-file-path 
				   (file-truename "/tmp/projects/scala-proj/play/app/Blah.scala") 
				   scala-proj :src-dirs)))
    (should (string= "bar/Blah.scala" 
		     (tgt-relative-file-path 
		      (file-truename "/tmp/projects/scala-proj/foo-module/src/bar/Blah.scala") 
		      scala-proj :src-dirs)))
    (should-not (tgt-relative-file-path 
	     (file-truename "/tmp/projects/scala-proj/play/app1/Blah.scala") 
	     scala-proj :src-dirs))
    (should-not (tgt-relative-file-path 
	     (file-truename "/tmp/projects/scala-proj/foo-module/src/bar/Blah.scala") 
	     scala-proj :test-dirs))
    (should-not (tgt-relative-file-path 
	     (file-truename "/tmp/projects/python-proj/src/foo.py") 
	     scala-proj :src-dirs))
    (should (string= "src/tests_foo.py"
		     (tgt-relative-file-path 
		      (file-truename "/tmp/projects/python-proj/tests/src/tests_foo.py") 
		      py-proj :test-dirs)))
    (should (string= "controllers/" 
		     (tgt-relative-file-path 
		      (file-truename "/tmp/projects/python-proj/tests/controllers/") 
		      py-proj :test-dirs)))))

(ert-deftest should-indentify-ancestor ()
(should (tgt-is-ancestor-p "/foo" "/foo/bar"))
(should-not (tgt-is-ancestor-p "/foo/b" "/foo/bar"))
(should-not (tgt-is-ancestor-p "" "foobar"))
(should-not (tgt-is-ancestor-p 'nil "foobar"))
(should-not  (tgt-is-ancestor-p 'nil 'nil))
(should-not (tgt-is-ancestor-p "hello" 'nil))
(should-not  (tgt-is-ancestor-p "foobar" "f0")))

(ert-deftest should-identify-the-project-given-a-file ()
  (setq tgt-projects '())
  (add-to-list 'tgt-projects '((:root-dir "/foo/blah-3.3")))
  (add-to-list 'tgt-projects '((:root-dir "/foo/bar/baz/")))
  (add-to-list 'tgt-projects '((:root-dir "/blah/bar//..")))
  (add-to-list 'tgt-projects '((:root-dir "c:/foo")))
  (should (equal '((:root-dir "c:/foo")) (tgt-proj-for "c:/foo/blah/bar.el")))
  (should (equal '((:root-dir "/foo/blah-3.3")) (tgt-proj-for "/foo/blah-3.3/bar.el")))
  (should (equal '((:root-dir "/foo/blah-3.3")) (tgt-proj-for "/foo/blah-3.3/baz/bar.el")))
  (should-not  (tgt-proj-for "/foo/blah.el"))

  (should (equal '((:root-dir "/foo/bar/baz/")) (tgt-proj-for "/foo/bar/baz/foo/bar")))
  (should (equal '((:root-dir "/blah/bar//..")) (tgt-proj-for "/blah/bar/loo")))
  (should (equal '((:root-dir "/blah/bar//..")) (tgt-proj-for "/blah/bar.el")))
  ;this is not the right behaviour. not worth fix ing it
  (should (equal '((:root-dir "/blah/bar//..")) (tgt-proj-for "/blah/"))) 
  (should-not  (tgt-proj-for "/blah"))
  (should-not  (tgt-proj-for "/blah.el")))

(ert-deftest should-cross-join-lists ()
  (should (equal '((3 c) (3 b) (3 a) 
				   (2 c) (2 b) (2 a) 
				   (1 c) (1 b) (1 a)) 
				 (tgt-cross-join '(1 2 3) '(a b c))))
  (should (equal '((1  (1 1)) (1 (1 0)) (1 (0 1)) (1 (0 0)) 
				   (0 (1 1)) (0 (1 0)) (0 (0 1)) (0 (0 0)))
				 (tgt-cross-join '(0 1) (tgt-cross-join '(1 0) '(1 0)))))
  (should-not (tgt-cross-join 'nil '()))
  (should (equal '(1 2 3) (tgt-cross-join '() '(1 2 3))))
  (should (equal '(1 2 3) (tgt-cross-join '(1 2 3)  'nil)))
  (should (equal '(6 5 4 3 2) (tgt-cross-join '(1 2 3)  '(1 2 3) #'+))))

(ert-deftest should-generate-possible-src-filenames ()
  (should (equal '("foo.py")  
				 (tgt-possible-src-file-names "test_foo.py" '("test_" "spec_") 'nil)))
  (should (equal '("foo.rb")  
				 (tgt-possible-src-file-names "foo_spec.rb" '() '("_test" "_spec"))))
  (should (equal '("foo.rb") (tgt-possible-src-file-names "foo.rb" '() '())))
  (should (equal '("Blah.scala" "BlahSpec.scala" "UnitBlah.scala") 
				 (tgt-possible-src-file-names "UnitBlahSpec.scala" 
											  '("Unit" "Integration" "Functional") 
											  '("Spec" "$Spec"))))
  (should (equal '("Blah.scala") 
				 (tgt-possible-src-file-names "FunctionalBlah.scala" 
											  '("Unit" "Integration" "Functional") 
											  '("Spec" "$Spec"))))
  (should (equal '("Blah$.scala" "Blah.scala") 
				 (tgt-possible-src-file-names "Blah$Spec.scala" 
											  '("Unit" "Integration" "Functional") 
											  '("Spec" "$Spec"))))
  (should (equal '("Blah.scala" "Blah$.scala" 
				   "Blah$Spec.scala" "FunctionalBlah$.scala" 
				   "FunctionalBlah.scala") 
				 (tgt-possible-src-file-names "FunctionalBlah$Spec.scala" 
											  '("Unit" "Integration" "Functional") 
											  '("Spec" "$Spec"))))
  (should (equal '("Blah.scala") (tgt-possible-src-file-names 
								 "BlahTest.scala" 
								 'nil 
								 '("Spec" "$Spec" "Test" "$Test")))))

(ert-deftest should-generate-possible-test-filename ()
  (should (equal '("test_foo.py" "spec_foo.py")  
				 (tgt-possible-test-file-names "foo.py" '("test_" "spec_") 'nil)))
  (should (equal '("foo_test.rb" "foo_spec.rb")  
				 (tgt-possible-test-file-names "foo.rb" '() '("_test" "_spec"))))
  (should (equal '("foo.rb") (tgt-possible-test-file-names "foo.rb" '() '())))
  (should (equal '("FunctionalBlah$Spec.scala" "FunctionalBlahSpec.scala" 
				   "IntegrationBlah$Spec.scala" "IntegrationBlahSpec.scala"
				   "UnitBlah$Spec.scala" "UnitBlahSpec.scala" 
				   "UnitBlah.scala" "IntegrationBlah.scala" 
				   "FunctionalBlah.scala" "BlahSpec.scala" "Blah$Spec.scala") 
				 (tgt-possible-test-file-names "Blah.scala" 
											   '("Unit" "Integration" "Functional") 
											   '("Spec" "$Spec")))))

(ert-deftest should-generate-all-possible-toggle-paths ()
  (multiple-value-bind (scala-proj py-proj) (setup-test-projects) 
	(should (equal (list (file-truename 
						  "/tmp/projects/python-proj/tests/foo/test___init__.py"))
				   (tgt-all-toggle-paths "foo/__init__.py" py-proj 
										 :test-dirs #'tgt-possible-test-file-names)))
	(should (equal (list (file-truename
						  "/tmp/projects/python-proj/src/foo/blah.py"))
				   (tgt-all-toggle-paths "foo/test_blah.py" py-proj 
										 :src-dirs #'tgt-possible-src-file-names)))
	(should (equal (list 
					(file-truename "/tmp/projects/python-proj/tests/test_foo.py"))
				   (tgt-all-toggle-paths "foo.py" py-proj 
										 :test-dirs #'tgt-possible-test-file-names)))
	
	(should (equal 
			 (list (file-truename 
					"/tmp/projects/scala-proj/foo-module/src/controllers/Blah.scala") 
			   (file-truename 
				"/tmp/projects/scala-proj/play/app/controllers/Blah.scala"))  
			 (tgt-all-toggle-paths "controllers/BlahTest.scala" scala-proj 
							   :src-dirs #'tgt-possible-src-file-names)))))

(ert-deftest should-handle-test-with-no-prefix-suffix ()
  (setq tgt-projects '(((:root-dir "/projects") 
						(:test-dirs "tests") 
						(:src-dirs "src"))))
  (should (equal '("/projects/tests/blah/bar.py") 
				 (tgt-find-match "/projects/src/blah/bar.py")))
  (should (equal '("/projects/src/foo.py") 
				 (tgt-find-match "/projects/tests/foo.py"))))

(ert-deftest should-find-all-matches ()
  "tests the top level matching function"
  (multiple-value-bind (scala-proj py-proj) (setup-test-projects) 
	
	(let ((msg "")) 
	  ;Mock message 
	  (flet ((message (str &rest args) (setq msg (apply 'format (cons str args)))))

	   (should-not (tgt-find-match (file-truename "/not-configured-proj/src/foo.py")))
	   (should (string= (concat "File '/not-configured-proj/src/foo.py' not part of" 
						   " any project. Have you defined a project?")
					  msg))
	   
	   (should-not (tgt-find-match
					(file-truename "/tmp/projects/python-proj/build/foo.py")))
	   (should (string= (format 
					   "File '%s' in project '%s' is not part src-dirs or test-dirs"
							  (file-truename "/tmp/projects/python-proj/build/foo.py")
							  (file-truename "/tmp/projects/python-proj")) 
						msg))))

	(should (equal `(,(file-truename "/tmp/projects/python-proj/src/foo/blah.py"))
				   (tgt-find-match
					(file-truename "/tmp/projects/python-proj/tests/foo/test_blah.py"))))
	(should (equal `(,(file-truename "/tmp/projects/python-proj/tests/test_foo.py"))
				   (tgt-find-match 
					(file-truename "/tmp/projects/python-proj/src/foo.py"))))
	(should (equal 
			 (mapcar 
			  #'file-truename 
			  '("/tmp/projects/scala-proj/foo-module/tests/foo/bar/Blah$Test.scala" 
				"/tmp/projects/scala-proj/foo-module/tests/foo/bar/BlahTest.scala"
				"/tmp/projects/scala-proj/foo-module/tests/foo/bar/BlahSpec.scala" 
				"/tmp/projects/scala-proj/foo-module/tests/foo/bar/Blah$Spec.scala"
				"/tmp/projects/scala-proj/play/specs/foo/bar/Blah$Test.scala" 
				"/tmp/projects/scala-proj/play/specs/foo/bar/BlahTest.scala"
				"/tmp/projects/scala-proj/play/specs/foo/bar/BlahSpec.scala" 
				"/tmp/projects/scala-proj/play/specs/foo/bar/Blah$Spec.scala"
				"/tmp/projects/scala-proj/integration-specs/foo/bar/Blah$Test.scala"
				"/tmp/projects/scala-proj/integration-specs/foo/bar/BlahTest.scala"
				"/tmp/projects/scala-proj/integration-specs/foo/bar/BlahSpec.scala"
				"/tmp/projects/scala-proj/integration-specs/foo/bar/Blah$Spec.scala"))
			 (tgt-find-match 
			  (file-truename 
			   "/tmp/projects/scala-proj/foo-module/src/foo/bar/Blah.scala"))))))

(ert-deftest should-filter-existing-files ()  
  (should (equal `(,'nil ,'nil) (tgt-best-matches 'nil)))
  (let ((file1 (make-temp-file "foo")) 
		(file2 (make-temp-file "bar")))
	(should (equal `(,'nil ("/tmp/foo/1" "/tmp/foo/3" "/tmp/foo/2")) 
		 (tgt-best-matches '("/tmp/foo/1" "/tmp/foo/3" "/tmp/foo/2"))))
	(should (equal 
			 `(,t (,file1)) 
			 (tgt-best-matches (list file1 "/tmp/f00" "/tmp/b00"))))
	(should (equal 
			 `(,t (,file2 ,file1)) 
			 (tgt-best-matches (list file2 "/doesnt-exist/1" file1))))))



;;End to end/Integration test
(defun click (txt)
  (catch 'break
	(while t
	  (forward-button 1)						
	  (if (string= (button-label (button-at (point))) txt)
		  (progn 
			(push-button)
			(throw 'break 'nil))))))

(ert-deftest should-toggle ()
  (let ((root (make-temp-file "foo" t)))
	(setq tgt-projects 'nil)
	(add-to-list 'tgt-projects 
				 `((:root-dir ,root) 
				   (:src-dirs "src") 
				   (:test-dirs "unit-specs" "integration-specs" ) 
				   (:test-suffixes "$Spec")))
	
	(mkdir (expand-file-name "src" root) t)
	(find-file (expand-file-name "src/Foo.scala" root))
;;  toggle for <root>/src/Foo.scala => <root>/unit-specs/Foo$Spec.scala
;;                                     <root>/functional-specs/Foo$Spec.scala
;;  Should present the choices
;;  select <root>/unit-specs/Foo$Spec.scala
;;
;;  if <root>/unit-specs/Foo$Spec.scala exists then
;;  toggle for <root>/src/Foo.scala => <root>/unit-specs/Foo$Spec.scala without prompt
	(let ((tgt-open-in-new-window 'nil)) 
	  (tgt-toggle)
	  (should (equal "*Toggle Test*" (buffer-name)))
	  (click (file-truename (expand-file-name "unit-specs/Foo$Spec.scala" root)))
	  (should (file-directory-p (expand-file-name "unit-specs/" root)))
	  (should (equal buffer-file-truename 
					 (file-truename (expand-file-name "unit-specs/Foo$Spec.scala" root))))
	  (save-buffer) ; make sure <root>/unit-specs/Foo$Spec.scala is created
	  (find-file (expand-file-name "src/Foo.scala" root))
	  (tgt-toggle) ; This time no prompt to use. 1 exact match.
	  (should (equal buffer-file-truename 
					 (file-truename (expand-file-name "unit-specs/Foo$Spec.scala" root)))))
	
;;  toggle for <root>/src/Blah.scala => <root>/unit-specs/Blah$Spec.scala
;;                                     <root>/functional-specs/Blah$Spec.scala
;;  present the choices. select <root>/integration-specs/Blah$Spec.scala
;;
;;  toggleing from <root>/integration-specs/Blah$Spec.scala should directly take you 
;;  src without any prompts
	
	(find-file (expand-file-name "src/Blah.scala" root))
	(let ((tgt-open-in-new-window t)) 
	  (tgt-toggle)
	  (should (equal "*Toggle Test*" (buffer-name)))
	  (click (file-truename (expand-file-name "integration-specs/Blah$Spec.scala" root)))
	  (should (file-directory-p (expand-file-name "integration-specs/" root)))
	  (should (equal buffer-file-truename 
					 (file-truename 
					  (expand-file-name "integration-specs/Blah$Spec.scala" root))))
	  (tgt-toggle)
	  (should (equal buffer-file-truename 
					 (file-truename 
					  (expand-file-name "src/Blah.scala" root)))))))

