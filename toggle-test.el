(eval-when-compile (require 'cl))

(defgroup toggle-test nil
  "IntelliJ like facility to quickly toggle between source and its corresponding test files."
  :group 'convenience
  :prefix "tgt-"
  :link '(emacs-library-link :tag "Lisp File" "toggle-test.el"))

;; A list of projects. Each item in this list is a an alist that specifies,
;; 1. The project root directory
;; 2. Source folder(s) (relative to root directory)
;; 3. Test source folder(s) (relative to root directory)
;; 4. prefix/suffix attached to source file names to name the test files
;;    Ex: src/foo.py <-> test/test_foo.py - the prefix is 'test_'
;;        src/controllers/Foo.scala <-> specs/controllers/Foo$Spec.scala 
;;        The suffix here is '$Spec'.
;;    Note: you can specify both prefix and suffix if required.
;;
;; Usage:
;; (add-to-list tgt-projects '((:root-dir "~/python-project") 
;;                             (:src-dirs "src") (:test-dirs "tests") 
;;                             (:test-prefixes "test-")))          
;; (add-to-list tgt-projects '((:root-dir "~/scala-project") 
;;                             (:src-dirs "src") (:test-dirs "specs") 
;;                             (:test-suffixes "$Spec")))
(defcustom tgt-projects '() 
  "Project entries. 
One entry per project that provides naming convention and folder structure"
  :group 'toggle-test
  :type '(repeat (alist)))

;; Indicates if the toggle file should be opened in a new window 
;; or replace the buffer in current window. The default behavior is to open it in new window
;; Use (setq tgt-open-in-new-window 'nil) to override default behavior
(defcustom tgt-open-in-new-window t 
  "Indicates if the files are opened in new window or current window"
  :group 'toggle-test
  :type 'boolean)

(defun tgt-proj-prop (prop proj) (cdr (assoc prop proj)))
(defun tgt-root-dir (proj) (car (tgt-proj-prop :root-dir proj)))

(defun tgt-relative-file-path (file proj dir-type) 
  (reduce 
   (lambda (cur_val dir) 
     (or cur_val 
	 (let ((src-dir (file-name-as-directory 
			 (expand-file-name dir (tgt-root-dir proj))))) 
	   (if (tgt-is-ancestor-p src-dir file)
	       (subseq file (length src-dir)))))) 
   (cdr (assoc dir-type proj))
   :initial-value 'nil))

;; Given a file return its project 
;; (returns first match. Doesnt handle sub directories added as different project)
(defun tgt-proj-for (file)
  (car (remove-if-not
		(lambda (proj) (tgt-is-ancestor-p (tgt-root-dir proj) file)) tgt-projects)))

(defun tgt-find-project-file-in-dirs (file proj)
  (let ((src-file-rel-path (tgt-relative-file-path file proj :src-dirs)))
    (if src-file-rel-path 
	(values src-file-rel-path 'nil)
      (values 'nil (tgt-relative-file-path file proj :test-dirs)))))

(defun tgt-find-match (file) 
  (let ((proj (tgt-proj-for file)))
      (cond (proj 
	   (multiple-value-bind 
	       (src-file-rel-path test-file-rel-path) 
	       (tgt-find-project-file-in-dirs file proj)
	     (cond
	      (src-file-rel-path (tgt-all-toggle-paths 
							  src-file-rel-path proj :test-dirs 
							  #'tgt-possible-test-file-names))
	      (test-file-rel-path (tgt-all-toggle-paths 
							   test-file-rel-path proj :src-dirs 
							   #'tgt-possible-src-file-names))
	      (t (message "File '%s' in project '%s' is not part src-dirs or test-dirs"
			  file (tgt-root-dir proj)) 'nil))))
	  (t (message "File '%s' not part of any project. Have you defined a project?" file) 
		 'nil))))

(defun tgt-best-matches (all-matches)
  (let ((exact-matches (remove-if-not #'file-exists-p all-matches)))
	(if exact-matches (values t exact-matches) (values 'nil all-matches))))

(defun tgt-all-toggle-paths (rel-path proj dir-type file-names-generator)
  (tgt-make-full-paths 
   (tgt-possible-dirs proj dir-type (file-name-directory rel-path)) 
   (funcall file-names-generator 
			(file-name-nondirectory rel-path) 
			(tgt-proj-prop :test-prefixes proj) 
			(tgt-proj-prop :test-suffixes proj))))

(defun tgt-make-full-paths (dirs filenames)
  (tgt-cross-join dirs filenames (lambda (dir file) (expand-file-name file dir))))

;rel-dir-path is com/foo/bar for src in "proj-root/src/com/foo/bar/Blah.java
(defun tgt-possible-dirs (proj dir-type rel-dir-path)
  (let ((root (tgt-root-dir proj)))
	(mapcar (lambda (dir) (expand-file-name rel-dir-path (expand-file-name dir root))) 
			(tgt-proj-prop dir-type proj))))


(defun tgt-remove-file-prefix (prefix file) 
  (if (string-match (concat "^" prefix) file) (replace-match "" t t file) 'nil))

(defun tgt-remove-file-suffix (name suffix ext)
  (if (string-match (concat suffix "$") name) 
	  (concat (replace-match "" t t name) ext) 
	'nil))

(defun tgt-remove-file-preffix-suffix (prefix name suffix ext) 
  (tgt-remove-file-prefix prefix (or (tgt-remove-file-suffix name suffix ext) "")))

(defun tgt-possible-src-file-names (file prefixes suffixes)
  (tgt-possible-file-names file prefixes suffixes 
						   #'tgt-remove-file-prefix
						   #'tgt-remove-file-suffix
						   #'tgt-remove-file-preffix-suffix))

(defun tgt-possible-test-file-names (file prefixes suffixes)
  (tgt-possible-file-names file prefixes suffixes #'concat #'concat #'concat))

(defun tgt-santize-seq (seq)
  ;(append seq 'nil) converts vectors/sequences/list to list
  (delete-dups (remove 'nil (append seq 'nil))))

(defun tgt-possible-file-names (file prefixes suffixes 
									 pref-fn suff-fn
									 pref-suff-fn)
  (or (tgt-santize-seq 
	   (let ((name (file-name-sans-extension file))
			 (ext (file-name-extension file t))
			 (ret-val '()))
		 (setq ret-val (vconcat 
						(mapcar (lambda (prefix) (funcall pref-fn prefix file)) prefixes)
						(mapcar (lambda (suffix) (funcall suff-fn name suffix ext)) suffixes)))
	 
		 (if (and prefixes suffixes) 
			 (setq ret-val (vconcat (tgt-cross-join 
									 prefixes suffixes 
									 (lambda (prefix suffix) 
									   (funcall pref-suff-fn prefix name suffix ext))) 
									ret-val)))
	 
		 ret-val)) (list file)))

;join 2 lists - to make list of tuples by default. 
;fn argument can override how 2 elements join
(defun tgt-cross-join (list1 list2 &optional fn)
  (cond ((not list1) list2)
		((not list2) list1)
		(t (let ((ret-val '())) 
			 (dolist (i list1)
			  (dolist (j list2) 
				(add-to-list 'ret-val (if fn (funcall fn i j) (list i j)))))
			 ret-val))))

(defun tgt-open-file (file)
  (tgt-find-file file (if tgt-open-in-new-window #'find-file-other-window #'find-file)))

(defun tgt-find-file (file find-file-fn)
  (mkdir (file-name-directory file) t);Ensure parent directory exits
  (funcall find-file-fn file))


(defun tgt-show-matches (matches exact-match-p)
  (with-output-to-temp-buffer "*Toggle Test*"  
	(funcall (if tgt-open-in-new-window 
				 #'switch-to-buffer-other-window #'switch-to-buffer) "*Toggle Test*")
	(princ (if exact-match-p 
			   "Mutiple matching files were found. Choose one to open:\n"
			 "No matching file found. These are the potentials. Pick one to create:\n"))
	(dolist (file matches)
	  (princ "* ")
	  (insert-button file 'action (lambda (btn) 
									(tgt-find-file 
									 (button-label btn) #'find-alternate-file)))
	  (princ "\n"))))


(defun tgt-open (files)
  (if files
	  (multiple-value-bind (exact-match-p matches) (tgt-best-matches files)
		(cond
		 ((= 1 (length matches)) (tgt-open-file (car matches)))
		 (t (tgt-show-matches matches exact-match-p))))))

(defun tgt-toggle ()
  (tgt-open (tgt-find-match (file-truename buffer-file-name))))

(defun tgt-is-ancestor-p (dir file)
  (if (and dir file (> (length file) 0) (> (length dir) 0))
   (let ((dir-name (file-name-as-directory (expand-file-name dir))) 
	 (file-name (expand-file-name file)))
	 (and (>= (length file-name) (length dir-name)) 
	      (string= (substring file-name 0 (length dir-name)) dir-name))) 'nil))

(provide 'toggle-test)
;;;;;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;

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
		      "/tmp/projects/scala-proj/foo-module/src/Foo.scala" 
		      scala-proj :src-dirs)))
    (should (string= "bar/" (tgt-relative-file-path 
			     "/tmp/projects/scala-proj/foo-module/src/bar/" 
			     scala-proj :src-dirs)))
    (should (string= "Blah.scala" (tgt-relative-file-path 
				   "/tmp/projects/scala-proj/play/app/Blah.scala" 
				   scala-proj :src-dirs)))
    (should (string= "bar/Blah.scala" 
		     (tgt-relative-file-path 
		      "/tmp/projects/scala-proj/foo-module/src/bar/Blah.scala" 
		      scala-proj :src-dirs)))
    (should-not (tgt-relative-file-path 
	     "/tmp/projects/scala-proj/play/app1/Blah.scala" 
	     scala-proj :src-dirs))
    (should-not (tgt-relative-file-path 
	     "/tmp/projects/scala-proj/foo-module/src/bar/Blah.scala" 
	     scala-proj :test-dirs))
    (should-not (tgt-relative-file-path 
	     "/tmp/projects/python-proj/src/foo.py" 
	     scala-proj :src-dirs))
    (should (string= "src/tests_foo.py"
		     (tgt-relative-file-path 
		      "/tmp/projects/python-proj/tests/src/tests_foo.py" 
		      py-proj :test-dirs)))
    (should (string= "controllers/" 
		     (tgt-relative-file-path 
		      "/tmp/projects/python-proj/tests/controllers/" 
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
  (add-to-list 'tgt-projects '((:root-dir "/foo/blah")))
  (add-to-list 'tgt-projects '((:root-dir "/foo/bar/baz/")))
  (add-to-list 'tgt-projects '((:root-dir "/blah/bar//..")))
  (should (equal '((:root-dir "/foo/blah")) (tgt-proj-for "/foo/blah/bar.el")))
  (should (equal '((:root-dir "/foo/blah")) (tgt-proj-for "/foo/blah/baz/bar.el")))
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
	(should (equal '("/tmp/projects/python-proj/tests/foo/test___init__.py")
				   (tgt-all-toggle-paths "foo/__init__.py" py-proj 
										 :test-dirs #'tgt-possible-test-file-names)))
	(should (equal '("/tmp/projects/python-proj/src/foo/blah.py")
				   (tgt-all-toggle-paths "foo/test_blah.py" py-proj 
										 :src-dirs #'tgt-possible-src-file-names)))
	(should (equal 
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
			   "/tmp/projects/scala-proj/integration-specs/foo/bar/Blah$Spec.scala")
			 (tgt-all-toggle-paths "foo/bar/Blah.scala" scala-proj 
								   :test-dirs #'tgt-possible-test-file-names)))
	(should (equal 
			 '("/tmp/projects/scala-proj/foo-module/src/controllers/Blah.scala" 
			   "/tmp/projects/scala-proj/play/app/controllers/Blah.scala")  
			 (tgt-all-toggle-paths "controllers/BlahTest.scala" scala-proj 
							   :src-dirs #'tgt-possible-src-file-names)))))

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



