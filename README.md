Toggle Test [![Build Status](https://travis-ci.org/rags/toggle-test.png?branch=master)](https://travis-ci.org/rags/toggle-test)
===========

Toggle test provides IntelliJ like test toggle functionality. It presents the user with choices in case there are 
multiple macthes (Ex: You have integration and unit test for the same source file ). It created the file (test or 
source), along with the entire directory hierarchy if the file does not exist.

Installing
-----------
To get test toggle. Simply get the code from github.
```bash
git clone git://github.com/rags/toggle-test.git
```
or
```bash
git submodule add git://github.com/rags/toggle-test.git ~/emacs.d/plugins/toggle-test
```

And add the following lines to your [initialization file](http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html).
```lisp
(load-file "/path/to/toggle-test.el"); load toggle-test.el
(require 'toggle-test)
```

**Recommended key binding**
This is the recommended key binding for test toggle.
```lisp
(global-set-key (kbd "C-c t") 'tgt-toggle)
```

Configuring
-------------
**tgt-projects:**

Once you have installed the extension you can start configuring toggle test by adding your projects. 
This configuration allows 
```lisp
  (add-to-list 'tgt-projects '((:root-dir <root directory of project>)
                              (:src-dirs <list source folders relative to root>) 
                              (:test-dirs <list of test folders relative to root>)
                              (:test-prefixes <optional list of prefix strings that are added on source file names 
                                            to get test file names>)
                              (:test-prefixes <optional list of suffix strings that are added on source file names 
                                            to get test file names>)))
```
**tgt-open-in-new-window:**

This setting controls where the toggle file opens up. A non nil value opens toggle file in a new window, 
so you do a side-by-side edit of source and test files. A nil value replaces the current window content. 
The default value is **t**  
```lisp
  (setq tgt-open-in-new-window <'nil or t>)
```

**Basic Example**
- - - - - - - - -
For a python project follows coding convention of prefixing "test_" for test names and has a directory structure 
that look like this
```
py-proj
|-- lib
|-- src
|   |-- bar.py
|   `-- foo.py
`-- tests
    `-- test_foo.py
```
The configuration would look loke this
```lisp
(add-to-list 'tgt-projects '((:root-dir "~/py-proj")
                              (:src-dirs "src") 
                              (:test-dirs "tests")
                              (:test-prefixes "test_")))
```
