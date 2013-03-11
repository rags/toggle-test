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
```lisp
(global-set-key (kbd "C-c t") 'tgt-toggle)
```

Configuring
-------------
Once you have installed the extension you can start configuring toggle test by adding your projects. Lets look 
sample projects one with simple configure and a more involved example.
