Toggle Test [![Build Status](https://travis-ci.org/rags/toggle-test.png?branch=master)](https://travis-ci.org/rags/toggle-test)
===========

Toggle Test allows you quick between test and source files. This is a very useful feature to have when you are [TDD](http://en.wikipedia.org/wiki/Test-driven_development)ing.
Test Toggle is similar to test toggle functionality provided by IntelliJ/pycharm/resharper.

**Features:**
* Test Toggle allows you quick navigate between test and source files, without having to remember file paths.
* It creates the the test or source file you are trying to navigate to (along the entire directory hierarchy) 
  if it does not already exist.
* Sometimes there might be mutiple test files for a given source (Ex: Unit and Integration test). Test Toggle 
  presents the user with choices whenever there is multiple matches and navigates to the chosen file 
  (creating it of required).
* Test Toggle is language/tech stack agnostic. So it can work with your rails, python, scala or any other project.
* You can work with multiple such projects at the same time.


Installing
-----------
To install test toggle, simply get the code from github.
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

This is the recommended key binding for toggle test.
```lisp
(global-set-key (kbd "C-c t") 'tgt-toggle)
```

Configuring
------------
Before you can start using Test Toggle you need to tell Test Toggle about your projects, you can do this by configuring 
the variable *tgt-projects*. The other optional configuration is *tgt-open-in-new-window* which controls where the 
files are opened. The rest of this section describes these 2 variables.

**tgt-projects:**

This configuration allows Toggle Test to understand the project structure and naming conventions. tgt-projects is a list 
where each item is a project configuration. Here is how you add to this list.

```lisp
  (add-to-list 'tgt-projects '((:root-dir <root directory of project>)
                              (:src-dirs <list source folders relative to root>) 
                              (:test-dirs <list of test folders relative to root>)
                              (:test-prefixes <optional list of prefix strings that are added on source file names 
                                            to get test file names>)
                              (:test-suffixes <optional list of suffix strings without the file extension>))) 
```

Each project that you configure is an alist with *:root-dir, :src-dirs, :test-dirs* as mandatory and 
*:test-prefixes, :test-suffixes* as optional entries. 

**Assumptions:** Test Toggle makes 2 assumptions about the all projects defined. These are not tricky assumptions. 
Most projects have folder organization and naming conventions that complement these assumptions

* The test and source directory structure (tree hierarchy) is similar.
* The test file name can be derived by adding some prefix or suffix to source file name. 

  Ex: if the source file is src/controllers/user_contoller.py then the test is 
      test/controllers/user_controller_test.py. Note that the :test-suffixes here is "_test"


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
The configuration for this project would look look this
```lisp
(add-to-list 'tgt-projects '((:root-dir "~/py-proj")
                              (:src-dirs "src") 
                              (:test-dirs "tests")
                              (:test-prefixes "test_")))
```

**A more involved example**
- - - - - - - - - - - - -
This is a project that has 3 modules a scala library, java library and a scala Play application.
```
polyglot-proj
|-- bar-lib
|   |-- src
|   |   `-- com
|   |       `-- bar
|   |           `-- Bar.java
|   `-- test
|       `-- com
|           `-- bar
|               `-- BarTest.java
|-- foo-lib
|   |-- src
|   |   `-- Foo.scala
|   `-- test
|       `-- FooSpec.scala
|-- integration-specs
|   |-- Foo$Spec.scala
|   `-- controllers
|       `-- Application$Spec.scala
`-- play
    |-- app
    |   `-- controllers
    |       `-- Application.scala
    `-- test
        `-- controllers
            `-- ApplicationSpec.scala

```
Based on these observations about the project:

* Scala tests have suffixes **Spec**, **$Spec** and java tests have **Test** suffix.
* Integration tests for *foo-lib* and play app is in integration-specs.
* *bar-lib* is an independent/self-contained module that has all tests/source source inside the bar-lib folder.
* *bar-lib* has a completely different naming convention.

The best configuration for this project is,
```lisp
(add-to-list 'tgt-projects '((:root-dir "~/polyglot-proj")
                              (:src-dirs "play/app" "foo-lib/src") 
                              (:test-dirs "play/test" "foo-lib/test" "integration-specs")
                              (:test-suffixes "Spec" "$Spec")))
(add-to-list 'tgt-projects '((:root-dir "~/polyglot-proj/bar-lib")
                              (:src-dirs "src") 
                              (:test-dirs "test")
                              (:test-suffixes "Test")))

```
Note that the potential candidates for *foo-lib/src/Foo.scala* are
* play/test/FooSpec.scala
* play/test/Foo$Spec.scala
* foo-lib/test/FooSpec.scala
* foo-lib/test/Foo$Spec.scala
* integration-specs/test/FooSpec.scala
* integration-specs/test/Foo$Spec.scala

The best match is *foo-lib/test/FooSpec.scala* as this file already exists. 

If this file did not exist, then the user will be presented with all the options and the selected file is automatically 
created.


**Adding new projects vs adding new src/test directories:**

Note that in the configuration in the second example defines separate projects for *bar-lib*, while *foo-lib* 
is just extra entries in *:src-dirs* and *:test-dirs*. It is recommended that you define many smaller projects 
whenever possible. If a module is independent and contains all tests inside module directory then it is better 
to add it as a separate tgt-project. A separate project cannot be defined in case of *foo-lib* because 
integration-specs depends on both *foo-lib* and the Play application 


**tgt-open-in-new-window:**

This setting controls where the toggle file opens up. A non nil value opens the toggle file in a new window, 
so you can do a side-by-side edit of source and test files. A nil value replaces the current window content. 
The default value is **t**  
```lisp
  (setq tgt-open-in-new-window <'nil or t>)
```

Contributing
--------------------------
* Feel free to [fork](https://help.github.com/articles/fork-a-repo) the [repo](https://github.com/rags/toggle-test), 
make changes and raise [pull request](https://help.github.com/articles/using-pull-requests) once you are done. 
Toggle Test uses [ert](http://www.gnu.org/software/emacs/manual/html_mono/ert.html) for unit/intergration tests. 
Keep them green. Test your changes.
* Report any bugs and feature requests in the [issues section](https://github.com/rags/toggle-test/issues)

License
--------
[GNU General Public License](http://www.gnu.org/licenses/)

Copyright (C) 2013 [Raghunandan Rao](mailto:r.raghunandan@gmail.com)

