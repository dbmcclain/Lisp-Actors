# Getting started

```common-lisp
(setf asdf:*central-registry* (list* #P"/mnt/bs-raidz2/git/COMMON-LISP/cl-advice/" asdf:*central-registry*))
(ql:quickload :cl-advice)

(setf asdf:*central-registry* (list* #P"/mnt/bs-raidz2/git/COMMON-LISP/Lisp-Actors/xTActors/actors-base/" asdf:*central-registry*))
(setf asdf:*central-registry* (list* #P"/mnt/bs-raidz2/git/COMMON-LISP/Lisp-Actors/useful-macros/" asdf:*central-registry*))
(setf asdf:*central-registry* (list* #P"/mnt/bs-raidz2/git/COMMON-LISP/Lisp-Actors/mpcompat/" asdf:*central-registry*))

(load "/mnt/bs-raidz2/git/COMMON-LISP/Lisp-Actors/project-packages-sbcl")
(load "/mnt/bs-raidz2/git/COMMON-LISP/Lisp-Actors/project-mappings")

(ql:quickload :com.ral.useful-macros/ext)
(ql:quickload :com.ral.actors)
```

A very simple program for a start: [`xTActors/Examples/mytest.lisp`](./xTActors/Examples/mytest.lisp).

http://www.dalnefre.com/wp/
