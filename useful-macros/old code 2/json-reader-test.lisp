;; To run these tests,
;;
;; 1. (LOAD "json-reader.lisp")   ;; load json reader
;; 2. (LOAD "test.lisp")          ;; load this file
;; 3. (run-tests :json-test)      ;; run the tests
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(cl:in-package #:cl-user)

(defpackage #:json-test)

(json-reader:enable-json-syntax)

(defun random-number ()
  (random (expt 2 32)))

(defun random-string ()
  (with-output-to-string (out)
    (loop repeat (random 10)
       do (format out "~A " (random (expt 2 32))))))

(defun run-tests (package)
  (do-symbols (s package)
    (when (fboundp s)
      (format t "~&~A: ~A" (symbol-name s)
              (handler-case (progn (funcall s) t)
                (error (c) c))))))

(defun json-test::vector-empty ()
  (let ((x []))
    (assert (vectorp x))
    (assert (zerop (length x)))))

(defun json-test::vector-single-element ()
  (let ((x [1]))
    (assert (vectorp x))
    (assert (= (length x) 1))
    (assert (= (elt x 0) 1))))

(defun json-test::vector-true-false ()
  (let ((x [true, false]))
    (assert (vectorp x))
    (assert (= (length x) 2))
    (assert (eql (elt x 0) t))
    (assert (eql (elt x 1) nil))))

(defun json-test::vector-strings ()
  (let ((x ["foo", "bar", "baz"]))
    (assert (vectorp x))
    (assert (= (length x) 3))
    (assert (every #'string-equal x '("foo" "bar" "baz")))))

(defun json-test::vector-lisp-forms ()
  (let* ((w "blah")
         (x [ "foo", 1, (+ 3 4), w ]))
    (assert (vectorp x))
    (assert (= (length x) 4))
    (assert (every #'equalp x (list "foo" 1 7 w)))))

(defun json-test::hash-table-empty ()
  (let ((x {}))
    (assert (hash-table-p x))
    (assert (zerop (hash-table-count x)))))

(defun json-test::hash-table-single-entry ()
  (let ((x {"foo": 1}))
    (assert (hash-table-p x))
    (assert (= (hash-table-count x) 1))
    (assert (eql (gethash "foo" x) 1))))

(defun json-test::hash-table-table-single-null-entry ()
  (let ((x {"foo": null}))
    (assert (hash-table-p x))
    (assert (= (hash-table-count x) 1))
    (assert (eql (gethash "foo" x) nil))))

(defun json-test::hash-table-multiple-entries ()
  (let ((x {
             "foo": 1,
             "bar": 2,
             "baz": 3
            }))
    (assert (hash-table-p x))
    (assert (= (hash-table-count x) 3))
    (assert (eql (gethash "foo" x) 1))
    (assert (eql (gethash "bar" x) 2))
    (assert (eql (gethash "baz" x) 3))))

(defun json-test::hash-table-lisp-forms ()
  (let* ((w "blah")
         (x {
              "foo": 1,
              "bar": (+ 3 4),
              "baz": w
            }))
    (assert (hash-table-p x))
    (assert (= (hash-table-count x) 3))
    (assert (eql (gethash "foo" x) 1))
    (assert (eql (gethash "bar" x) 7))
    (assert (eql (gethash "baz" x) w))))

(defun json-test::hash-table-key-literals ()
  (let ((x { foo: 1, bar: 2 }))
    (assert (hash-table-p x))
    (assert (= (hash-table-count x) 2))
    (assert (eql (gethash "foo" x) 1))
    (assert (eql (gethash "bar" x) 2))))

(defun json-test::vector-includes-hash-table ()
  (let ((x [ {  foo: 1 } ]))
    (assert (vectorp x))
    (assert (= (length x) 1))
    (let ((hash-table (elt x 0)))
      (assert (hash-table-p hash-table))
      (assert (eql (gethash "foo" hash-table) 1)))))

(json-reader:disable-json-syntax)
