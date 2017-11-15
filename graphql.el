;;; graphql.el --- GraphQL utilities                 -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: hypermedia, tools, lisp
;; Package-Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Graphql.el provides a generally-applicable domain-specific language
;; for creating and executing GraphQL queries against your favorite
;; web services.

;;; Code:

(require 'pcase)

(defun graphql--encode-atom (g)
  (cond
   ((stringp g)
    g)
   ((symbolp g)
    (symbol-name g))
   ((numberp g)
    (number-to-string g))
   ((and (consp g)
         (not (consp (cdr g))))
    (symbol-name (car g)))))
(defun graphql--encode-list (l)
  (when (and (consp l) (consp (car l)))
    (mapconcat #'graphql--encode l " ")))
(defun graphql--encode-argument-spec (spec)
  (graphql--encode-argument (car spec) (cdr spec)))
(defun graphql--encode-argument (key value)
  (format "%s:%s" key (graphql--encode-argument-value value)))
(defun graphql--encode-argument-value (value)
  (cond
   ((symbolp value)
    (symbol-name value))
   ((eq '$ (car-safe value))
    (format "$%s" (cadr value)))
   ((listp value)
    (format "{%s}" (mapconcat #'graphql--encode-argument-spec value ",")))
   ((stringp value)
    (format "\"%s\"" value))
   ((numberp value)
    (number-to-string value))
   (t
    (graphql--encode value))))
(defun graphql--encode-parameter-spec (spec)
  "Encode a parameter SPEC.
SPEC is expected to be of the following form:

   (NAME TYPE [REQUIRED] . [DEFAULT])

NAME is the name of the parameter.

TYPE is the parameter's type.

A non-nil value for REQUIRED will indicate the parameter is
required.  A value of `!' is recommended.

A non-nil value for DEFAULT will provide a default value for the
parameter."
  (let ((last (last spec)))
    (graphql--encode-parameter (car spec)
                               (cadr spec)
                               (car last)
                               (cdr last))))
(defun graphql--encode-parameter (name type &optional required default)
  (format "$%s: %s%s%s"
          (symbol-name name)
          (symbol-name type)
          (if required "!" "")
          (if default
              (concat " = " (graphql--encode-argument-value default))
            "")))

(defun graphql--get-keys (g)
  (let (graph keys)
    (while g
      (if (keywordp (car g))
          (let* ((param (pop g))
                 (value (pop g)))
            (push (cons param value) keys))
        (push (pop g) graph)))
    (list keys (nreverse graph))))

(defun graphql-encode (g)
  "Encode G as a GraphQL string."
  (or (graphql--encode-atom g)
      (graphql--encode-list g)
      (pcase (graphql--get-keys g)
        (`(,keys ,graph)
         (let ((object (car graph))
               (name (alist-get :op-name keys))
               (params (alist-get :op-params keys))
               (arguments (alist-get :arguments keys))
               (fields (cdr graph)))
           (concat
            (symbol-name object)
            (when arguments
              ;; Format arguments "key:value, ..."
              (format "(%s)"
                      (mapconcat #'graphql--encode-argument-spec arguments ",")))
            (when name
              (format " %S" name))
            (when params
              (format "(%s)"
                      (mapconcat #'graphql--encode-parameter-spec params ",")))
            (when fields
              (format " { %s }"
                      (mapconcat #'graphql-encode fields " ")))))))))

(defun graphql--genform-operation (args kind)
  (pcase args
    (`(,graph)
     `(graphql-encode '(,kind ,graph)))

    (`((,name) ,graph)
     `(graphql-encode '(,kind :op-name ,name
                              ,graph)))
    (`((,name ,parameters) ,graph)
     `(graphql-encode '(,kind :op-name ,name
                              :op-params ,parameters
                              ,graph)))

    (_ (error "bad form"))))

(defmacro graphql-query (&rest args)
  "Construct a Query object.
Calling pattern:

  (fn GRAPH) := Just encode GRAPH as a Query.

  (fn (NAME) GRAPH) := Give the Query a NAME.

  (fn (NAME PARAMETERS) GRAPH) := Give the Query PARAMETERS;
                                  see below.

Parameters are formatted as defined by
`graphql--encode-parameter-spec'."
  (graphql--genform-operation args 'query))

(defmacro graphql-mutation (&rest args)
  "Construct a Mutation object.
Calling pattern:

  (fn GRAPH) := Just encode GRAPH as a Mutation.

  (fn (NAME) GRAPH) := Give the Mutation a NAME.

  (fn (NAME PARAMETERS) GRAPH) := Give the Mutation PARAMETERS;
                                  see below.

Parameters are formatted as defined by
`graphql--encode-parameter-spec'."
  (graphql--genform-operation args 'query))

(provide 'graphql)
;;; graphql.el ends here
