(in-package :cl-user)

(ql:quickload :drakma)

(defparameter *frlog-port* 7654)

(defun frlog (text &key title tag)
  (drakma:http-request (format nil "http://localhost:~a/frlog" *frlog-port*)
                       :method :POST
                       :parameters `(,(when title `("title" . ,title))
                                     ,(when tag `("tag" . ,tag)))
                       :content text
                       :external-format-out :utf-8))
                       
