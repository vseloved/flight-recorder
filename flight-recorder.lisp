(ql:quickload :local-time)

(defpackage #:flrec
  (:use #:cl #:rutilsx #:hunch)
  (:local-nicknames (#:re #:ppcre)
                    (#:htt #:hunchentoot)))

(in-package #:flrec)
(named-readtables:in-readtable rutilsx-readtable)


(url "/frlog" (text title tag)
  (when (eql :POST (htt:request-method*))
    (unless text (setf text (htt:raw-post-data :force-text t))))
  (when text
    (when (blankp title) (void title))
    (when (blankp tag) (void tag))
    (with-open-file (out (fmt "~~/.frlog~@[-~A~].md" tag)
                         :direction :output :if-does-not-exist :create
                         :if-exists :append)
      (format out "~&~A~@[~A~] ~A~%~%    ~A~%~%"
              (if title "### " ";;;")
              title
              (local-time:format-timestring
               nil (local-time:now)
               :format '((:year 4) #\- (:month 2) #\- (:day 2)
                         #\_ (:hour 2) #\: (:min 2) #\: (:sec 2)))
              (re:regex-replace-all (fmt "~%") text (fmt "~%    "))))))
