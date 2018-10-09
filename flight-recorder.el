;;;;; (c) 2016-2018 Vsevolod Dyomkin <vseloved@gmail.com>

(defvar *frlog-port* "7654")
(defvar *frlog-notified-of-conn-failure* nil)
(defvar *frlog-session* nil)

(defvar *frlog-serial* 0)

(defun url-encode (s)
  (let ((unquoted-re "[^a-zA-Z0-9_./-]")
        (encoded (encode-coding-string s 'utf-8))
        (n 0))
    (while (setq n (string-match unquoted-re encoded n))
      (setq encoded
            (replace-match (format "%%%X" (string-to-char
                                           (match-string 0 encoded)))
                           t t encoded)
            n (1+ n)))
    encoded))

(defun frlog (text &optional title)
  (setq text (with-temp-buffer
               (lisp-mode)
               (insert text)
               (indent-region 0 (point))
               (string-trim (buffer-string))))
  (when (not (string-empty-p text))
    (let ((url-request-method "POST")
          (url-request-data (encode-coding-string text 'utf-8)))
      (url-retrieve (concat "http://localhost:"
                            *frlog-port*
                            "/frlog?title="
                            (if title (url-encode title) "")
                            "&tag="
                            (or *frlog-session* ""))
                    (lambda (status)
                      (if status
                        (unless *frlog-notified-of-conn-failure*
                          (message "Couldn't connect to flog.")
                          (setf *frlog-notified-of-conn-failure* t))
                        (setf *frlog-notified-of-conn-failure* nil)))))))

(defun frlog-start-session ()
  (interactive)
  (setf *frlog-session* (read-string "Session tag: ")))

(defun frlog-end-session ()
  (interactive)
  (setf *frlog-session* nil))


;;; monkey-patching slime

(defun slime-repl-eval-string (string)
  (frlog string (downcase (concat (slime-current-package)
                                  " ("
                                  (number-to-string (incf *frlog-serial*))
                                  ")")))
  (slime-rex ()
      ((if slime-repl-auto-right-margin
           `(swank-repl:listener-eval
             ,string
             :window-width ,(with-current-buffer (slime-output-buffer)
                              (window-width)))
         `(swank-repl:listener-eval ,string))
       (slime-lisp-package))
    ((:ok result)
     (slime-repl-insert-result result))
    ((:abort condition)
     (slime-repl-show-abort condition))))

(defun slime-presentation-write (string &optional target)
  (case target
    ((nil) ; Regular process output
     (frlog string)
     (slime-repl-emit string))
    (:repl-result
     (slime-presentation-write-result string))
    (t (slime-emit-to-target string target))))

(defun slime-presentation-write-result (string)
  (with-current-buffer (slime-output-buffer)
    (let ((marker (slime-output-target-marker :repl-result))
          (saved-point (point-marker)))
      (goto-char marker)
      (slime-propertize-region `(face slime-repl-result-face
                                      rear-nonsticky (face))
        (insert string))
      ;; Move the input-start marker after the REPL result.
      (set-marker marker (point))
      (set-marker slime-output-end (point))
      ;; Restore point before insertion but only it if was farther
      ;; than `marker'. Omitting this breaks REPL test
      ;; `repl-type-ahead'.
      (when (> saved-point (point))
        (goto-char saved-point)))
    (slime-repl-show-maximum-output))
  (frlog string))

(defun slime-repl-show-abort (condition)
  (frlog condition)
  (with-current-buffer (slime-output-buffer)
    (save-excursion
      (slime-save-marker slime-output-start
        (slime-save-marker slime-output-end
          (goto-char slime-output-end)
          (insert-before-markers (format " ; Evaluation aborted on %s.\n"
            condition))
          (slime-repl-insert-prompt))))
    (slime-repl-show-maximum-output)))
