;;; presence-mode.el --- Connects with the Presence server for Discord Rich Presence

;; Copyright (C) 2018 by github.com/volskaya

;; Author: ---
;; Version: 1.0.0
;; Url: https://github.com/volskaya/presence-mode
;; Keywords: discord, rich, presence, github
;; License: GPL-3.0

;;; Commentary:
;; Sends file path and current major mode to the Presence server, which then
;; processes this info and sends a Discord Presence, which would look something
;; like:
;;          Working on Presence
;;          Pushed 3 of 7
;;;
;;;-*- lexical-binding: t; -*-
;;; Code:

(require 'json)
(require 'url)

(defvar url-http-end-of-headers)

(defgroup presence nil
  "Presence options"
  :prefix "presence-"
  :group 'external)

(defconst presence-url "http://127.0.0.1:8080"
  "Url of the Presence RPC server.")

(defconst presence--id (emacs-pid)
  "Process PID used as the ID for which Presence will cache repo info.")

(defconst presence--json-body
  `((id . ,presence--id)
    (editor . "Emacs")))

(defvar presence--timer nil
  "Reference to the update timer.")

(defvar presence--last-file-name nil
  "Used to decide whether to send a ping or a set_path request.")

(defvar presence--last-buffer nil
  "The last current buffer.")

(defvar presence--active-buffer-name nil
  "Active buffer name.")

(defvar presence--should-be-active nil
  "Used as a boolean, to track inbetween post insert and lose frame.")

(defcustom presence-user-dir (file-name-directory load-file-name)
  "Directory containing Presence files."
  :type 'directory
  :risky t
  :group 'presence)

(defcustom presence-server-exe-dir (expand-file-name "bin" presence-user-dir)
  "Installation prefix used to install Presence."
  :type 'directory
  :group 'presence)

(defconst presence--on-windows (eq system-type "windows-nt")
  "Return t, if running on windows.")

(defconst presence--server-exe-name (if presence--on-windows
                                        "presence.exe"
                                      "presence")
  "Presence executable name, depending on the OS.")

(defconst presence--server-url
  (format "https://raw.githubusercontent.com/volskaya/presence/master/bin/%s"
          presence--server-exe-name)
  "Github link for server executable.")

(define-minor-mode presence-mode
  "Minor mode for sending data to the Presence server"
  nil nil nil
  :require 'presence-mode
  :global t
  :group 'presence
  :after-hook
  (progn
    (cond
     (presence-mode
      (presence--enable))
     (t
      (presence--disable)))))

(defun presence--is-the-same-buffer ()
  "Return t, if active buffer is not the same major buffer anymore."
  (if (equal buffer-file-name presence--active-buffer-name)
      t
    nil))

(defun presence--flag-as-active ()
  "Send out SET_PATH method with the current active filename and its mode."
  (unless (and presence--should-be-active (presence--is-the-same-buffer))
    (setq presence--should-be-active t
          presence--active-buffer-name buffer-file-name)
    (presence--set-path)))

(defun presence--flag-as-inactive ()
  "Flip should-be-active to nil."
  (setq presence--should-be-active nil))

(defun presence--has-buffer-changed ()
  "Call presence--flag-as-inactive, if the buffer has changed."
  (unless (eq (current-buffer) presence--last-buffer)
    (setq presence--last-buffer (current-buffer))
    (presence--flag-as-inactive)))

(defun presence--enable-hooks ()
  "Subscribe hooks."
  (add-hook 'post-self-insert-hook #'presence--flag-as-active)
  (add-hook 'post-command-hook #'presence--has-buffer-changed)
  (add-function :after after-focus-change-function #'presence--flag-as-inactive))

(defun presence--disable-hooks ()
  "Unsubscribe hooks."
  (remove-hook 'post-self-insert-hook #'presence--flag-as-active)
  (remove-hook 'post-command-hook #'presence--has-buffer-changed)
  (remove-function after-focus-change-function #'presence--flag-as-inactive))

(defun presence--create-directory (path)
  "Create the directory PATH, if it doesn't exist."
  (make-directory path t))

(defun presence-download-server (command)
  "Fetch Presence binary from volskaya/presence. COMMAND."
  (interactive
   (list
    (let* ((presence-path
            (expand-file-name presence--server-exe-name
                              (file-name-as-directory presence-server-exe-dir))))
      (progn (presence--create-directory presence-server-exe-dir)
             (url-copy-file presence--server-url presence-path t nil nil)
             (unless presence--on-windows
               (set-file-modes
                presence-path (logior 73 (file-modes presence-path)))))))))

(defun presence--start-server ()
  "Start the server or session, if the server is already running."
  (presence--is-running
   (lambda (result)
     (if result (when presence-mode (presence--start-session))
       (presence--ensure-server)))))

(defun presence--enable ()
  "Ensures Presence server is running and begins sending data to it."
  (presence--start-server))

(defun presence--stop-loop ()
  "Stop the update timer."
  (when presence--timer
    (cancel-timer presence--timer)
    (setq presence--timer nil)))

(defun presence--disable ()
  "Sends shutdown notice to the Presence server."
  (presence--stop-session)
  (presence--leave))

(defun presence--update-presence ()
  "Send active mode and file path to the Presence server."
  (when buffer-file-name
    (unless (equal buffer-file-name presence--last-file-name)
      (presence--set-path)))
  (presence--ping))

(defun presence--on-server-die ()
  "Called when a post request fails."
  (setq presence--last-file-name nil
        presence-mode nil)
  (presence--stop-session)
  (princ "presence-mode disabled, server died"))

(defun presence--post (data callback)
  "Posts DATA wrapped in RPC boiler plate to Presence server.
After response recieved, call CALLBACK on its results"
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data
         (json-encode
          (append presence--json-body data))))
    (url-retrieve presence-url
                  `(lambda (event)
                     (if (car event) (,callback nil)
                       (progn (goto-char url-http-end-of-headers)
                              (let ((json-object-type 'plist)
                                    (json-key-type 'symbol)
                                    (json-array-type 'vector))
                                (,callback (json-read))
                                )))
                     ) nil t)))

(defun presence--ping ()
  "Pings the Presence server."
  (presence--post `((method . "ping"))
                  (lambda (result)
                    (unless result (presence--on-server-die)))))

(defun presence--set-path ()
  "Sends PATH to the Presence server."
  (when buffer-file-name
    (setq presence--last-file-name buffer-file-name)
    (presence--post `((method . "set_path")
                      (params . ((path . ,buffer-file-name)
                                 (language . ,major-mode))))
                    `(lambda (result)
                       (unless result (presence--on-server-die))))))

(defun presence--is-running (callback)
  "Query Presence server and call CALLBACK on its response."
  (presence--post '((method . "is_running")) callback))

(defun presence--leave ()
  "Sends leave notice to the Presence server."
  (presence--post `((method . "im_leaving"))
                  `(lambda (result)
                     ;; Do nothing
                     )))

(defvar presence--server-executable nil)
(defvar presence--nohup-prefix
  (if presence--on-windows
      ""
    "nohup ")
  "Nohup prefix, to prevent the server from exitting with Emacs.")

(defun presence--find-server-executable ()
  "Return t if Presence server executable exists."
  (let* ((exec-path (cons presence-server-exe-dir exec-path))
         (exe (executable-find presence--server-exe-name)))
    (if (and exe (file-executable-p exe))
        exe
      nil)))

(defun presence--start-process ()
  "Start Presence server."
  (let ((process-connection-type nil)
        (process-adaptive-read-buffering nil)
        process)
    (setq process (start-process-shell-command
                   "Presence" nil (format "%s%s"
                                          presence--nohup-prefix
                                          (shell-quote-argument
                                           presence--server-executable))))
    (set-process-query-on-exit-flag process nil)))

(defun presence--await-rpc ()
  "Await for json rpc and begin ping loop."
  (presence--is-running
   (lambda (result)
     (if result (when presence-mode (presence--start-session))
       (unless presence--timer  ;; Prevent repeatedly constructing the timer
         (setq presence--timer (run-at-time nil 1 'presence--await-rpc)))))))

(defun presence--start-session ()
  "Subscribe to hooks and start update loop."
  (when presence--timer (presence--stop-loop))
  (presence--enable-hooks)
  (setq presence--timer (run-at-time nil 15 'presence--update-presence)))

(defun presence--stop-session ()
  "Unsubscribe hooks and stop looping ping."
  (presence--disable-hooks)
  (presence--stop-loop))

(defun presence--ensure-server ()
  "Ensure the Presence server is running."
  (setq presence--server-executable (presence--find-server-executable))
  (if presence--server-executable
      (progn (presence--start-process)
             (presence--await-rpc))
    (princ (format
            "Presence server is not present! Run presence-download-server to retrieve it from %s"
            presence--server-url))
    (setq presence-mode nil)))

(provide 'presence)
;;; presence-mode.el ends here
