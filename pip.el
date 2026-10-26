;;; pip.el --- PIP Emacs Integration -*- lexical-binding: t -*-

;; Copyright (C) 2026 Yunsik Jang

;; Author: Yunsik Jang <z3ph1e@gmail.com>
;; Keywords: python package
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/zeph1e/pip.el
;; License : WTFPL version 2 (https://www.wtfpl.net/)

;;; Commentary:

;; Provide a minor mode to work with PIP, a python package manager in Emacs.

;;; Code:

(require 'url)
(require 'json)
(require 'mail-utils)

(defgroup pip nil
  "PIP Emacs Integration."
  :group 'tools
  :prefix "pip-")

(defcustom pip-cache-dir (expand-file-name
                          (concat user-emacs-directory ".cache/pip/"))
  "Path to PIP cache."
  :type 'directory
  :group 'pip)

(defun pip-cache--file ()
  "Return the path to the PIP cached list."
  (expand-file-name ".packages" pip-cache-dir))

(defconst pip-cache-version 1
  "PIP list cache version.")

(defvar pip-cache nil
  "Cached PIP list.

Cache structure:
 (:version 1
  :token <freshness-token>
  :packages (<package1 name> <package2 name> ...))")

(defconst pip-pypi-list-url "https://pypi.org/simple/"
  "PyPI package list URL in simple version.")

(defconst pip-pypi-info-url-format "https://pypi.org/pypi/%s/json"
  "PyPI package Info JSON API format.")

(defun pip-cache--retrieve (&optional header-only)
  "Fetch PyPI package list from pypi.org.
If HEADER-ONLY is non-nil, fetch only HTTP Headers."
  (let* ((url-request-method (if header-only "HEAD" "GET"))
         (url-mime-accept-string "application/vnd.pypi.simple.v1+json"))
    (url-retrieve-synchronously pip-pypi-list-url)))

(defun pip-cache--response-status (buffer)
  "Return the HTTP status code from BUFFER as an integer, or nil."
  (with-current-buffer buffer
    (if (and (boundp 'url-http-response-status) url-http-response-status)
        url-http-response-status
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
          (string-to-number (match-string 1)))))))

(defun pip-cache--freshness-token (&optional buffer-or-name)
  "Get hint token that the requested URL got updated from BUFFER-OR-NAME.
If BUFFER-OR-NAME is nil, it uses new buffer with retrieved URL contents."
  (when-let* ((buffer (or buffer-or-name (pip-cache--retrieve t))))
    (unwind-protect
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (or (mail-fetch-field "Last-Modified")
                (mail-fetch-field "ETag"))))
      (unless buffer-or-name
        (kill-buffer buffer)))))

(defun pip-cache--updated-p ()
  "Predicate cache is updated."
  (when-let* ((token (pip-cache--freshness-token)))
    (not (and (eq (pip-cache--get :version) pip-cache-version)
              (equal (pip-cache--get :token) token)))))

(defun pip-cache--get (&optional key)
  "Get whole cache or certain value in KEY."
  (let ((cache (or pip-cache
                   (setq pip-cache
                         (when (file-exists-p (pip-cache--file))
                           (with-temp-buffer
                             (insert-file-contents (pip-cache--file))
                             (read (buffer-string))))))))
    (if key (plist-get cache key) cache)))

(defun pip-cache--save ()
  "Save cache to cache file."
  (make-directory (file-name-directory (pip-cache--file)) t)
  (with-temp-buffer
    (insert (prin1-to-string pip-cache))
    (let ((buffer-save-without-query t))
      (write-file (pip-cache--file)))))

(defun pip-cache--update ()
  "Update cache with fresh contents from PyPI."
  (message "Retrieving PIP package list...")
  (let ((buffer (pip-cache--retrieve)))
    (unwind-protect
        (progn
          (unless (and buffer (eql (pip-cache--response-status buffer) 200))
            (error "Unable to update PIP cache!"))
          (with-current-buffer buffer
            (let ((token (pip-cache--freshness-token buffer))
                  (packages (mapcar (lambda (prj) (plist-get prj :name))
                                    (plist-get
                                     (save-excursion
                                       (goto-char (point-min))
                                       (when (re-search-forward "^$" nil t)
                                         (delete-region (point-min) (point))
                                         (json-parse-buffer
                                          :object-type 'plist)))
                                     :projects))))
              (setq pip-cache `(:version ,pip-cache-version :token ,token
                                         :packages ,packages)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (pip-cache--save)))

(defun pip-cache-get (&optional key)
  "Get whole cache or certain value in KEY.
If cache is empty or invalid, it will be updated."
  (when (pip-cache--updated-p)
    (pip-cache--update))
  (pip-cache--get key))

(defun pip-util--privileged-p ()
  (not (file-writable-p
        (nth 1 (split-string (shell-command-to-string
                              "pip show pip | grep Location"))))))

(defun pip-util--run-command (command)
  (let ((command (if (pip-util--privileged-p)
                     (concat "sudo " command) command)))
    (async-shell-command command)))

(defun pip-install (package)
  "Install PACKAGE with PIP."
  (interactive
   (list (completing-read "Package to install: "
                          (pip-cache-get :packages) nil t)))
  (unless (string= package "")
    (pip-util--run-command
     (format "pip install -U %s" (shell-quote-argument package)))))

(defun pip-uninstall (package)
  "Uninstall PACKAGE with PIP."
  (interactive
   (list (completing-read
          "Package to uninstall: "
          (condition-case nil
              (mapcar (lambda (item) (plist-get item :name))
                      (json-parse-string
                       (shell-command-to-string
                        "pip list -l --format=json")
                       :object-type 'plist))
            (error nil))
          nil t)))
  (unless (string= package "")
    (pip-util--run-command
     (format "pip uninstall %s" (shell-quote-argument package)))))

(defvar pip-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c p i") 'pip-install)
    (define-key map (kbd "C-c p u") 'pip-uninstall)
    map)
  "Keymap for `pip-mode'.")

;;;###autoload
(define-minor-mode pip-mode
  "This minor mode supports functionality to manage Python package through pip.

Key bindings:
\\{pip-mode-keymap}"
  :keymap pip-mode-keymap
  :lighter " pip")

(provide 'pip)

;;; pip.el ends here
