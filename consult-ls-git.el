;;; consult-ls-git.el --- Consult integration for git  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  Robin Joy
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (consult "0.16"))
;; URL: https://github.com/rcj/consult-ls-git

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides several consult commands to interact with git.

;; `consult-ls-git' provides a multi-view for opening files from a git
;; repository. It shows modified and untracked files, local branches
;; and tags, stashes, open buffers in this repository and a list of
;; all tracked files.

;;; Code:

(require 'consult)

(defcustom consult-ls-git-sources
  '(consult-ls-git--source-tracked-files)
  "Sources used by `consult-ls-git'")

(defvar consult-ls-git--project-root nil)

(defvar consult-ls-git--source-tracked-files
  (list :name     "Tracked files"
        :narrow   '(?f . "Tracked files")
        :category 'file
        :face     'consult-file
        :history  'file-name-history
        :action   (lambda (f) (consult--file-action (concat consult-ls-git--project-root f)))
        :items
        (lambda ()
          (split-string
           (shell-command-to-string (format "git -C %s ls-files -z" consult-ls-git--project-root))
           "\000" 'omit-nulls))))

(defun consult-ls-git--get-project-root ()
  "Return git project root.

If default-directory isn't inside a git repository, call `project-root' to select a project.
Returns nil in case no valid project root was found."
  (or (locate-dominating-file default-directory ".git")
      (locate-dominating-file (project-prompt-project-dir) ".git")
      (error "Not a git repository")))

;;;###autoload
(defun consult-ls-git ()
  "Create a multi view for current git repository."
  (interactive)
  (let ((consult-ls-git--project-root (consult-ls-git--get-project-root)))
    (consult--multi consult-ls-git-sources
                    :prompt "Switch to: "
                    :require-match t
                    :sort nil)))

(provide 'consult-ls-git)
;;; consult-ls-git.el ends here
