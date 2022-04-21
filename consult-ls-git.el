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
;; all files.

;; For each view there is also a separate command to show just this
;; information.

;;; Code:

(require 'consult)



(provide 'consult-ls-git)
;;; consult-ls-git.el ends here
