;;; transcript.el --- Emacs Mode For Handy Log File Analysis -*- lexical-binding: t -*-

;; Copyright (C) Samuele Favazza

;; Author: Samuele Favazza <sfavazza.github@gmail.com>
;; URL: https://github.com/sfavazza/transcript
;; Version: 0.0
;; Keywords: log, transcript
;; Package-Requires: 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs Mode To Ease Tools Output Log Files Analysis

;; transcript mode provides line highlighting for lines which requires user attention like: fatal,
;; errors, warnings, file names and line numbers.
;; It offers to jump to the reported offending line.

;; For more information, read the transcript-mode documentation on the official repository page:

;; https://github.com/sfavazza/transcript

;;; Code:

;; =================================================================================================
;; user customizations

;; groups
(defgroup transcript nil
  "Transcript mode customization."
  :group 'convenience
  :link '(url-link "https://github.com/sfavazza/transcript-mode"))

(defgroup transcript-default-faces nil
  "Transcript default faces to highlight the line of interest"
  :group 'transcript)

(defvar transcript-re-ignore-case t
  "If `non-nil' all defined regular expression used in the highlighting profiles are `case-INsensitive',
  if `nil' they will be `case-sensitive'")

;; =================================================================================================
;; faces
;; NOTE: a taste of the colors can be achieved with rainbow-mode enabled
(defface hi-fatal
  '((t (:weight bold :foreground "black" :background "goldenrod")))
  "Highlighting for fatal notes."
  :group 'transcript-default-faces)

(defface hi-critical
  '((t (:weight bold :foreground "white" :background "dark red")))
  "Highlighting for critical notes."
  :group 'transcript-default-faces)

(defface hi-error
  '((t (:weight bold :foreground "red1")))
  "Highlighting for errors."
  :group 'transcript-default-faces)

(defface hi-warning
  '((t (:weight bold :foreground "DarkOrange")))
  "Highlighting for warnings."
  :group 'transcript-default-faces)

(defface hi-note
  '((t
     (:weight bold :foreground "green")))
  "Highlighting for notes."
  :group 'transcript-default-faces)

;; =================================================================================================
;; moving functions

;; the movements are based on the text highlighting as it constitute a text property change.
;; TODO: define a macro to define a movement template function depending on the given profile template
;; :fatal :critical :error :warning :note

;; =================================================================================================
;; mode support definitions

(defun transcript-define-profile (name &rest msg-level)
  "Define a new highlithing profile called NAME.

The MSG-LEVEL is a list of keyword arguments (`:fatal', `:critical', `:error', `:warning', `:note')
whose value is a regular-expression used to look for the specific line of interest. The face is
automatically selected according to the keyword."

  ;; when one keyword is not indicated it is simply omitted from the font-lock keywords
  (let ((profile '()))

    (let ((regexp (plist-get msg-level :fatal)))
      (when regexp
        (add-to-list 'profile `(,regexp . 'hi-fatal))))

    (let ((regexp (plist-get msg-level :critical)))
      (when regexp
        (add-to-list 'profile `(,regexp . 'hi-critical))))

    (let ((regexp (plist-get msg-level :error)))
      (when regexp
        (add-to-list 'profile `(,regexp . 'hi-error))))

    (let ((regexp (plist-get msg-level :warning)))
      (when regexp
        (add-to-list 'profile `(,regexp . 'hi-warning))))

    (let ((regexp (plist-get msg-level :note)))
      (when regexp
        (add-to-list 'profile `(,regexp . 'hi-note))))

    ;;TODO: define a plist keeping track of the defined keywords so that when defining the moving
    ;;functions they will be aware of the face property to look for

    ;; return the profile list
    (add-to-list 'profile name)))

;; simply create a list to store all the highlight profiles
(setq transcript-profile-list
      '((transcript-define-profile "default"
                                   :critical ("^.*\\(?:critical\\).*$" . 'hi-critical)
                                   :fatal ("^.*\\(?:fatal\\).*$" . 'hi-fatal)
                                   :error ("^.*\\(?:error\\).*$" . 'hi-error)
                                   :warning ("^.*\\(?:warning\\).*$" . 'hi-warning)
                                   :note ("^.*\\(?:note\\).*$" . 'hi-note))
        (transcript-define-profile "modelsim"
                                   :error ("^\\*\\{2\\}\\s-Error:.*$" . 'hi-error)
                                   :warning ("^\\*\\{2\\}\\s-Error:.*$" . 'hi-warning))))

;; =================================================================================================
;; mode implementation
(define-derived-mode transcript-mode
  ;; deriving from the "special" mode, the buffer becomes read-only
  special-mode "Transcript"
  "An emacs mode to ease tools output log files analysis."
  :group 'transcript

  ;; -----------------------------------------------------------------------------------------------
  ;; body of the derived mode
  (setq transcript-font-lock-keywords
        (let* ((profiles
                ;; create the list of available profiles
                (mapcar 'car transcript-profile-list))
               (profile-sel
                ;; ask user for the profile to be adopted
                (completing-read "Highlight profile: " profiles)))
          ;; define keywords for font-lock mode
          (cdr (assoc profile-sel transcript-profile-list))))

  (setq font-lock-defaults
        '(transcript-font-lock-keywords nil transcript-re-ignore-case))
  ;; re-fontify current buffer as the defaults are directly changed
  (font-lock-refresh-defaults)
  )

(provide 'transcript)
;;; transcript.el ends here
