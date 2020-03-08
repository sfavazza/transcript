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

(setq transcript-loi-faces '((:fatal . 'hi-fatal)
                             (:critical . 'hi-critical)
                             (:error . 'hi-error)
                             (:warning . 'hi-warning)
                             (:note . 'hi-note)))

(defun transcript-define-profile (name &rest loi-regexps)
  "Define a new highlithing profile called NAME.

The LOI-REGEXPS is a list of cons of the form (:KEYWORD . REGEXP).
Where the :KEYWORD is one of `:fatal', `:critical', `:error', `:warning', `:note' and REGEXP is the
regular expression string to match a line of interest (loi).
The face is automatically selected according to the keyword."

  ;; The LOI-REGEXP argument is consumed to extract the keywords and relative regular expressions.
  ;; The faces are extracted by the internal list of loi key-words.  
  ;; When one keyword is not indicated it is simply omitted from the font-lock keywords
  (let ((profile '())
        (regexp-list loi-regexps))

    ;; define an empty p-list for the line of interest (loi) of the created profile
    (put 'profile :loi-faces '())

    (while regexp-list

      (let* ((loi-keyword (car regexp-list))
             (loi-regexp (cadr regexp-list))
             (loi-face (cdr (assoc loi-keyword transcript-loi-faces))))
        ;; Add the loi face to the profile property, so that the moving functions can identify
        ;; whether their target loi is described in the defined profile.
        (put 'profile :loi-faces (append (get 'profile :loi-faces) `(,loi-face)))
        ;; add a cons with the regexp and face to the profile being defined
        (add-to-list 'profile `(,loi-regexp . ,loi-face)))

      ;; consume the list
      (setq regexp-list (cddr regexp-list)))

    ;; return the profile list
    (add-to-list 'profile name)))

;; simply create a list to store all the highlight profiles
(setq transcript-profile-list
      (list
       (transcript-define-profile "default"
                                  :critical "^.*\\(?:critical\\).*$"
                                  :fatal "^.*\\(?:fatal\\).*$"
                                  :error "^.*\\(?:error\\).*$"
                                  :warning "^.*\\(?:warning\\).*$"
                                  :note "^.*\\(?:note\\).*$")))

;; =================================================================================================
;; mode implementation
(define-derived-mode transcript-mode
  ;; deriving from the "special" mode, the buffer becomes read-only
  special-mode "Transcript"
  "An emacs mode to ease tools' output log files analysis."
  :group 'transcript

  ;; -----------------------------------------------------------------------------------------------
  ;; body of the derived mode
  (setq transcript-font-lock-keywords
        (let* (
               ;; create the list of available profiles
               (profiles (mapcar 'car transcript-profile-list))
               ;; ask user for the profile to be adopted
               (profile-sel (completing-read "Highlight profile: " profiles)))
          ;; define keywords for font-lock mode
          (cdr (assoc profile-sel transcript-profile-list))))

  (setq font-lock-defaults
        '(transcript-font-lock-keywords nil transcript-re-ignore-case))
  ;; re-fontify current buffer as the defaults are directly changed
  (font-lock-refresh-defaults)
  )

(provide 'transcript)
;;; transcript.el ends here
