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
  :group 'convenience)

(defgroup transcript-default-faces nil
  "Transcript default faces to highlight the line of interest"
  :group 'transcript)

(defvar transcript-re-ignore-case t
  "If `non-nil' all defined regular expression used in the highlighting profiles are `case-INsensitive',
  if `nil' they will be `case-sensitive'")

;; =================================================================================================
;; faces
;; NOTE: a taste of the colors can be achieved with rainbow-mode enabled
(defface hi-fatal-fbg
  '((t (:weight bold :foreground "black" :background "goldenrod")))
  "Highlighting for fatal notes."
  :group 'transcript-default-faces)

(defface hi-critical-fbg
  '((t (:weight bold :foreground "white" :background "dark red")))
  "Highlighting for critical notes."
  :group 'transcript-default-faces)

(defface hi-error-fg
  '((t (:weight bold :foreground "red1")))
  "Highlighting for errors."
  :group 'transcript-default-faces)

(defface hi-warning-fg
  '((t (:weight bold :foreground "OrangeRed")))
  "Highlighting for warnings."
  :group 'transcript-default-faces)

(defface hi-note-fg
  '((t
     (:weight bold :foreground "DodgerBlue1")))
  "Highlighting for notes."
  :group 'transcript-default-faces)


;; =================================================================================================
;; mode support definitions

;; simply create a list to store all the highlight profiles
(setq transcript-profile-list '(("transcript-default-profile"
                                 ("^.*\\(?:fatal\\).*$" . 'hi-fatal-fbg)
                                 ("^.*\\(?:critical\\).*$" . 'hi-critical-fbg)
                                 ("^.*\\(?:error\\).*$" . 'hi-error-fg)
                                 ("^.*\\(?:warning\\).*$" . 'hi-warning-fg)
                                 ("^.*\\(?:note\\).*$" . 'hi-note-fg))))

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
