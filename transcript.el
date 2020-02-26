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
(defgroup transcript nil
  "Transcript mode customization."
  :group 'convenience
  :link (url-link "https://github.com/sfavazza/transcript"))

(defgroup transcript-default-faces nil
  "Transcript default faces to highlight the line of interest"
  :group 'transcript)


;; =================================================================================================
;; faces
;; NOTE: a taste of the colors can be achieved with rainbow-mode enabled
(defface hi-fatal-fg
  '((t (:weight bold :foreground "black" :background "goldenrod")))
  "Highlighting for fatal notes."
  :group 'transcript-default-faces)

(defface hi-critical-fg
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
;; mode-support definitions

;;TODO: definition of profile concept: list of cons (reg-exp . face)

;; =================================================================================================
;; mode implementation
(define-derived-mode transcript-mode
  ;; deriving from the "special" mode, the buffer becomes read-only
  special-mode "Transcript"
  "An emacs mode to ease tools output log files analysis."
  :group 'transcript

  ;; -----------------------------------------------------------------------------------------------
  ;; body of the derived mode

  ;; disable automatic fontification to let the user decide how what should be highlighted

  ;; define keywords for font-lock mode
  ;; source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html#Search_002dbased-Fontification
  (setq transcript-font-lock-keywords
        '("error")           ; errors
        )
  (setq font-lock-defaults '(("[Ee]rror" . 'hi-red-b)))

  ;; -----------------------------------------------------------------------------------------------
  ;; define the most common faces
  
  )

;;; transcript.el ends here
