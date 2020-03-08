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

(defgroup transcript-search nil
  "Transcript search options"
  :group 'transcript)

;; =================================================================================================
;; search options

(defcustom transcript-re-ignore-case t
  "If `non-nil' all defined regular expression used in the highlighting profiles are `case-INsensitive',
  if `nil' they will be `case-sensitive'"
  :group 'transcript-search)

;; TODO: this var should be used a chunk dimension of the text to be checked every time a line of
;; interest if searched for. So it should be used to set the LIMIT value of the function searching
;; for a characteristic change as: LIMIT = (+ (point) transcript-other-line-search-limit)
;; (defcustom transcript-other-line-search-limit nil
;;   "Define a limit to search for the next loi.

;; When nil search till the end of the buffer (for next line search) or till the start of the buffer
;; (for previous line search). When non-nil it must be an integer, which indicates the point where the 
;; "
;;   :group transcript-search)

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
;; moving functions (based on the highlighting property changes)

(defun transcript--warn-message (loi-keyword)
  "Just warn the user that no more lines of the specified type are found.

LOI is the keyword to be used in the message"
  (message "no more %s lines found" (cdr (assoc loi-keyword transcript-loi-strings))))

(defmacro transcript-define-loi-move (name loi-keyword search-dir)
  "Blueprint to define the function called NAME to navigate among the loi.

LOI-KEYWORD is one of the keywords: `fatal', `critical', `error', `warning', `note'.
While the SEARCH-DIR is an integer indicating the direction where to search for the loi, in
particular `1' to look forward and `-1' to look backward."

  ;; In emacs only functions to detect a property change are available, hence the next line must be
  ;; first analyzed to check for the loi.

  ;; TODO: the following instruction is wrong as it does not assign the value to the "doc-string"
  ;; function on-the-fly, string substitutions are better
  `(setq doc-string (let ((prev-or-next (if (= ,search-dir 1)
                                            "next"
                                          "previous"))
                          (loi-string (cdr (assoc ,loi-keyword transcript-loi-strings))))
                      (format "Move to the %s found %s line (if any).
  Warn the user if no further %s lines can be found." prev-or-next loi-string loi-string)))

  `(defun ,name ()
     "TODO: add a mechanism to automatically generate the function doc-strig"
     (interactive)
     (let ((pnt)                           ; symbol where point will be stored
           (starting-point (point))        ; keep track of the last
           (continue-search t)
           ;; Ensure to get the same lisp-object as the face found in text, otherwise the text
           ;; property comparison yield always "nil". Hence use "caddr" instead of "cdr" function.
           (target-face (caddr (assoc ,loi-keyword transcript-loi-faces)))
           (search-func))

       ;; assign the search function
       (if (= ,search-dir 1)
           (fset 'search-func 'next-single-property-change)
         (fset 'search-func 'previous-single-property-change))

       (save-excursion
         (beginning-of-line)
         (forward-line ,search-dir)
         (setq pnt (point))

         ;; Ensure the point haven't reached the beginning of the buffer, the pointer isn't nil and the
         ;; research can continue.
         (while (and pnt continue-search)
           (if (or (eq (get-text-property pnt 'face) target-face) (bobp))
               (setq continue-search nil)
             (setq pnt (search-func pnt 'face (current-buffer) nil))))
         (when (or
                ;; Reaching the end of the buffer means that no other lines with the target face were
                ;; found. Hence the point should remain in its first position.
                (bobp)
                ;; When the property search returns nil, the line cannot be found till the end of the
                ;; buffer.
                (not pnt))
           (setq pnt starting-point)
           (transcript--warn-message ,loi-keyword)))

       ;; point to the last found line, or remain to the starting point when none were found
       (goto-char pnt))))

(transcript-define-loi-move transcript-next-fatal :fatal 1)
(transcript-define-loi-move transcript-previous-fatal :fatal -1)
(transcript-define-loi-move transcript-next-critical :critical 1)
(transcript-define-loi-move transcript-previous-critical :critical -1)
(transcript-define-loi-move transcript-next-error :error 1)
(transcript-define-loi-move transcript-previous-error :error -1)
(transcript-define-loi-move transcript-next-warning :warning 1)
(transcript-define-loi-move transcript-previous-warning :warning -1)
(transcript-define-loi-move transcript-next-note :note 1)
(transcript-define-loi-move transcript-previous-note :note -1)


;; =================================================================================================
;; mode support definitions

(setq transcript-loi-faces '((:fatal . 'hi-fatal)
                             (:critical . 'hi-critical)
                             (:error . 'hi-error)
                             (:warning . 'hi-warning)
                             (:note . 'hi-note)))

(setq transcript-loi-strings '((:fatal . "FATAL")
                               (:critical . "CRITICAL")
                               (:error . "ERROR")
                               (:warning . "WARNING")
                               (:note . "NOTE")))

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
                                  :fatal "^.*\\(?:fatal\\).*$"
                                  :critical "^.*\\(?:critical\\).*$"
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

  ;; define the single-key mapping
  (define-key transcript-mode-map (kbd "f") #'transcript-next-fatal)
  (define-key transcript-mode-map (kbd "F") #'transcript-previous-fatal)
  (define-key transcript-mode-map (kbd "c") #'transcript-next-critical)
  (define-key transcript-mode-map (kbd "C") #'transcript-previous-critical)
  (define-key transcript-mode-map (kbd "e") #'transcript-next-error)
  (define-key transcript-mode-map (kbd "E") #'transcript-previous-error)
  (define-key transcript-mode-map (kbd "w") #'transcript-next-warning)
  (define-key transcript-mode-map (kbd "W") #'transcript-previous-warning)
  (define-key transcript-mode-map (kbd "n") #'transcript-next-note)
  (define-key transcript-mode-map (kbd "N") #'transcript-previous-note)

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
