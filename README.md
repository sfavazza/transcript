# Transcript Mode #

Emacs mode for handy log file analysis. It is useful to inspect usually text based tools output log files.
This mode offers line highlighting of the most relevant lines like: `fatal`, `errors`, `warnings`, `critical
warnings`, `note`.

Basic Configuration
-------------------

To use this mode ensure that the `*.el` files are in your load path.
As there is no unified way for tools to generate their output logs, the user should manually define
the reg-exp for the file naming/extension on which enable this mode.
For the same reason also the whole set of reg-exp for the lines of interest is required

```lisp
;; add *.el files location to your load paths
(add-to-list 'load-path "<path_to_Transcript_mode>/transcript")

(use-package transcript)
```

Features
--------

The tool logs analysis gets easy and time-saving with this mode, which features:

- Line of interest highlighting: the faces and regexp of the line of interests are fully
  customizable.
- Fast log section navigation: single-key navigation allows to quickly move around the line of
  interest.
- Automatically revert the visited log file.
- The buffer is opened in read-mode to ensure any accidental modification from the user.

### Highlighting Options ###

The user can define her/his own highlighting scheme to adapt to the specific tool's output being
inspected.
For instance the following snippet shows how a to define a profile for ModelSim <sup>&reg;</sup>:

```lisp
(use-package transcript
  :config
  (add-to-list 'transcript-profile-list
               (transcript-define-profile "modelsim"
                                          :error "^\\*\\{2\\}\\s-Error:.*$"
                                          :warning "^\\*\\{2\\}\\s-Warning:.*$")))
```
    
### Log File Navigation ###

    todo

      * Show the default key-bindings for moving around the log file
      * Add some gif to show the moving feature
