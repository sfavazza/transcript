# Transcript Mode #

Emacs mode for handy log file analysis. It is useful to inspect usually text based tools output log files.
This mode offers line highlighting of the lines-of-interest (loi) like: `fatal`, `errors`,
`warnings`, `critical, warnings`, `note`.

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

As the buffer is read-only, it is possible to define single key functions. The user can move around
the line-of-interests (loi) in forward direction using a lower-case letter and/or in backward
direction using an upper-case letter.

The letters corresponds to the first one of the `loi` keywords (`fatal`, `errors`, `warnings`,
`critical, warnings`, `note`).

For instance `e` calls `transcript-next-error`, `F` calls `transcript-previous-fatal` and so on...

    todo

        * Add some gif to show the moving feature
