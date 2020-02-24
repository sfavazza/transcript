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
- Extensible line of interest classes.

### Highlighting Options ###

    todo
    
### Log File Navigation ###

    todo
    
### Line of Interest Definition ###

As the no tools out there agree on a common standard to report the process of their tasks, the user
might need to define a `profile` according to the specific tool generating a log output.

    todo: show some example about how to define them.
    It should be a set of definition to run during the package initialization:
    
```lisp
(use-package transcript
    :init
    ;; profile for menthor modelsim
    
    ;; profile for intel quartus
    )
```

