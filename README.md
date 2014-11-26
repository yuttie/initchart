# initchart

Visual profiling of Emacs init process.

See [this page](http://yuttie.github.io/initchart/) for examples.


## Usage

`initchart.el` provides macros and functions to measure and visualize a init
process of Emacs.

First, one need to record the execution time of some primary functions such
as `load` and `require`.  Use the macro `initchart-record-execution-time-of`
at the beginning of your `init.el` to register functions of concern, and then
launch an Emacs process as usual.  Execution time of the registered functions
will be logged in the `*initchart*` buffer.

Then, you can visualize these logs by invoking the command
`initchart-visualize-init-sequence`, which will ask you the filepath to save
an output SVG image.


## Configuration

```lisp
;; Load the library
(require 'initchart)

;; Measure the execution time of a specified function for every call.
;; Optionally, you might give a parameter name of the function you specified to
;; record what value is passed to the function.
(initchart-record-execution-time-of load file)
(initchart-record-execution-time-of require feature)
```
