(load "punter.asd")
(ql:quickload 'punter)
(sb-ext:save-lisp-and-die "zebiekste.out" :executable t :save-runtime-options t :toplevel #'punter/core::main)
