;;;; package.lisp

(defpackage :xkb
  (:use :common-lisp :cffi)
  (:export
   xkb-rule-names
   xkb-context-new
   xkb-context-unref
   xkb-keymap-new-from-string
   xkb-keymap-new-from-names
   xkb-keymap-get-as-string
   new-keymap-from-names
   xkb-state-new
   xkb-state-key-get-utf8
   xkb-state-key-get-one-sym
   xkb-keysym-get-name
   get-keysym-name
   ;;xkb-keysym-from-name
   get-keysym-from-name
   ;;xkb-keysym-to-utf8
   get-keysym-character
   xkb-state-mod-name-is-active
   xkb-keymap-key-by-name
   xkb-keymap-min-keycode
   xkb-keymap-max-keycode
   xkb-keymap-unref
   xkb-state-update-key
   xkb-state-update-mask
   xkb-state-serialize-mods
   xkb-state-serialize-layout
   xkb-state-unref
   ;; Not XKB but they do deal with keysyms
   ;; From ctypes.h
   toupper
   tolower
   ))
