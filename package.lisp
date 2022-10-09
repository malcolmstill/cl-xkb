;;;; package.lisp

(defpackage :xkb
  (:use #:common-lisp #:cffi)
  (:export
    #:xkb-rule-names
    #:xkb-keycode
    #:xkb-keysym
    #:xkb-layout-index
    #:xkb-layout-mask
    #:xkb-level-index
    #:xkb-mod-index
    #:xkb-mod-mask
    #:xkb-led-index
    #:xkb-led-mask

    #:+xkb-keycode-invalid+
    #:+xkb-layout-invalid+
    #:+xkb-level-invalid+
    #:+xkb-mod-invalid+
    #:+xkb-led-invalid+
    #:+xkb-keycode-max+
    #:xkb-keycode-is-legal-ext-p
    #:xkb-keycode-is-legal-x11-p

    ;; Keysyms
    #:xkb-keysym-flags

    #:xkb-keysym-get-name
    #:xkb-keysym-from-name
    #:xkb-keysym-to-utf8
    #:xkb-keysym-to-utf32
    #:xkb-utf32-to-keysym
    #:xkb-keysym-to-upper
    #:xkb-keysym-to-lower

    ;; Library Context
    #:xkb-context-flags
    #:xkb-context-new
    #:xkb-context-ref
    #:xkb-context-unref
    #:xkb-context-set-user-data
    #:xkb-context-get-user-data
    #:xkb-context-user-data

    ;; Include Paths
    #:xkb-context-include-path-append
    #:xkb-context-include-path-append-default
    #:xkb-context-include-path-reset-defaults
    #:xkb-context-include-path-clear
    #:xkb-context-num-include-paths
    #:xkb-context-include-path-get

    ;; Logging Handling
    #:xkb-log-level

    #:xkb-context-set-log-level
    #:xkb-context-get-log-level
    #:xkb-context-log-level
    #:xkb-context-set-log-verbosity
    #:xkb-context-get-log-verbosity
    #:xkb-context-log-verbosity
    #:xkb-context-set-log-fn

    ;; Keymap Creation
    #:xkb-keymap-compile-flags
    #:xkb-keymap-format
    #:+xkb-keymap-use-original-format+

    #:xkb-keymap-new-from-names
    #:new-keymap-from-names
    #:xkb-keymap-new-from-file
    #:xkb-keymap-new-from-string
    #:xkb-keymap-new-from-buffer
    #:xkb-keymap-ref
    #:xkb-keymap-unref
    #:xkb-keymap-get-as-string

    ;; Keymap Components
    #:xkb-keymap-min-keycode
    #:xkb-keymap-max-keycode
    #:xkb-keymap-key-for-each
    #:xkb-keymap-key-get-name
    #:xkb-keymap-key-by-name
    #:xkb-keymap-num-mods
    #:xkb-keymap-mod-get-name
    #:xkb-keymap-mod-get-index
    #:xkb-keymap-num-layouts
    #:xkb-keymap-layout-get-name
    #:xkb-keymap-layout-get-index
    #:xkb-keymap-num-leds
    #:xkb-keymap-led-get-name
    #:xkb-keymap-led-get-index
    #:xkb-keymap-num-levels-for-key
    #:xkb-keymap-key-get-mods-for-level
    #:xkb-keymap-key-get-syms-by-level
    #:xkb-keymap-key-repeats

    ;; Keyboard State
    #:xkb-key-direction
    #:xkb-state-component
    #:xkb-state-match
    #:xkb-consumed-mode

    #:xkb-state-new
    #:xkb-state-ref
    #:xkb-state-unref
    #:xkb-state-get-keymap
    #:xkb-state-update-key
    #:xkb-state-update-mask
    #:xkb-state-key-get-syms
    #:xkb-state-key-get-utf8
    #:xkb-state-key-get-utf32
    #:xkb-state-key-get-one-sym
    #:xkb-state-key-get-layout
    #:xkb-state-key-get-level
    #:xkb-state-serialize-mods
    #:xkb-state-serialize-layout
    #:xkb-state-mod-name-is-active
    #:xkb-state-mod-names-are-active
    #:xkb-state-mod-index-is-active
    #:xkb-state-mod-indices-are-active
    #:xkb-state-key-get-consumed-mods2
    #:xkb-state-key-get-consumed-mods
    #:xkb-state-mod-index-is-consumed2
    #:xkb-state-mod-index-is-consumed
    #:xkb-state-mod-mask-remove-consumed
    #:xkb-state-layout-name-is-active
    #:xkb-state-layout-index-is-active
    #:xkb-state-led-name-is-active
    #:xkb-state-led-index-is-active

    ;; Compose and dead-keys support
    #:xkb-compose-compile-flags
    #:xkb-compose-format
    #:xkb-compose-state-flags
    #:xkb-compose-status
    #:xkb-compose-feed-result

    #:xkb-compose-table-new-from-locale
    #:xkb-compose-table-new-from-file
    #:xkb-compose-table-new-from-buffer
    #:xkb-compose-table-ref
    #:xkb-compose-table-unref

    #:xkb-compose-state-new
    #:xkb-compose-state-ref
    #:xkb-compose-state-unref
    #:xkb-compose-state-get-compose-table
    #:xkb-compose-state-feed
    #:xkb-compose-state-reset
    #:xkb-compose-state-get-status
    #:xkb-compose-state-get-utf8
    #:xkb-compose-state-get-one-sym

    ;; Not XKB but they do deal with keysyms
    ;; From ctypes.h
    #:toupper
    #:tolower
    ))
