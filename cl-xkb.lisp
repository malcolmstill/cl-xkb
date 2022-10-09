
(in-package :xkb)

(define-foreign-library xkb
  (:unix (:or "/usr/lib64/libxkbcommon.so.0" "/usr/lib64/libxkbcommon.so" "/usr/lib/x86_64-linux-gnu/libxkbcommon.so"))
  (t (:default "libxkbcommon")))

(use-foreign-library xkb)

(defmacro %definline (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))

(defmacro %define-ref-and-unref (prefix param-name)
  (let ((prefix* (eval prefix)))
    `(progn
       (defcfun ,(concatenate 'string prefix* "_ref") :pointer
         (,param-name :pointer))
       (defcfun ,(concatenate 'string prefix* "_unref") :void
         (,param-name :pointer)))))

;; Define an accessor out of a pair of getter/setter bindings, to
;; enable duality of syntax for lisp users.
(defmacro %define-accessor (name reader-name writer-name (&rest args))
  `(progn
     (declaim (inline ,name (setf ,name)))
     (defun ,name ,(butlast args)
       (,reader-name ,@(butlast args)))
     (defun (setf ,name) ,(cons (car (last args)) (butlast args))
       (,writer-name ,@args))))



;; Ubiquitous Types, Constants

(defcstruct xkb-rule-names
  (rules :string)
  (model :string)
  (layout :string)
  (variant :string)
  (options :string))

(defctype xkb-keycode :uint32)
(defctype xkb-keysym :uint32)
(defctype xkb-layout-index :uint32)
(defctype xkb-layout-mask :uint32)
(defctype xkb-level-index :uint32)
(defctype xkb-mod-index :uint32)
(defctype xkb-mod-mask :uint32)
(defctype xkb-led-index :uint32)
(defctype xkb-led-mask :uint32)

(defconstant +xkb-keycode-invalid+ #xffffffff)
(defconstant +xkb-layout-invalid+ #xffffffff)
(defconstant +xkb-level-invalid+ #xffffffff)
(defconstant +xkb-mod-invalid+ #xffffffff)
(defconstant +xkb-led-invalid+ #xffffffff)
(defconstant +xkb-keycode-max+ (1- #xffffffff))

(%definline xkb-keycode-is-legal-ext-p (key)
  (<= key +xkb-keycode-max+))
(%definline xkb-keycode-is-legal-x11-p (key)
  (and (>= key 8) (<= key 255)))



;; Keysyms

(defbitfield xkb-keysym-flags
  (:no-flags 0)
  :case-insensitive)

(defcfun "xkb_keysym_get_name" :int
  (keysym xkb-keysym-flags)
  (buffer :string)
  (size :size))

(defun get-keysym-name (keysym)
  (with-foreign-pointer-as-string (cstr 64)
    (xkb-keysym-get-name keysym cstr 64)))

(defcfun "xkb_keysym_from_name" xkb-keysym
  (name :string)
  (flags xkb-keysym-flags))

(defun get-keysym-from-name (name &key case-insensitive)
  (with-foreign-string (cname name)
    (xkb-keysym-from-name cname (when case-insensitive '(:case-insensitive)))))

(defcfun "xkb_keysym_to_utf8" :int
  (keysym xkb-keysym)
  (buffer :string)
  (size :size))

(defun get-keysym-character (keysym)
  (char (with-foreign-pointer-as-string (char 16)
          (xkb-keysym-to-utf8 keysym char 16))
        0))

(defcfun "xkb_keysym_to_utf32" :uint32
  (keysym xkb-keysym))

(defcfun "xkb_utf32_to_keysym" xkb-keysym
  (ucs :uint32))

(defcfun "xkb_keysym_to_upper" xkb-keysym
  (ks xkb-keysym))

(defcfun "xkb_keysym_to_lower" xkb-keysym
  (ks xkb-keysym))



;; Library Context

(defbitfield xkb-context-flags
  (:no-flags 0)
  no-default-includes
  no-environment-names)

(defcfun "xkb_context_new" :pointer
  (flags xkb-context-flags))

(%define-ref-and-unref "xkb_context" context)

(defcfun "xkb_context_set_user_data" :void
  (context :pointer)
  (user-data :pointer))

(defcfun "xkb_context_get_user_data" :pointer
  (context :pointer))

(%define-accessor xkb-context-user-data
                  xkb-context-get-user-data
                  xkb-context-set-user-data
                  (context user-data))



;; Include Paths

(defcfun "xkb_context_include_path_append" :int
  (context :pointer)
  (path :string))

(defcfun "xkb_context_include_path_append_default" :int
  (context :pointer))

(defcfun "xkb_context_include_path_reset_defaults" :int
  (context :pointer))

(defcfun "xkb_context_include_path_clear" :void
  (context :pointer))

(defcfun "xkb_context_num_include_paths" :uint
  (context :pointer))

(defcfun "xkb_context_include_path_get" :string
  (context :pointer)
  (index :uint))



;; Logging Handling

(defcenum xkb-log-level
  (:critical 10)
  (:error 20)
  (:warning 30)
  (:info 40)
  (:debug 50))

(defcfun "xkb_context_set_log_level" :void
  (context :pointer)
  (level xkb-log-level))

(defcfun "xkb_context_get_log_level" xkb-log-level
  (context :pointer))

(%define-accessor xkb-context-log-level
                  xkb-context-get-log-level
                  xkb-context-set-log-level
                  (context level))

(defcfun "xkb_context_set_log_verbosity" :void
  (context :pointer)
  (verbosity :int))

(defcfun "xkb_context_get_log_verbosity" :int
  (context :pointer))

(%define-accessor xkb-context-log-verbosity
                  xkb-context-get-log-verbosity
                  xkb-context-set-log-verbosity
                  (context verbosity))

(defcfun "xkb_context_set_log_fn" :void
  (context :pointer)
  (log-fn :pointer))



;; Keymap Creation

(defbitfield xkb-keymap-compile-flags
  (:no-flags 0))

(defcenum xkb-keymap-format
  (:text-v1 1))

(defconstant +xkb-keymap-use-original-format+
             (1- (ash 1 (* 8 (cffi:foreign-type-size 'xkb-keymap-format)))))

(defcfun "xkb_keymap_new_from_names" :pointer
  (context :pointer)
  (names (:pointer (:struct xkb-rule-names)))
  (flags xkb-keymap-compile-flags))

(defun new-keymap-from-names (ctx rules model layout variant options)
  (with-foreign-object (names '(:struct xkb-rule-names))
    (with-foreign-strings ((rules-ptr rules)
                           (model-ptr model)
                           (layout-ptr layout)
                           (variant-ptr variant)
                           (options-ptr options))
      (setf (foreign-slot-value names '(:struct xkb-rule-names) 'rules) rules-ptr
            (foreign-slot-value names '(:struct xkb-rule-names) 'model) model-ptr
            (foreign-slot-value names '(:struct xkb-rule-names) 'layout) layout-ptr
            (foreign-slot-value names '(:struct xkb-rule-names) 'variant) variant-ptr
            (foreign-slot-value names '(:struct xkb-rule-names) 'options) options-ptr)

      (xkb-keymap-new-from-names ctx names ()))))

(defcfun "xkb_keymap_new_from_file" :pointer
  (context :pointer)
  (file :pointer)
  (format xkb-keymap-format)
  (flags xkb-keymap-compile-flags))

(defcfun "xkb_keymap_new_from_string" :pointer
  (context :pointer)
  (string :string)
  (format xkb-keymap-format)
  (flags xkb-keymap-compile-flags))

(defcfun "xkb_keymap_new_from_buffer" :pointer
  (context :pointer)
  (buffer :string)
  (length :size)
  (format xkb-keymap-format)
  (flags xkb-keymap-compile-flags))

(%define-ref-and-unref "xkb_keymap" keymap)

;; TODO define xkb_keymap_get_as_string, whose result must be foreign-free'd.
;; (defcfun "xkb_keymap_get_as_string" :string
;;   (keymap :pointer)
;;   (format xkb-keymap-format))



;; Keymap Components

(defcfun "xkb_keymap_min_keycode" xkb-keycode
  (keymap :pointer))

(defcfun "xkb_keymap_max_keycode" xkb-keycode
  (keymap :pointer))

(defcfun "xkb_keymap_key_for_each" :void
  (keymap :pointer)
  (iter :pointer)
  (data :pointer))

(defcfun "xkb_keymap_key_get_name" :string
  (keymap :pointer)
  (key xkb-keycode))

(defcfun "xkb_keymap_key_by_name" xkb-keycode
  (keymap :pointer)
  (name :string))

(defcfun "xkb_keymap_num_mods" xkb-mod-index
  (keymap :pointer))

(defcfun "xkb_keymap_mod_get_name" :string
  (keymap :pointer)
  (idx xkb-mod-index))

(defcfun "xkb_keymap_mod_get_index" xkb-mod-index
  (keymap :pointer)
  (name :string))

(defcfun "xkb_keymap_num_layouts" xkb-layout-index
  (keymap :pointer))

(defcfun "xkb_keymap_layout_get_name" :string
  (keymap :pointer)
  (idx xkb-layout-index))

(defcfun "xkb_keymap_layout_get_index" xkb-layout-index
  (keymap :pointer)
  (name :string))

(defcfun "xkb_keymap_num_leds" xkb-led-index
  (keymap :pointer))

(defcfun "xkb_keymap_led_get_name" :string
  (keymap :pointer)
  (idx xkb-led-index))

(defcfun "xkb_keymap_led_get_index" xkb-led-index
  (keymap :pointer)
  (name :string))

(defcfun "xkb_keymap_num_levels_for_key" xkb-level-index
  (keymap :pointer)
  (key xkb-keycode)
  (layout xkb-layout-index))

(defcfun "xkb_keymap_key_get_mods_for_level" :size
  (keymap :pointer)
  (key xkb-keycode)
  (layout xkb-layout-index)
  (level xkb-level-index)
  (masks-out (:pointer xkb-mod-mask))
  (masks-size :size))

(defcfun "xkb_keymap_key_get_syms_by_level" :int
  (keymap :pointer)
  (key xkb-keycode)
  (layout xkb-layout-index)
  (level xkb-level-index)
  (syms-out (:pointer (:pointer xkb-keysym))))

(defcfun "xkb_keymap_key_repeats" :int
  (keymap :pointer)
  (key xkb-keycode))



;; Keyboard State

(defcenum xkb-key-direction
  :up
  :down)

(defbitfield xkb-state-component
  :mods-depressed
  :mods-latched
  :mods-locked
  :mods-effective
  :layout-depressed
  :layout-latched
  :layout-locked
  :layout-effective
  :leds)

(defbitfield xkb-state-match
  :any
  :all
  (:non-exclusive #.(ash 1 16)))

(defcenum xkb-consumed-mode
  :xkb :gtk)

(defcfun "xkb_state_new" :pointer
  (keymap :pointer))

(%define-ref-and-unref "xkb_state" state)

(defcfun "xkb_state_get_keymap" :pointer
  (state :pointer))

(defcfun "xkb_state_update_key" xkb-state-component
  (state :pointer)
  (key xkb-keycode)
  (direction xkb-key-direction))

(defcfun "xkb_state_update_mask" xkb-state-component
  (state :pointer)
  (depressed-mods xkb-mod-mask)
  (latched-mods xkb-mod-mask)
  (locked-mods xkb-mod-mask)
  (depressed-layout xkb-layout-index)
  (latched-layout xkb-layout-index)
  (locked-layout xkb-layout-index))

(defcfun "xkb_state_key_get_syms" :int
  (state :pointer)
  (key xkb-keycode)
  (syms-out (:pointer (:pointer xkb-keysym))))

(defcfun "xkb_state_key_get_utf8" :int
  (state :pointer)
  (key xkb-keycode)
  (buffer :string)
  (size :size))

(defcfun "xkb_state_key_get_utf32" :uint32
  (state :pointer)
  (key xkb-keycode))

(defcfun "xkb_state_key_get_one_sym" xkb-keysym
  (state :pointer)
  (key xkb-keycode))

(defcfun "xkb_state_key_get_layout" xkb-layout-index
  (state :pointer)
  (key xkb-keycode))

(defcfun "xkb_state_key_get_level" xkb-level-index
  (state :pointer)
  (key xkb-keycode)
  (layout xkb-layout-index))

(defcfun "xkb_state_serialize_mods" xkb-mod-mask
  (state :pointer)
  (components xkb-state-component))

(defcfun "xkb_state_serialize_layout" xkb-layout-mask
  (state :pointer)
  (components xkb-state-component))

(defcfun "xkb_state_mod_name_is_active" :int
  (state :pointer)
  (name :string)
  (type xkb-state-component))

(defcfun "xkb_state_mod_names_are_active" :int
  (state :pointer)
  (type xkb-state-component)
  (match xkb-state-match)
  &rest)

(defcfun "xkb_state_mod_index_is_active" :int
  (state :pointer)
  (idx xkb-mod-index)
  (type xkb-state-component))

(defcfun "xkb_state_mod_indices_are_active" :int
  (state :pointer)
  (type xkb-state-component)
  (match xkb-state-match)
  &rest)

(defcfun "xkb_state_key_get_consumed_mods2" xkb-mod-mask
  (staet :pointer)
  (key xkb-keycode)
  (mode xkb-consumed-mode))

(defcfun "xkb_state_key_get_consumed_mods" xkb-mod-mask
  (state :pointer)
  (key xkb-keycode))

(defcfun "xkb_state_mod_index_is_consumed2" :int
  (state :pointer)
  (key xkb-keycode)
  (idx xkb-mod-index)
  (mode xkb-consumed-mode))

(defcfun "xkb_state_mod_index_is_consumed" :int
  (state :pointer)
  (key xkb-keycode)
  (idx xkb-mod-index))

(defcfun "xkb_state_mod_mask_remove_consumed" xkb-mod-mask
  (state :pointer)
  (key xkb-keycode)
  (mask xkb-mod-mask))

(defcfun "xkb_state_layout_name_is_active" :int
  (state :pointer)
  (name :string)
  (type xkb-state-component))

(defcfun "xkb_state_layout_index_is_active" :int
  (state :pointer)
  (idx xkb-layout-index)
  (type xkb-state-component))

(defcfun "xkb_state_led_name_is_active" :int
  (state :pointer)
  (name :string))

(defcfun "xkb_state_led_index_is_active" :int
  (state :pointer)
  (idx xkb-led-index))



;; Compose and dead-keys support

(defbitfield xkb-compose-compile-flags
  (:no-flags 0))

(defcenum xkb-compose-format
  (:text-v1 1))

(defbitfield xkb-compose-state-flags
  (:no-flags 0))

(defcenum xkb-compose-status
  :nothing
  :composing
  :composed
  :cancelled)

(defcenum xkb-compose-feed-result
  :ignored
  :accepted)

(defcfun "xkb_compose_table_new_from_locale" :pointer
  (context :pointer)
  (locale :String)
  (flags xkb-compose-compile-flags))

(defcfun "xkb_compose_table_new_from_file" :pointer
  (context :pointer)
  (file :pointer)
  (locale :string)
  (format xkb-compose-format)
  (flags xkb-compose-compile-flags))

(defcfun "xkb_compose_table_new_from_buffer" :pointer
  (context :pointer)
  (buffer :string)
  (length :size)
  (locale :string)
  (format xkb-compose-format)
  (flags xkb-compose-compile-flags))

(%define-ref-and-unref "xkb_compose_table" table)

(defcfun "xkb_compose_state_new" :pointer
  (table :pointer)
  (flags xkb-compose-state-flags))

(%define-ref-and-unref "xkb_compose_state" state)

(defcfun "xkb_compose_state_get_compose_table" :pointer
  (state :pointer))

(defcfun "xkb_compose_state_feed" xkb-compose-feed-result
  (state :pointer)
  (keysym xkb-keysym))

(defcfun "xkb_compose_state_reset" :void
  (state :pointer))

(defcfun "xkb_compose_state_get_status" xkb-compose-status
  (state :pointer))

(defcfun ("xkb_compose_state_get_utf8" %xkb-compose-state-get-utf8) :int
  (state :pointer)
  (buffer :string)
  (size :size))

(%define-utf8-string-reader
  xkb-compose-state-get-utf8 %xkb-compose-state-get-utf8 (state))

(defcfun "xkb_compose_state_get_one_sym" xkb-keysym
  (state :pointer))



;;; Not XKB but they do deal with keysyms
;;; From ctypes.h

(defcfun "toupper" :int
  (c :int))

(defcfun "tolower" :int
  (c :int))
