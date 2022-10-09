
# cl-xkb

`cl-xkb` is a Common Lisp wrapper for libxkbcommon. libxkbcommon is library for handling keyboard descriptions and their state.

## Status

`cl-xkb` started development to support [ulubis](https://github.com/malcolmstill/ulubis). The library currently supports these xkb [modules](https://xkbcommon.org/doc/current/modules.html):

- [Keysyms](https://xkbcommon.org/doc/current/group__keysyms.html)
- [Library Context](https://xkbcommon.org/doc/current/group__context.html)
- [Include Paths](https://xkbcommon.org/doc/current/group__include-path.html)
- [Logging Handling](https://xkbcommon.org/doc/current/group__logging.html)
- [Keymap Creation](https://xkbcommon.org/doc/current/group__keymap.html)
- [Keymap Components](https://xkbcommon.org/doc/current/group__components.html)
- [Keyboard State](https://xkbcommon.org/doc/current/group__state.html)
- [Compose and dead-keys support](https://xkbcommon.org/doc/current/group__compose.html)

Pull requests adding more of the API are more than welcome.

## Requirements

`cl-xkb` requires libxkbcommon and CFFI. It may require development files from your distribution -- the package should usually be called something like `libxkbcommon-dev`

## Installation

```
CL-USER> (ql:quickload :cl-xkb)
```
