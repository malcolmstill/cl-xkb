
# cl-xkb

`cl-xkb` is a Common Lisp wrapper for libxkbcommon. libxkbcommon is library for handling keyboard descriptions and their state.

## Status

`cl-xkb` is being developed primarily in support of [ulubis](https://github.com/malcolmstill/ulubis) and is therefor feature incomplete. Pull requests adding more of the API are more than welcome.

## Requiremnts

`cl-xkb` requires libxkbcommon and cffi. It is likely that libxkbcommon already exists on your Linux installation if it is recent.

## Installation

```
CL-USER> (ql:quickload :cl-xkb)
```
