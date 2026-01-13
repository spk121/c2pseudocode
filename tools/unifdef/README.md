# unifdef

This directory contains the `unifdef` utility by Tony Finch.

`unifdef` is a tool for removing `#ifdef`'ed lines from C source files. It's useful for preprocessing C files before converting them to pseudocode.

## Building

```bash
make
```

## Installing

```bash
sudo make install
```

This installs to `/usr/local/bin` by default. To install to a different location:

```bash
make install PREFIX=/path/to/install
```

## Usage

See `unifdef.1` for the full manual page, or:

```bash
man ./unifdef.1
```

Basic usage:

```bash
./unifdef -DMACRO file.c
```

## License

Copyright (c) 2002 - 2020 Tony Finch <dot@dotat.at>

See the source files for full license terms (BSD-style license).
