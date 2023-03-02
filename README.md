# haskells

This is a simple implementation of the `ls` command in Haskell. It lists the contents of a directory, including file names, with colors and icons based on file type.

## Features

- Lists files and directories in a specified directory.
- Colors file names based on their file type.
- Adds icons to file names based on their file type.

## Usage

To use the ls command, open a terminal and navigate to the directory where the ls executable is located. You can then run the command with the following syntax:

```console
./haskells [PATH] [OPTIONS]
```

Where `PATH` is the optional path to the directory you want to list. If path is not specified, the current directory will be listed.

## Options

- `-a`, `--all`: Display all files, including hidden files.
- `-v`, `--version`: Print the version number.
- `-h`, `--help`: Print a help message with a list of available options.

## Examples

List the contents of the current directory:

```console
./haskells
```

List the contents of a specified directory:

```console
./haskells /path/to/directory

```

List all files in a directory:

```console
./haskells -a /path/to/directory
```

Display the version number:

```console
./haskells --version
```

Display the help message:

```console
./haskells --help
```

## Compiling the Program

To compile the program, you need to have the GHC (Glasgow Haskell Compiler) command-line tool installed on your system. You can then run the following command in the terminal:

```console
ghc haskells.hs
```

This will produce an executable file named `haskells` in the same directory as `haskells.hs`.
