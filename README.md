# Getting started

Clone this project to your computer using Git (`git clone [url of repository]`). Then pick an option (or do both if you like):

## Option 1: Visual Studio

This option requires less work to set up if you already work with the `delft3d` repository.

See the [README for Visual Studio](docs/README-Visual-Studio.md).

## Option 2: Visual Studio Code with CMake

This option has much better support for working with Fortran, but may be less familiar.

See the [README for Visual Studio Code with CMake](docs/README-VS-Code-CMake.md).

## Option 3: Visual Studio Code with FPM

A cool, modern option is to use the official Fortran Package Manager (FPM) instead of CMake. This requires some more work at the command-line.

See the [README for Visual Studio Code with FPM](docs/README-VS-Code-FPM.md).

## Profiling with Valgrind and perf running in Docker

You'll find a number of useful scripts in the `bin` folder. They are all based on the IFX compiler and FPM. Everything you need for this is provided in a Docker container, which you have to build once.

### Preparation

- If you're on Windows, install WSL with for example the Ubuntu distribution. Also install Docker Desktop and enable WSL integration for your WSL distribution. This allows you to use `docker` inside a WSL distribution.
- If you're on Windows, install docker

### Building the container

```bash
docker compose build
```

### Running an executable in release mode

Use `bin/fast` to compile and run an FPM executable with the `release` profile, which includes many optimizations:

```bash
bin/fast hello_world
```

### Checking memory use of an FPM executable

```bash
bin/memory hello_world
```

### Finding (potential) memory leaks in an FPM executable

```bash
bin/leaks hello_world
```

## Analyzing function performance with perf

```bash
bin/perf hello_world
```

This allows you to browse through function calls and find out how much time was spent by the function itself and its "children".
