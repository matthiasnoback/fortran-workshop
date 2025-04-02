# README for VS Code

## Setup

Requirements:

- For Windows: Visual Studio 2022, with the "Desktop development with C++" installed and Intel oneAPI Fortran Essentials [Download here](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler-download.html)
- For Linux: CMake >= 2.30 and Intel oneAPI Fortran Essentials  [Download here](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler-download.html)
- Make sure the oneAPI bin folder is in your `PATH` environment variable (e.g. `C:\Program Files (x86)\Intel\oneAPI\compiler\latest\bin`)
- Python 3 [Download here](https://www.python.org/downloads/)
- Visual Studio Code IDE [Download here](https://code.visualstudio.com/download).

Start VS Code, and open the folder where you've cloned this project to.

Open the `Extensions` panel on the left (or press `Ctrl + Shift + X`). Then install the following extensions:

- Cmake Tools (provides a UI for interacting with CMake targets)
- Modern Fortran (understands Fortran code and gives code completion based on input from the Fortran Language Server, etc., runs the compiler as a linter)
- Trigger Task on Save (allows auto-formatting with fprettify and running the Fortitude linter on-save)
- EditorConfig for VS Code (automatically deals with whitespacing)
- CTest Lab (integrates CTests with the Test panel so you can run tests in your IDE)

Optional:

- CMake IntelliSense (formatting, auto-complete in CMake configuration)
- Python (helps with writing Python code for the output visualization scripts)

CMake Tools will ask for a "CMake configure preset". Select:

- On Windows, with Visual Studio installed: pick "ifx".
- On Linux, with Intel oneAPI Fortran Essentials installed: pick "linux_make_ifx

## Building and running

From the CMake panel (you'll find an icon on the left of the screen), you can select any of the project's targets that you want to build, run, or debug. The menu (`...`) shows specific actions like Reconfiguring, Rebuilding, Cleaning, etc.

Try, for example, to run `fortran-workshop-tester`. Right-click on the target and press `Set as Launch/Debug Target`. Then press the debug or play icon in the bar at the bottom of the screen.

### Debugging (optional)

Unfortunately, debugging doesn't work out-of-the-box with CMake tools. To Debug, make sure to select a Launch target in the CMake panel on the left. Then choose `Run` -> `Start Debugging`.

To improve the debugger's understanding of Fortran code and application memory, follow [these instructions](VS-Code-Fortran-Debugging.md).

## Fortitude linter

Fortitude will check the file immediately after saving it. Any issues will be reported in the file itself, but also in the `Problems` panel.

To check all files at once, run inside a terminal:

```bash
.\venv\Scripts\fortitude.exe check --fix -- src test
```

## Configuring the Modern Fortran extension

The Modern Fortran extension needs some [manual configuration steps](VS-Code-Modern-Fortran.md).
