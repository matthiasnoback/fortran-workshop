# Configuring the Modern Fortran extension

The Modern Fortran extension requires the `fortls` language server, which comes installed with this project as a Python executable. Go to `File` - `Preferences` - `Settings` - `Extensions` - `Modern Fortran` - `Language server`. Make sure to select the `User` tab, not the `Workspace` tab. Then for the setting `Fortran > Fortls: Path` provide the full path to `fortls.exe`, e.g.

```
C:\Users\[location of the repository]\venv\Scripts\fortls.exe
```

(Unfortunately it doesn't work with a relative path)

`fprettify` will be executed on save, and to ensure that the Modern Fortran doesn't try to reformat the code too, we disable formatting under `File` - `Preferences` - `Settings` - `Extensions` - `Modern Fortran` - `Formatting`.
