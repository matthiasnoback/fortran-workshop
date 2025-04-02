# Debugging Fortran Code with Visual Studio Code

The debugger doesn't show the correct contents of variables. To fix this (yes, this is incredibly silly, I just read it [somewhere](https://gist.github.com/albertziegenhagel/6431811950864bd0009b6a1fa78e7f2b)), take the following steps.

- Open this folder, inside your oneAPI installation directory: C:\Program Files (x86)\Intel\oneAPI\debugger\latest\share\ide_support\visual_studio\debugger\vs2022
- Copy the file FEE_VSIX_v17.vsix to your home directory, e.g. C:\Users\noback
- Rename the file to FEE_VSIX_v17.zip, then right-click and `Extract all...`
- Open the extracted folder and create a file in this folder called `.vsdbg-config.json`
- Copy the following text into this file:

```json
{
  "$schema": "https://aka.ms/vs/vsdbg-config-schema",
  "languages": [
    {
      "languageId": "{8e546c6f-4819-4dde-83dd-f998d52e6f33}",
      "vendorId": "{2a333b19-f91e-477b-8032-22de549d925a}",
      "name": "Fortran",
      "codeViewCompilerIds": [ { "code": 2 } ]
    }
  ]
}
```

- Create a `.cppvsdbg\extensions` in your home directory (e.g. `C:\Users\noback\.cppvsdbg\extensions`).
- Create a file called `FEE.link` in this new directory. The contents should be the full path to the unzipped directory, e.g. `C:\Users\noback\FEE_VSIX_v17`.
- Next time you do a Debug launch, the Debug console panel should show:

```
------------------------------------------------------------------------------
You may only use the C/C++ Extension for Visual Studio Code with Visual Studio
Code, Visual Studio or Visual Studio for Mac software to help you develop and
test your applications.
------------------------------------------------------------------------------
Loading extensions from 'C:\Users\noback\FEE_VSIX_v17'.
[...]
```
