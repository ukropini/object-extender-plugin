# ObjectExtender PlugIn

ObjectExtender PlugIn for [PL/SQL Developerd](https://www.allroundautomations.com/products/pl-sql-developer/) is designed to simplify subtype creation.

## Installation

Copy **ObjectExtenderPlugIn.dll** and **ObjectExtenderPlugIn.sql** files (you can find them in [Compiled](Compiled) directory) to your PlugIn directory and restart PL/SQL Developer.

PL/SQL Developer PlugIn directory can be found in `Configure -> Preferences -> Files -> Directories -> Plug-Ins` in your PL/SQL Developer.

After installation you should see a new plugin in PlugIn Manager window (`Configure -> Plug-Ins`):
![plug-in-mng](img/plugin-mng.png)

You can configure a hot key (shortcut key) to trigger this PlugIn - go to `Configure -> Preferences -> User Interface -> Key Configuration`, find **Plug-Ins / Object Extender / Extend** function and setup a key:
![prefs](img/prefs.png)

## Usage

**Right click**:

![rightclick](img/right-click.gif)

**Shortcut key**:

![shortcut](img/shortkey.gif)

## Customization

You can change the logic of subtype creation by editing the **CREATE_NEW_TYPE_SCRIPT** procedure in [ObjectExtenderPlugIn.sql](Compiled/ObjectExtenderPlugIn.sql) file or you can completely refactor the file. There is only one requirement for this file - you must specify the **:type_owner** and **:type_name** SQL variables.

## Built With

* [Microsoft Visual Studio](https://visualstudio.microsoft.com/vs/)

## Authors

* **Heorhii Stalbovskyi** - *Initial work*

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details
