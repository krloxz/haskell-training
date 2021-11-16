File Manager
============

How to Install GTK
------------------

```bash
sudo apt install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev build-essential libgtk-3-dev libghc-glib-dev
```
### Resources
https://www.howtoinstall.me/ubuntu/18-04/libghc-gtk-dev/
https://itectec.com/ubuntu/ubuntu-pkg-config-not-finding-gtk-3-0/


How to Run
----------

This project has multiple executables and Cabal will need to know what executable to run, for example:
```sh
cabal run TreeStore
```

Also the plugin Simple GHC Integration won't work in VSCode unless an executable is picked, just create the file `<project-path>/.vscode/settings.json` and enter the Cabal command:
```json
{
  "ghcSimple.replCommand": "cabal repl TreeStore"
}
```
