Learn You Haskell
=================

Repository to store the excercises of the book
[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters).


## Code Organization

All the modules that have dependencies additional to the `base` package can be found in the `src`
folder and they compile using standard [Cabal](https://www.haskell.org/cabal/) commands:

```shell
cabal clean
cabal build
cabal run
```

The rest of modules are organized in their own feature folders.


## VS Code Setup

Coding Haskell in VS Code was nicer with the help of the following plugins:
* [Haskell Syntax Highlighting](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell)
* [Simple GHC (Haskell) Integration for VSCode](https://marketplace.visualstudio.com/items?itemName=dramforever.vscode-ghc-simple)

> Be aware that the *inline REPL offered by Simple GHC* doesn't support two `Main` modules opened
> at the same time in the editor. If the REPL is throwing a *main module error* just make sure that
> only one `Main` module is open in the editor.
