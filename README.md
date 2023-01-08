![](https://i.imgur.com/HCtqmyB.png)

## look at that `sweet logo`
teardrop is a thing to make "ansi" art in your terminal. its not compatible with demoscene "ansi art" that ends in .ans, more like its a text art maker for use with any "ansi standard compliable" terminal. which is just ECMA-48 plus ISO 8613-6 plus whatever xterm does

## keybindings as of v0.0-a
- q e z c w a s d : move cursor
- i o k l : color selecting
- h j y u : brush selecting
- space : paint
- shift-s : save
- shift-l : load

## compiling from source
in order to compile this thing, youre gonna need some haskell tools. use GHCup to install ghc-9.2.1 and cabal-3.4.0.0 or anything higher.

then:
- git clone https://github.com/nikshalark/teardrop.git
- cd teardrop
- cabal build
- cabal run

if this doesnt work, please open an issue immediately
