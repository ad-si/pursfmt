# Pursfmt

Configurable syntax formatter for PureScript.


## Install

```sh
npm install --save-dev pursfmt
```

Do not install it globally, so that different PureScript projects can use
different versions of `pursfmt`.

Also available for [Nix](https://nixos.org/) via
[Nixpkgs 22.11+](https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=pursfmt)
and [Easy PureScript Nix](https://github.com/justinwoo/easy-purescript-nix)


## Usage

You can use `pursfmt` to format files in place or via stdin / stdout
(which is useful for editor integration):


##### Formatting a collection of files in place:

```sh
pursfmt format-in-place "src/**/*.purs"
```


##### Using STDIN to format a file:

```sh
pursfmt format < MyFile.purs
```

You can also use `pursfmt` to verify whether files have already been formatted.
This is often useful to verify, in continuous integration,
that all project files are formatted according to the configuration.
Files that would be changed by running `format-in-place` are listed out.



##### Verifying files are formatted

```sh
pursfmt check "src/**/*.purs"
All files are formatted.
```


### Configuration

You can see all configuration that `pursfmt` accepts using the `--help` flag
for the command you are using:

```sh
pursfmt format-in-place --help
```

Some common options include:

- `--indent` to set the number of spaces used in indentation,
    which defaults to 2 spaces
- `--arrow-first` or `--arrow-last` to control whether type signatures
    put arrows first on the line or last on the line (purty-style),
    which defaults to arrow-first.

You can generate a `.pursfmt.yaml` using the `generate-config` command.
For example:

```sh
pursfmt generate-config --indent 2 --width 80 --arrow-last
```

If a `.pursfmt.yaml` file is found, it will be used in lieu of CLI arguments.


### Operator Precedence

To support correct operator precedence without having to parse your entire
source tree (potentially for a single file), `pursfmt` uses a pre-baked
operator precedence table. By default, `pursfmt` ships with a table built
from the core and contrib organizations. If you need support for more
operators, you can generate your own table using the `generate-operators`
command.

```sh
spago sources | xargs pursfmt generate-operators > .pursfmtoperators
$ pursfmt generate-config --arrow-first --unicode-never --operators .pursfmtoperators
```


## Editor Support

- [Spacemacs](#spacemacs)
- [Vim](#vim)
- [VS Code](#vs-code)


### Spacemacs

[Spacemacs' Purescript layer](https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Blang/purescript)
supports formatting using pursfmt out of the box.

You can run the formatter manually with either `M-x spacemacs/purescript-format`
or with the shortcut `SPC m =`.

To enable automatic formatting of the buffer on save,
enable `purescript-fmt-on-save` in your spacemacs config:

```elisp
  (setq-default dotspacemacs-configuration-layers '(
    (purescript :variables
                purescript-fmt-on-save t)))
```


### Vim

#### via [ALE](https://github.com/dense-analysis/ale)

Add to your other fixers `.vimrc` or `$XDG_CONFIG_HOME/neovim/init.vim`

```viml
let b:ale_fixers = { 'purescript': [ 'pursfmt' ] }
" suggested to fix on save
let g:ale_fix_on_save = 1
```


#### via [Neoformat](https://github.com/sbdchd/neoformat)

Add to your `.vimrc` or `$XDG_CONFIG_HOME/neovim/init.vim`

```viml
let g:neoformat_enabled_purescript = ['pursfmt']
```


### VS Code

#### via [PureScript IDE](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript)

The PureScript IDE plugin for VS Code supports `pursfmt` as a built-in formatter in versions after `0.25.1`. Choose `pursfmt` from the list of supported formatters in the settings, or add this to your `settings.json`:

```json
"purescript.formatter": "pursfmt"
```


## Development

### Running `bin`

For local development pointing to the `output` directory:

```sh
npm run build
$ ./bin/index.dev.js --help
```

For a local production build pointing to the `bundle` directory:

```sh
npm run bundle
$ ./bin/index.js --help
```

If you would like to use your local build of `pursfmt` in your editor,
use path to `bin/index.js` instead of the `pursfmt` binary in your settings.
For example, instead of setting the format command to `pursfmt format`,
set it to `$PURSFMT_DIR/bin/index.js format` where `$PURSFMT_DIR` is the location
of your checkout of this repository.


### Running `test`

To accept snapshot tests:

```sh
npm run test -- -a "--accept"
```


### Generating the built-in operator table

```sh
npm run generate-default-operators
```


### Release

To release a new version of `pursfmt`, run the following commands

1. `npm version minor`
1. `npm publish`
1. `npx spago publish --package pursfmt`
1. `git push --tags`


## History

This project was originally forked from
[purescript-tidy](https://github.com/natefaubion/purescript-tidy).
