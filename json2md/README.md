# json2md CLI

## json2md

This is a CLI version of [json2md](https://github.com/IonicaBizau/json2md)

It reads `JSON` from a given file and in case of successful convertation ouputs `Markdown` to `stdout`

Sample usage:

```sh
nix shell .#
json2md docs.json
```

or

```sh
nix run .# docs.json
```

## nix2md

The function `nix2md` produces a script to translate a Nix expression to `md`.
Internally, this expressions is translated into a `JSON` file, and this file must abide the rules of `json2md`.
