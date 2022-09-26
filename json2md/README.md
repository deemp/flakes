# json2md CLI

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
