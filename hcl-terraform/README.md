# Nix eDSL for Terraform HCL

Writing in Terraform [configuration syntax](https://www.terraform.io/language/syntax/configuration) often violates the [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) principle due to lack of named types.

This repo contains a `Nix` [eDSL](https://wiki.haskell.org/Embedded_domain_specific_language) which can be translated into HCL, a language that [Terraform](https://www.terraform.io/) uses. In other words, you may use the power of Nix to generate understandable HCL expressions without having to use the built-in Terraform [functions](https://stackoverflow.com/a/69134570).

## Contents

- [language definition](hcl.nix)
- [sample expressions](test-data.nix) used in tests
- [scripts](tf-tools.nix) to generate `.tf` files from given `Nix` expressions

## Run tests

Running the tests will write into `.tf` files the expressions generated from [test-data](test-data.nix)

```sh
nix run .#writeTests
```

## Limitations

There are some HCL constructs that aren't yet supported. To name a few:

- [Conditional expressions](https://developer.hashicorp.com/terraform/language/expressions/conditionals)
  - require making conditional accessors or merging accessors for objects in both options
- Possibly, some [built-in](https://developer.hashicorp.com/terraform/language/functions) functions
  - For now, they're all constructed using simple language constructs. It's highly likely (and I haven't yet checked) that some of them use more advanced constructs
