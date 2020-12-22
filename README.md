# HI

`hi` is a generic project initialization tool that is written in Haskell. It is similar to cookiecutter in functionality.

## Installation

Currently it's only possible to compile `hi` from source.

### cabal

After cloning this repository, run:

```
$ cabal install hi
```

### nix

To build `hi`, run:

```
$ nix build github:poscat0x04/HI
```

After that, you could install hi to your user profile by running:

```
nix-env -i ./result ./result-data
```

If you instead want to install `hi` as a sysetm package, you could make this repository a input of your flake and add `defaultPackage` to `systemPackages`.

## Usage

1. Initialize a project

```
$ hi init haskell my-project
```

which creates a project with the following directory structure:

```
my-project
├── cabal.project
├── CHANGELOG.md
├── hie.yaml
├── LICENSE
├── my-project.cabal
├── README.md
├── src
└── test
    └── Spec.hs
```

2. List all templates available

```
$ hi list
```

which outputs:

```
- local templates
  • haskell
```

## Configuration

### Global configuration

Global configuration is read from `$XDG_CONFIG_HOME/hi/config.toml`. If this file is not present on your system, `hi` will ask for user inputs so that a minimal configuration can be constructed.

Top-level configuration options:

- `name :: string`\
  required, your full name, used in `LICENSE` files and some default templates
- `github_username :: string`\
  required, your github username, used in some default templates
- `email :: string`\
  required, your email, used in some default templates
- `license :: string`\
  optional, the default license, must be a valid [SPDX license identifier](https://spdx.org/licenses/).
- `vcs :: string`\
  optional, the version control tool, can by any string but `Git`, `Darcs`, `Mercurial`, `Pijul` has special meanings (`hi` will call these vcs tools after the project has been initialized)

Addtionaly, default values of variables can be set under the table `[custom]`, note that the value must be either a bool or a piece of text.

## Templating

Unlike cookiecutter, hi uses [mustache](https://mustache.github.io/), a much simpler templating language for writing project templates. User templates are placed under `$XDG_DATA_HOME`.

### Template configuration

Every template is required to have a config file `template.toml` and here are its configuration options:

- `desc :: string`\
  optional, description for the template.
- `ignores :: [string]`\
  required, ignored files, can use [globbing syntax](https://hackage.haskell.org/package/Glob-0.10.1/docs/System-FilePath-Glob.html#v:compile). Note that when judging whether a file should be ignored, it is the file's path relative the the root of the template that will be matched against the patterns specified here, not just the file name.
- `tags :: [string]`\
  required, tags (duh), currently this is not being used by any command.
- `options :: [array of tables](https://toml.io/en/v1.0.0-rc.3#array-of-tables)`\
  parameters of the template.\
  suboptions:
    - `name :: string`\
      required, the name of the option.
    - `desc :: string`\
      optional, the description for this option.
    - `type :: string`\
      rquired if `default` is not set, otherwise it will be ignored completely, the type of this option, currently only supports `"bool"` and `"text"`.
    - `default :: string | bool`\
      optional, the default value for this option, if this is not set then the program will ask for user input when initializing a project from this template
- `optionals :: array of tables`\
  optionally ignored files, when the expression specified by `when` evaluates to true, the `ignores` field will be merged into the top-level ignored files.\
  suboptions:
    - `when :: string`\
      required, an arbitrary boolean expression constructed from variables (must contain only alphanumeric characters), negations (`!` or `¬`), conjunctions (`&` or `∧`) and disjunctions (`|` or `∨`), can have parenthesis.
    - `ignores :: [string]`\
      required, see the description of the top-level `ignores` option.

When initializing from a template, both the the file content and the file name will be read as mustache templates. After performing a substitution, the new file will be written to the corresponding location inside the project directory.

You can also take a look at [bundled templates](https://github.com/poscat0x04/HI/tree/master/data/templates).

### Special variables names

`hi` will set the values for some special variables and can overwrite user configs. These names should not be used in your `options`. These special variables include:

- `name`, `email`, `github_usernmae`
- `year`, `month`, `day`, `iso8601`: system time
- `project`: the name of the project that is trying to create.
