# purus-pkg

A package manager for [purus](https://github.com/mlabs-haskell/purus).

A *package* contains:

- A name.

- A version (using [semantic versioning](https://semver.org/)).

- A mapping from names to [constraints](https://hackage.haskell.org/package/semver-0.4.0.1/docs/Data-SemVer-Constraint.html#v:satisfies) (boolean expressions on versions) called *dependencies*.

- A file `./package.json` which contains the above information.

- Source files for the package of the form `*.purs`.

The executable `purus-pkg`, when given a list of *registries* (a *registry* is
a mapping from a package name and version to the package sources), will find a
set of packages (where the names are unique) which contain at least all names
in the dependencies, and satisfy all the version constraints of each package in
the transitive reflexive closure of the dependencies of the `./package.json`.

This has the implication that given packages `A`,`B`,`C`, and `D` with a
dependency tree as follows (an arrow from a package to another denotes the
former package depends on the latter package with some version constraint).

```mermaid
graph TD;
    A-->B;
    A-->C;
    B-->D;
    C-->D;
```

`purus-pkg` will *always* try to find single version of package `D` which
satisfies the constraints of both packages `B` and `C`.

Unfortunately, this has the consequence of being NP-complete (reduce to 3SAT),
so there may be certain instances which take a long time compute. In which
case, one can tighten the version constraints to reduce the search time.

## Getting started

An easy way to get started is to use the flake template provided by this
project. Here's how to start a new project using the template:

```
mkdir myproject
cd myproject
nix flake init --template github:mlabs-haskell/purus-pkg
```

Notice that this requires Nix, for an introduction to the Nix ecosystem, check
out [Zero to Nix.](https://zero-to-nix.com/concepts/flakes/), in particular
learn more about [Nix flakes](https://zero-to-nix.com/concepts/flakes/).

## `purus-pkg`

`purus-pkg` is the executable for the package manager.

- `purus-pkg install` will find satisfying versions (from the provided
  registries) and install the versions in the `purus-modules/` folder.

- `purus-pkg build` will call `purus purus-modules/` to compile the project.

### Registries

`purus-pkg` looks for the dependencies by looking in the provided registries.
In the case that multiple registries provide the same name and version of a
package, the rightmost package takes precedence e.g. the following means
`/path/to/registry2.json` takes precedence over `/path/to/registry1.json`.

```bash
$ purus-pkg install \
    --local-registry /path/to/registry1.json \
    --local-registry /path/to/registry2.json
```

#### Local registries 

The tool `purus-pkg-local-registry` allows one to create local registries.
