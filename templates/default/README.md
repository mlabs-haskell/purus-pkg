# purus-pkg template

## Getting started

The following command will fetch all dependencies and compile the project.

```
make
```

## Files

- `README.md` this file.

- `./src` source files of the package.

- `./Makefile` Makefile to orchestrate fetching the dependencies and building
  the project.

- `package.json` describes this current package and the required dependencies.

- `flake.nix` Nix setup to get `purus` and `purus-pkg` to compile this project.

