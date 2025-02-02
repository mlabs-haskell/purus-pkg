# `./templates` includes files for creating purus projects from a template (using nix templates)
{ ... }: {

  flake.templates.default = {
    path = ./default;
    description =
      "Example flake for developing with Purus using purus-pkg to manage dependencies";
  };
}
