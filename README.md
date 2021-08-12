# Configs
Collection of my dotfiles and scripts, which allow me to get up and
running on a new machine within a very short amount of time.

One way to set things up quickly:

1. Clone this repo, creating the directory: `configs`.
2. Source shell functions: `source configs/.shell_functions`
3. Run: `make_home`. This will just be a dry run, listing the commands
   to be executed.
4. Run: `make_home -x` to actually do the symlinking of dotfiles in
   the correct locations.

## Dotfiles

Most of the configurations are present in the top level, as dotfiles.

## Scripts
The `scripts` directory consists of a set of scripts which I find
useful on my Linux/Mac systems.
