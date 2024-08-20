# Configs
Configuration files for the utilities I use. Managed by
[stow](https://www.gnu.org/software/stow/).

> I do have some repitition of configs in
> [nixos-config](https://github.com/crodjer/nixos-config), but eventually I'll
> be switching to one method.

## Setup
1. Install `git`, `stow`.
2. Clone this repo, creating the directory: `configs`.
3. For each package / configuration that you want to install run `stow`. For
   example for neovim:
   ```
   stow --no-folding -t $HOME neovim
   ```

## Mac
For Mac, there is `Brewfile` which tracks all the packages needed. It is handy
as a list of things I need for Linux too!
