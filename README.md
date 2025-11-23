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
   example for a healthy basic system:
   ```
   stow --no-folding -t $HOME neovim
   stow --no-folding -t $HOME fish
   stow --no-folding -t $HOME tmux
   stow --no-folding -t $HOME git
   stow --no-folding -t $HOME starship
   ```

## Mac
For Mac, use `nix-darwin/` to setup a nix based system.

## Debian
For debian, there's `packages.txt` which can be used for quickly installing the
basic required packages.

```
cat packges.txt | xargs sudo apt install
```
