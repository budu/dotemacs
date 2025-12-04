# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a personal Emacs configuration based on David Wilson's "Emacs From Scratch" series. The configuration uses the "mu/" namespace prefix for custom variables and functions, with a modular architecture that loads configuration files from the `mu/` directory.

## Package Management

This configuration uses the built-in package management tools:
- **package.el** (built-in): Standard Emacs package manager for MELPA/ELPA packages
- **use-package with :vc**: For packages from Git repositories (Emacs 29+)

### Update Commands

To update packages, run these commands in Emacs:
```elisp
M-x package-upgrade-all       ; Update all installed packages
```

For packages installed via `:vc`, you can update them by removing and reinstalling:
```elisp
M-x package-delete RET <package-name> RET
```
Then restart Emacs to reinstall from the Git repository.

## Architecture

### Core Files
- `init.el`: Main configuration file with package setup and global settings
- `functions.el`: General utility functions
- `macros.el`: Keyboard macros
- `mu/`: Directory containing modular configuration files organized by feature

### Modular Configuration (`mu/` directory)
- `ruby.el`: Ruby development tools and helpers
- `magit.el`: Git workflow enhancements
- `org.el`: Org-mode customizations and workflows
- `i18n.el`: Internationalization helpers
- `auto-clean-buffers.el`: Buffer management
- `cg.el`: Project-specific bindings
- `project.el`: Project management utilities
- `python.el`: Python development configuration
- `text.el`: Text manipulation functions

### Key Custom Functions

**Ruby Development:**
- `mu/ruby/find-definition`: Find Ruby method/class definitions using ripgrep
- `mu/ruby/find-references`: Find all references to Ruby symbols
- `rubocop-open-documentation-at-point`: Open RuboCop documentation for cop at point

**Git Workflows:**
- `mu/magit/quicksave`: Stage all, commit, and push in one command (C-c q)
- `mu/magit/open-parent`: Open magit for project root

**Org-mode Workflows:**
- `mu/org/todo-done-and-commit`: Mark TODO as DONE and commit staged changes using TODO content
- `mu/org/send-last-prompt-block-to-claude`: Send prompt blocks to Claude Code
- `mu/org/create-today-todo`: Create TODO scheduled for today

## Development Workflows

### Ruby Development
- RSpec testing with `F9` to run specs and `C-F9` for live mode
- RuboCop integration with auto-correction
- Rails project support with projectile-rails
- Ruby documentation via yari and robe

### Testing
For Elisp development:
- **buttercup**: Testing framework for Emacs Lisp
  - Run tests: `M-x buttercup-run-at-point` (C-c C-t in buttercup-minor-mode)
  - Command line: `emacs -batch -f package-initialize -L . -f buttercup-run-discover`
- **elisp-lint**: Linting for Emacs Lisp
  - Command line: `emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch *.el`

### Project Structure
- Personal projects in `~/projects/`
- Work projects in `~/cg/`
- Notes in `nb-notes/` subdirectory within projects
- Org agenda files from `~/cg/`, `~/org/`, and `~/projects/`

## Key Bindings

### Global Shortcuts
- `C-c g`: Prefix map for custom commands
- `C-x g`: Open magit for parent directory
- `C-c q`: Quick save (stage all, commit, push)
- `C-c f`: Find file in project (projectile)
- `C-c s`: Search in project (ripgrep)
- `F5`: Open main org file (`~/org/index.org`)
- `F6`: Open personal notes
- `F12`: Open init.el

### Development
- `M-.`: Find definition (language-specific)
- `C-M-.`: Find references
- `F9`: Run RSpec for current spec/file
- `C-c r`: Rails command map (projectile-rails)

## External Dependencies

- **ripgrep**: Required for fast project searching
- **ASDF**: Version manager integration (configured for `/opt/asdf-vm/bin/asdf`)
- **ruby-lsp**: Language server for Ruby
- **copilot-language-server**: AI completion via GitHub Copilot

## Font and Appearance

- Default font: "Fira Code Retina" at height 80
- Theme: doom-dracula with custom background (#121a1e)
- Icons: Requires `M-x all-the-icons-install-fonts` and `M-x nerd-icons-install-fonts`

## Special Features

- Golden ratio window management
- Aggressive auto-revert for file watching
- Custom paragraph handling for various list styles
- Posframe integration for floating UI elements
- Claude Code integration for AI assistance
