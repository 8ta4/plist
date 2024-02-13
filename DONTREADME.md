## Setup

> How do I set up this tool's dev environment?

1. Install [devenv](https://github.com/cachix/devenv/blob/dad24416b1ca61a5551db0de863b047765ac0454/docs/getting-started.md#installation).

1. Install [direnv](https://github.com/cachix/devenv/blob/dad24416b1ca61a5551db0de863b047765ac0454/docs/automatic-shell-activation.md#installing-direnv).

1. Run the following commands:

   ```sh
   git clone git@github.com:8ta4/plist.git
   cd plist
   direnv allow
   ```

The `devenv.nix` file has got all the scripts you need.
