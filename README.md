# Si to IT - API

## Developement

### Pre-requirements

This project is build using [haskell.nix](https://input-output-hk.github.io/haskell.nix/). This means that the only requirement for building this project is having [Nix installed](https://nixos.org/download.html) on your system.

In order to *download* GHC compiler from pre-build cache, please add the following sections to `/etc/nix/nix.conf` or `~/.config/nix/nix.conf`.

```
trusted-public-keys = [...] hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= [...]
substituters = [...] https://hydra.iohk.io [...]
```

Otherwise Nix will have to build several copies of GHC, before building this project - and this will take a while.

### Building the project

Enter the nix-shell

```
$> nix-shell
trace: Using latest index state for sitoitapi!
trace: Using index-state: 2020-09-14T00:00:00Z for haskell-template-project
trace: Shell for sitoitapi
trace: Shell for sitoitapi
trace: Using index-state: 2020-05-31T00:00:00Z for hoogle
(...)
trace: Using latest index state for cabal-install!
trace: Using index-state: 2020-09-14T00:00:00Z for cabal-install
```

## Deployment
This project is using [NixOps](https://github.com/NixOS/nixops) as tool for deploying.

### Deploy to VirtualBox (locally):

``` sh
$ nix-shell -i nixops
$ nixops create ./sitoitapi-configuration.nix ./sitoitapi-virtualbox.nix -d sitoitapi-local
$ nixops deploy -d sitoitapi-local
## $ nixops modify ./sitoitapi-configuration.nix ./sitoitapi-virtualbox.nix -d sitoitapi-local
```

### Deploy to EC2 (AWS)

``` sh
$ nix-shell -i nixops
$ nixops create ./sitoitapi-configuration.nix ./sitoitapi-aws.nix -d sitoitapi-prod
$ nixops deploy -d sitoitapi-prod
## $ nixops modify ./sitoitapi-configuration.nix ./sitoitapi-virtualbox.nix -d sitoitapi-aws
```
