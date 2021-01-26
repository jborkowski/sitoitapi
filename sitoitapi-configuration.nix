{
  pkgs ? import ./haskell-pkgs.nix,
}:
{
  network.description = "sitoitapi";

  sitoitapi =
    { config, ... }:
    let
      hsPkgs = import ./. { inherit pkgs; };
      sitoitapi = hsPkgs.sitoitapi.components.exes.sitoitapi-exe;
    in
    { networking.hostName = "sitoitapi";

      networking.firewall.allowedTCPPorts = [ 22 80 ];
      environment.systemPackages = [ sitoitapi ];

      systemd.services.sitoitapi =
        { description = "Si to IT - API";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig =
            { ExecStart = "${sitoitapi}/bin/sitoitapi-exe /secrets/appconfig";
            };

        };
      deployment.keys.appconfig = {
        text = builtins.readFile ./.prod.config;
        destDir = "/secrets";
      };
    };
}
