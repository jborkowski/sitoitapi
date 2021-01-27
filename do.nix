let
  region = "ams3";
in
{ sitoitapi =
  { resources, ... }:
  { deployment.targetEnv = "digitalOcean";
    deployment.digitalOcean.region = region;
    deployment.digitalOcean.size = "s-1vcpu-1gb";

  };
  resources.sshKeyPairs.ssh-key = {};
}
