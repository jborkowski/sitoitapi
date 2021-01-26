let
  region = "eu-central-1";
  accessKeyId = "prod";

in
{ sitoitapi =
  { resources, ... }:
  { deployment.targetEnv = "virtualbox";
    deployment.virtualbox.headless = true;
  };

}
