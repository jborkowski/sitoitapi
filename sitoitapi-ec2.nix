let
  region = "eu-west-2";
  accessKeyId = "prod";
  keyPair = { inherit region accessKeyId; };
in
{ sitoitapi =
  { resources, ... }:
  { deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = accessKeyId;
    deployment.ec2.region = region;
    deployment.ec2.instanceType = "t2.nano";
    deployment.ec2.keyPair = resources.ec2KeyPairs.sitoit-keys;

  };

  resources.ec2KeyPairs.sitoit-keys = keyPair;
}
