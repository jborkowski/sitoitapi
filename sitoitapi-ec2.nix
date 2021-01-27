let
  region = "us-east-1";
  accessKeyId = "prod";
  keyPair = { inherit region accessKeyId; };
in
{ sitoitapi =
  { resources, ... }:
  { deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = accessKeyId;
    deployment.ec2.region = region;
    deployment.ec2.instanceType = "t3.nano";
    deployment.ec2.keyPair = resources.ec2KeyPairs.sitoit-keys;
    deployment.ec2.ami = "ami-0f8b063ac3f2d9645";
    #deployment.ec2.securityGroups = [
    #    "HTTP/HTTPS"
    #    "ssh"
    #  ];
  };

  resources.ec2KeyPairs.sitoit-keys = keyPair;
}
