rec {
  description = "halogen-canvas";
  inputs = {
    env.url = "github:grybiena/purescript-environment";  
  };

  outputs = inputs@{ env, ... }:
    env.flake-utils.lib.eachDefaultSystem (system:
      env.build-package { inherit system;
                          name = description;
                          src = ./.;
                          overlays = inputs; 
                          derive-package = ./package.nix;
                        }                
   );
}

