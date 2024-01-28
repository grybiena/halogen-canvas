{ ps-pkgs, pkgs, name, ... }:
  with ps-pkgs;
  { version = "1.0.0";
    dependencies =
      [ aff-promise
        canvas
        colors
        halogen
      ];
    src = "src";
    pursuit = {
      inherit name; 
      repo = "https://github.com/grybiena/halogen-canvas.git";
      license = pkgs.lib.licenses.mit;
    };

  }
