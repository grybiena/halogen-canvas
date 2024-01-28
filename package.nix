{ ps-pkgs, pkgs, name, ... }:
  with ps-pkgs;
  { version = "1.0.0";
    dependencies =
      [ halogen
        halogen-css
        profunctor-lenses
        options
        canvas
      ];
    src = "src";
    pursuit = {
      inherit name; 
      repo = "https://github.com/grybiena/halogen-canvas.git";
      license = pkgs.lib.licenses.mit;
    };

  }
