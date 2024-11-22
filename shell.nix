{ pkgs ? import <nixpkgs> { } }: pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ nodejs_22 llvmPackages_18.lld ];
}
