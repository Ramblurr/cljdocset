{
  babashka,
  clj-kondo,
  coreutils,
  lib,
  makeWrapper,
  stdenv,
  ...
}:
let
  mainNs = "cljdocset.cli";
  pname = "cljdocset";
  version = "0.0.1";
  runtimeDeps = [
  ];
in
stdenv.mkDerivation {
  inherit pname;
  inherit version;
  src = ./.;

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [
    babashka
    clj-kondo
  ];

  doCheck = true;
  # --fail-level error \
  checkPhase = ''
    echo "Running clj-kondo linting..."
    ${lib.getExe clj-kondo} \
      --lint $src \
      --config '{:skip-comments true :linters {:namespace-name-mismatch {:level :off}}}'
  '';

  installPhase = ''
    mkdir -p $out/share/${pname}
    mkdir -p $out/bin

    pwd
    ls -al
    cp -r bb.edn $out/share/${pname}/
    cp -r src $out/share/${pname}/
    cp -r resources  $out/share/${pname}/

    makeWrapper ${babashka}/bin/bb $out/bin/${pname} \
      --add-flags "--config $out/share/${pname}/bb.edn" \
      --add-flags "--deps-root $out/share/${pname}" \
      --add-flags "--main ${mainNs}" \
      --prefix PATH : ${lib.makeBinPath runtimeDeps}
  '';

  meta = {
    description = "a tool to generate a docset from the cljdoc of a Clojure project";
    license = lib.licenses.eupl12;
    maintainers = [ lib.maintainers.ramblurr ];
  };
}
