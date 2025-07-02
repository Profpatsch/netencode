{ lib }:
let

  netstring = tag: suffix: s:
    "${tag}${toString (builtins.stringLength s)}:${s}${suffix}";

  unit = "u,";

  # Create a natural number (unsigned)
  natural = n: "n:${toString n},";

  # Create a signed integer
  integer = n: "i:${toString n},";

  # Create a boolean value as tagged unit
  boolean = b: if b then "<4:true|u," else "<5:false|u,";

  text = netstring "t" ",";
  binary = netstring "b" ",";

  tag = key: val: netstring "<" "|" key + val;

  concatStrings = builtins.concatStringsSep "";

  record = lokv: netstring "{" "}"
    (concatStrings (map ({ key, val }: tag key val) lokv));

  list = l: netstring "[" "]" (concatStrings l);

  dwim = val:
    let
      match = {
        "bool" = boolean;
        "int" = integer;
        "string" = text;
        "set" = attrs:
          # it could be a derivation, then just return the path
          if attrs.type or "" == "derivation" then text "${attrs}"
          else
            record (lib.mapAttrsToList
              (k: v: {
                key = k;
                val = dwim v;
              })
              attrs);
        "list" = l: list (map dwim l);
      };
    in
    match.${builtins.typeOf val} val;

in
{
  inherit
    unit
    natural
    integer
    boolean
    text
    binary
    tag
    record
    list
    dwim
    ;
}
