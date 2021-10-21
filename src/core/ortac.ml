module Make (B : Frontend.S) = struct
  let signature module_name namespace s =
    let driver = Drv.init module_name namespace in
    let translated = Translate.signature ~driver s in
    Report.emit_warnings Fmt.stderr translated;
    Generate.structure translated
end
