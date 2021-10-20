module Make (B : Frontend.S) = struct
  let signature module_name namespace s =
    let driver = Drv.init module_name namespace in
    s |> Translate.signature ~driver |> Generate.structure
end
