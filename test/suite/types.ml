open Ltypes__Lib_rtac
open Common

let field_access () =
  let t = { x = 10; y = false } in
  check_success "get_x" (fun () -> get_x t |> ignore)

let invariant () =
  let t = { x = -10; y = false } in
  check_raises_ortac "get_x" (fun () -> get_x t |> ignore)

let suite =
  ( "Types",
    [
      ("simple field access", `Quick, field_access);
      ("type invariant", `Quick, invariant);
    ] )
