open Gospel
open Ttypes

let rec mutable_ ~driver (ty : Ttypes.ty) =
  let ts_int = Drv.get_ts driver [ "Gospelstdlib"; "int" ] in
  let ts_array = Drv.get_ts driver [ "Gospelstdlib"; "array" ] in
  match ty.ty_node with
  | Ttypes.Tyvar _ -> Error ()
  | Ttypes.Tyapp (ts, [])
    when ts_equal ts ts_unit || ts_equal ts ts_integer || ts_equal ts ts_bool
         || ts_equal ts ts_float || ts_equal ts ts_char || ts_equal ts ts_int ->
      Ok false
  | Ttypes.Tyapp (ts, _) when ts_equal ts ts_string || ts_equal ts ts_array ->
      Ok true
  | Ttypes.Tyapp (ts, [ ty ]) when ts_equal ts ts_option || ts_equal ts ts_list
    ->
      mutable_ ~driver ty
  | Ttypes.Tyapp (ts, tyl) ->
      let def = Drv.get_type_definition driver ts in
      let mutable_ts = mutable_def ~driver def in
      List.fold_left
        (fun acc ty ->
          match acc with
          | Error _ -> acc
          | Ok b -> Result.bind (mutable_ ~driver ty) (fun b' -> Ok (b || b')))
        mutable_ts tyl

and mutable_def ~driver:_ _def = assert false
