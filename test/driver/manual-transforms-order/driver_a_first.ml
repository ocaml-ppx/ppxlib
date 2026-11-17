open Ppxlib

let reorder_transforms l =
  List.stable_sort
    (fun t t' ->
      match (Driver.transform_name t, Driver.transform_name t') with
      | "exp_to_a", _ -> -1
      | _, "exp_to_a" -> 1
      | _, _ -> 0)
    l

let () = Driver.all_transforms := reorder_transforms !Driver.all_transforms
let () = Driver.standalone ()
