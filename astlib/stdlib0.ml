module Int = struct
  let to_string = string_of_int
end

module Option = struct
  let map f o = match o with None -> None | Some v -> Some (f v)
end

module String = struct
  include String

  let is_prefix t ~prefix =
    let rec is_prefix_from t ~prefix ~pos ~len =
      pos >= len
      || Char.equal (String.get t pos) (String.get prefix pos)
         && is_prefix_from t ~prefix ~pos:(pos + 1) ~len
    in
    String.length t >= String.length prefix
    && is_prefix_from t ~prefix ~pos:0 ~len:(String.length prefix)
end
