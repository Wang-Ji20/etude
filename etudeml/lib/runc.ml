(* This file implements Run-length coding for strings. *)

let compression x =
  let lenx = String.length x in
  let format_n_elements x n =
    let x_as_str = String.make 1 x in
    if n > 1 then x_as_str ^ Int.to_string n else x_as_str
  in
  let rec compression_with_state seen n cursor =
    if cursor == lenx then format_n_elements seen n
    else
      let next = x.[cursor] in
      if next == seen then compression_with_state seen (n + 1) (cursor + 1)
      else format_n_elements seen n ^ compression_with_state next 1 (cursor + 1)
  in
  if lenx == 0 then x else compression_with_state x.[0] 1 1
