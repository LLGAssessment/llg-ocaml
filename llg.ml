open List;;
open String;;

let readlines () = 
    let rec readlines' lines = 
    try readlines' (read_line () :: lines) with
        | End_of_file -> lines
    in readlines' [] in

let links_for_word words (left_idx, left_word) = 
    let left_last = left_word.[String.length(left_word) - 1] in
    Array.of_list (
        List.rev_map fst (
            List.filter (fun (right_idx, right_word) ->
                right_idx != left_idx && right_word.[0] == left_last) words )) in

let links words = Array.of_list (
    List.map (links_for_word words) words) in

let longest_path graph =
    let rec longest_path' path depth pos =
        let new_path = pos :: path in
        Array.fold_left (
            fun (acc_path, acc_depth) -> fun next_pos ->
                if List.mem next_pos path then
                    (acc_path, acc_depth)
                else
                    let (cur_path, cur_depth) =
                        longest_path' new_path (depth + 1) next_pos in
                    if (acc_depth < cur_depth) then
                        (cur_path, cur_depth)
                    else
                        (acc_path, acc_depth)
        ) (new_path, depth) graph.(pos) in
    List.rev (fst (Array.fold_left (
        fun (acc_path, acc_depth) -> fun next_pos ->
            let (cur_path, cur_depth) =
                longest_path' [] 0 next_pos in
            if (acc_depth < cur_depth) then
                cur_path, cur_depth
            else
                acc_path, acc_depth
    ) ([], 0) (Array.mapi (fun idx -> fun _ -> idx) graph))) in

let words = 
    List.filter (function s -> String.length(s) > 0) (
        List.sort_uniq compare (
            List.rev_map String.trim (readlines ())
        )
    ) in
let words_enum = List.mapi (fun i -> fun e -> (i, e)) words in

let graph = links words_enum in

(*for i = 0 to Array.length(graph) - 1 do
    Printf.printf "%s -> " (List.nth words i);
    for j = 0 to Array.length(graph.(i)) - 1 do
        Printf.printf "%s, " (List.nth words graph.(i).(j))
    done;
    print_endline "";
done;*)

List.iter (fun idx -> print_endline (List.nth words idx)) (longest_path graph);;
