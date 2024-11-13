type 'a t = 'a array array

let vois_value (i,j) t =
    let n = Array.length t in
    let m = Array.length t.(0) in
    [(i-1,j);(i+1,j);(i,j-1);(i,j+1)]
    |> List.filter (fun (i',j') ->i'>=0 && i'<n && j'>=0 && j'<m)
    |> List.map (fun (i',j') -> t.(i').(j'))

let make n m init =
    Array.init n (fun _ -> Array.make m init)

let iteri f grid =
    Array.iteri (fun i row ->
        Array.iteri (fun j cell ->
            f (i,j) cell)
        row)
    grid

let mapi f grid =
    Array.mapi (fun i row ->
        Array.mapi (fun j cell ->
            f (i,j) cell)
        row)
    grid

let fold_left f v0 grid =
    Array.fold_left (fun acc1 t ->
        Array.fold_left (fun acc2 e ->
            f acc2 e) acc1 t)
    v0 grid

let set t x y v =
    t.(x).(y) <- v

let get t ?(default=0.0) x y =
    if x < 0 || x >= Array.length t || y < 0 || y >= Array.length t.(0) then
        default
    else
        t.(x).(y)
