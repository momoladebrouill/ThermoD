open Raylib
let w = 800
let h = 600
let c = 0.1
let rcercle = 20
let dt = 20.0 /. 60.0
let dx = 0.5
let dy = 0.5

type status = {
    t : float Grid.t ;
    time : float
}


let draw st =
    let t = st.t in
      begin_drawing ();
      Grid.iteri (fun (x,y) v  ->
          draw_rectangle (x*10) (y*10) 10 10 (
              color_from_hsv 0. 1. v
          )
        ) t ;
  end_drawing ()


let nabla t (x,y) =
    let get = Grid.get t ~default:0.5 in
    let dx2 = get (x+1) y
        -. 2.0 *. get (x) y
        +. get (x-1) y
    in
    let dy2 = get x (y+1)
        -. 2.0 *. get x (y)
        +. get x (y-1)
    in dx2 /. (dx**2.0) +. dy2 /. (dy**2.0)

let clamp v l t =
    max (min v t) l

let dist (x1,y1) (x2,y2) = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)

let update {t;time} =
    let t' = Grid.mapi (fun pos v ->
        c *. dt *. (nabla t pos) *. dt +. v
    ) t
    in
    Grid.iteri (fun (x,y) _ ->
        let d = dist (x,y) (w/20,h/20) in
        if d < rcercle then
            Grid.set t' x y ((1.0 +. cos time) /. 2.0)
    ) t;

    let time' = time +. 0.01 in
    {t = t'; time = time'}

let mingrid t = Grid.fold_left min 1.0 t
let maxgrid t = Grid.fold_left max 0.0 t

let rec loop st =
  if Raylib.window_should_close () then begin
      Raylib.close_window ();
      print_newline ();
      st
    end
  else begin
      draw st;
      loop (update st)
    end

let setup () =
  Raylib.init_window w h "Blob";
  if is_window_ready () then {
      t = Grid.make (w/10) (h/10) 0.;
      time = 0.0;
    }
  else failwith "window not ready"

let () =
    let st_final = setup () |> loop in
      Printf.printf "Caca caca de papa papa %f %f\n"
        (mingrid st_final.t) (maxgrid st_final.t)

