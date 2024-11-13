open Raylib
let w = 800
let h = 600
let dt = 1.0 /. 60.0

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

let update {t;time} =
    let t' = Grid.mapi (fun pos v ->
        let vois_values = Grid.vois_value pos t in
        let sum = List.fold_left (+.) 0. vois_values in
        let count = List.length vois_values in
        v +. (sum /. float count) *. dt
    ) t
    in
    Grid.set t' (w/20) (h/20) 1.0;
    let time' = time +. dt in
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

