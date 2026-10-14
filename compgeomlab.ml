open Basic

module FromRay = struct 
    let point v =
        let open Raylib in
        Point.make (Vector2.x v) (Vector2.y v)
end

module ToRay = struct 
    let vector2 p = 
        let open Point in
        Raylib.Vector2.create p.x p.y
end

module ColorIterator = struct
    let i = ref 0

    let reduce_opacity alpha c =
        let open Raylib.Color in
        create (r c) (g c) (b c) alpha
        

    let colors = 
        let open Raylib.Color in
        Array.map (reduce_opacity 100)
            [| red; blue; yellow; blue; pink; orange; maroon;
                skyblue; darkblue; purple; violet; darkpurple; beige;
                brown; darkbrown; magenta |]

    let ncolors = Array.length colors

    let reset () =
        i := 0

    let next () =
        let c = colors.(!i) in
        i := (!i + 1) mod ncolors;
        c
end

let draw_point col r p =
    Raylib.draw_circle_v 
          (ToRay.vector2 p) r col

let draw_segment col p1 p2 =
    if p1 <> p2
    then begin
        let ua = Point.coordinates p2 p1 (10.,0.) in
        let da = Point.coordinates p2 p1 (10.,10.) in
        Raylib.draw_triangle
            (ToRay.vector2 p2) 
            (ToRay.vector2 da)
            (ToRay.vector2 ua)
            col;
        Raylib.draw_line_ex
            (ToRay.vector2 p1) (ToRay.vector2 p2)
            1. col
    end

let draw_ray col r = 
    let a,b = Ray.segment r in
    draw_segment col a b

let draw_polygon col p =
    if List.length p >= 2
    then begin
        let start = List.hd p in
        let rec aux l =
            match l with
            | [] -> ()
            | [p] -> draw_segment col p start
            | p1::p2::q -> draw_segment col p1 p2; aux (p2::q)
        in
        aux p
    end

let fill_polygon col p =
    let pa = Ctypes.CArray.of_list Raylib.Vector2.t 
            (List.map ToRay.vector2 p) in
    Raylib.draw_triangle_fan pa.Ctypes_static.astart
            (List.length p) col

let draw_voronoi_cell box (site,c) =
    let col = ColorIterator.next () in
    fill_polygon col (Voronoi.polygon box c);
    draw_polygon Raylib.Color.black (Voronoi.polygon box c);
    Raylib.draw_circle_v (ToRay.vector2 site) 20. Raylib.Color.white
    
type env = 
    {
        mutable points : Point.t list;
        mutable polygons : Polygon.t list;
        mutable voronoi : Voronoi.t
    }

let width, height = 1000, 1000

let setup () =
    Raylib.init_window width height "Computational Geometry Lab";
    Raylib.set_target_fps 60;
    Random.self_init ();
    (*
    let p = List.init 3
        (fun _ -> Point.make 
            (float_of_int (Random.int 800))
            (float_of_int (Random.int 450))) in
    *)
    (*
    let p = [] in
    {
        points = p;
        polygons = [ConvexHull.compute p];
        voronoi = Voronoi.compute dbg p
    }
    *)
    {
        points = [];
        polygons = [];
        voronoi = []
    }

let handle_events env =
    let handled = ref false in
    let open Raylib in
    if is_mouse_button_pressed MouseButton.Left
    then begin
        let p = get_mouse_position () in
        env.points <- FromRay.point p :: env.points;
        handled := true
    end;
    if is_mouse_button_pressed MouseButton.Right
    then (env.points <- []; handled := true);
    !handled

let recompute dbg env =
    env.polygons <- [ConvexHull.compute env.points]
    (* env.voronoi <- Voronoi.compute dbg env.points*)

let dbg = ref []

let fwidth = float_of_int width
let fheight = float_of_int height

let rec loop env =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let bounding_box =
          AABB.make
            (Point.make (-. fwidth) (-. fheight))
            (Point.make (2. *. fwidth) (2. *. fheight)) in
      ColorIterator.reset ();
      if handle_events env
      then (dbg := []; recompute dbg env);
      Raylib.begin_drawing ();
      Raylib.clear_background Raylib.Color.raywhite;
      List.iter (draw_voronoi_cell bounding_box) env.voronoi;
      List.iter (draw_polygon Raylib.Color.black) env.polygons;
      List.iter (draw_point Raylib.Color.black 2.) env.points;
      let p = FromRay.point (Raylib.get_mouse_position ()) in
      (*
      List.iter (fun (site,c) ->
          let pol = Voronoi.polygon bounding_box c in
          if Polygon.inside pol p
          then Raylib.draw_circle_v 
            (ToRay.vector2 site) 
        10. Raylib.Color.green) env.voronoi;
      List.iter (fun (a,b) -> 
          draw_segment Raylib.Color.gray a b;
          draw_point Raylib.Color.gray 5. a;
          draw_point Raylib.Color.gray 5. b) !dbg;
          *)

      (*
      let p1 = Point.make 100. 100. in
      let p2 = Point.make (fwidth -. 100.) 100. in
      let p3 = Point.make 100. (fheight -. 100.) in
      let p4 = Point.make (fwidth -. 100.) (fheight -. 100.) in
      
      List.iter (draw_point Raylib.Color.black 10.) [p1;p2;p3;p4];
      let r = Ray.mirror (Ray.bisect p3 p4) in
      let r12 = Ray.emit p1 p2 in
      draw_ray Raylib.Color.green r12;
      draw_ray Raylib.Color.blue r;
      let i = Ray.intersect_rays r r12 in
      let p = Intersection.point r i in
      draw_point Raylib.Color.red 20. p;
      *)

      Raylib.end_drawing ();
      loop env

let _ = 
    Raylib.set_config_flags 
        [Raylib.ConfigFlags.Msaa_4x_hint];
    setup () |> loop
