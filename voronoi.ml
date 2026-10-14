open Basic

type cell =
      Unbounded of Ray.t * Point.t list * Ray.t
    | Bounded of Polygon.t

type t = (Point.t * cell) list

let enclosing l =
    let b = AABB.bounds l in
    let w = max (AABB.width b) 1. in
    let h = max (AABB.height b) 1. in
    
    let p1 = Point.make 0.5 (0.5 +. 3. *. sqrt 2. /. 2.) in
    let p2 = Point.make
            (-3. *. sqrt 6. /. 4. +. 0.5)
            (-0.5 +. 3. *. sqrt 2. /. 2.) in
    let p3 = Point.make
            (3. *. sqrt 6. /. 4. +. 0.5)
            (-0.5 +. 3. *. sqrt 2. /. 2.) in

    let open Point in
    let p1 = b.ul +$ (w *$ p1) in
    let p3 = b.ul +$ (w *$ p2) in
    let p2 = b.ul +$ (w *$ p3) in

    let t = Triangle.make p1 p2 p3 in
    let center = Triangle.circumscribed_circle_center t in
    let d12 = normal_vector p1 p2 in
    let d21 = opposite d12 in
    let d13 = normal_vector p1 p3 in
    let d31 = opposite d13 in
    let d23 = normal_vector p2 p3 in
    let d32 = opposite d23 in
    [
        (p1, Unbounded (Ray.make center d12, [],
              Ray.make center d31));
        (p2, Unbounded (Ray.make center d23, [],
              Ray.make center d12));
        (p3, Unbounded (Ray.make center d31, [],
              Ray.make center d23))
    ]

let insert v p =
    let rec start v =
        match v with
        | _ -> failwith "unreachable"
    in failwith "toto"

let compute _ l =
    let initial = enclosing l in
    List.fold_left insert initial l

let polygon box c =
    match c with
    | Bounded pol -> pol
    | Unbounded (r1, pol, r2) ->
        (* Compute the cell border with the box *)
        let pS = AABB.intersect box r1 in
        let pE = AABB.intersect box r2 in
        let bL = AABB.vertices box
          |> List.filter 
                (fun p -> Triangle.is_direct (Triangle.make pS p pE)) 
          |> List.sort (fun p1 p2 ->
                  if Triangle.is_direct (Triangle.make p1 p2 pS)
                  then 1 else -1) in
        [pS; r1.Ray.src] @ pol @ [r2.Ray.src; pE] @ bL

let line_support h1 h2 =
    let open Point in
    let h1 = Array.of_list h1 in
    let h2 = Array.of_list h2 in
    let n1 = Array.length h1 in 
    let n2 = Array.length h2 in

    let u = ref 0 in
    for i = 1 to n1 - 1 do
        if h1.(!u).x < h1.(i).x
        then u := i
    done;
    let v = ref 0 in
    for i = 1 to n2 - 1 do
        if h2.(!v).x > h2.(i).x
        then v := i
    done;
    let nextu () = (!u+1) mod n1 in
    let nextv () = (!v+1) mod n2 in
    while Triangle.is_direct (Triangle.make h1.(!u) h2.(!v) h1.(nextu ())) do
        u := nextu ()
    done;
    while Triangle.is_direct (Triangle.make h1.(!u) h2.(!v) h2.(nextv ())) do
        v := nextv ()
    done;
    h1.(!u), h2.(!v)

let intersect_cell r cell =
    match cell with
    | Unbounded (rin, pol, rout) ->
        let i_in = Ray.intersect_rays r rin in
        let i_out = Ray.intersect_rays r rout  in
        begin
        match i_in, i_out with
        | Some _, _ -> i_in
        | _, Some _ -> i_out
        | _ -> Polygon.intersect ~cycling:false r pol
        end
    | Bounded pol -> Polygon.intersect r pol

let rec intersect_voronoi r v =
    match v with
    | [] -> None
    | (_,cell)::q ->
        let i1 = intersect_cell r cell in
        let i2 = intersect_voronoi r q in
        Intersection.best i1 i2

(*
let higher_intersect_voronoi r v =
    let r' = Ray.mirror r in
    let i_dir = intersect_voronoi r v in
    let i_rev = intersect_voronoi r' v in
    match i_dir, i_rev with
    | None, None -> failwith "none"
    | Some _, None -> Intersection.point r i_dir
    | None, Some _ -> Intersection.point r' i_rev
    | Some _, Some _ ->
        let p = Intersection.point r i_dir in
        let p' = Intersection.point r' i_rev in
        Point.higher p p'

let merge dbg l1 v1 l2 v2 =
    let hull1 = ConvexHull.compute l1 in
    let hull2 = ConvexHull.compute l2 in

    let a, b = line_support hull1 hull2 in

    let r = Ray.bisect a b in
    let p1 = higher_intersect_voronoi r v1 in
    let p2 = higher_intersect_voronoi r v2 in

    if p1.Point.y < p2.Point.y
    then 

    dbg := (r.Ray.src, p) :: (a,b) :: !dbg;
    v1 @ v2

let rec compute_sh dbg l =
    match l with
    | [] | [_] -> []
    | [p1;p2] ->
        let r = Ray.bisect p1 p2 in
        let r' = Ray.mirror r in
        [ 
            (p1, Unbounded (r, [], r'));
            (p2, Unbounded (r', [], r))
        ]
    | [p1;p2;p3] ->
        let open Point in
        let p1, p2, p3 =
            if Triangle.is_direct
                (Triangle.make p1 p2 p3)
            then p2, p1, p3
            else p1, p2, p3 in
        let t = Triangle.make p1 p2 p3 in
        let center = Triangle.circumscribed_circle_center t in
        let d12 = normal_vector p1 p2 in
        let d21 = opposite d12 in
        let d13 = normal_vector p1 p3 in
        let d31 = opposite d13 in
        let d23 = normal_vector p2 p3 in
        let d32 = opposite d23 in
        [
            (p1, Unbounded (Ray.make center d12, [],
                  Ray.make center d31));
            (p2, Unbounded (Ray.make center d23, [],
                  Ray.make center d12));
            (p3, Unbounded (Ray.make center d31, [],
                  Ray.make center d23))
        ]
    | _ ->
        let n = List.length l in
        let rec split_aux l acc k =
            if k = 0
            then (List.rev acc, l)
            else match l with
                | [] -> failwith "unreachable"
                | t::q -> split_aux q (t::acc) (k-1)
        in
        let l1, l2 = split_aux l [] (n/2) in
        let v1 = compute_sh dbg l1 in
        let v2 = compute_sh dbg l2 in
        merge dbg l1 v1 l2 v2

let compute dbg l =
    let open Point in
    l
    |> List.sort (fun p1 p2 -> if p1.x < p2.x then -1 else 1)
    |> compute_sh dbg

*)
