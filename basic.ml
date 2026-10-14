module Point = struct
    type t = {
        x : float;
        y : float
    }

    let make x y = { x = x; y = y }

    let (+$) p1 p2 =
        { 
            x = p1.x +. p2.x;
            y = p1.y +. p2.y
        }

    let ( *$) l p =
        {
            x = l *. p.x;
            y = l *. p.y
        }

    let (-$) p1 p2 =
        p1 +$ ((-1.0) *$ p2)

    let norm p =
        sqrt (p.x *. p.x +. p.y *. p.y)

    let thres = 1e-5

    let normalize p =
        let n = norm p in
        assert (n > thres);
        (1. /. n) *$ p

    let middle p1 p2 =
        (0.5 *$ p1) +$ (0.5 *$ p2)

    let normal p = make (-. p.y) p.x

    let unit_vector a b =
        normalize (b -$ a)

    let normal_vector a b =
        normal (unit_vector a b)

    let opposite p =
        (-1.0) *$ p

    let coordinates p1 p2 (x,y) =
        let u = unit_vector p1 p2 in
        let v = normal u in
        p1 +$ (x *$ u) +$ (y *$ v)

    let dot_product p1 p2 =
        p1.x *. p2.x +. p1.y *. p2.y

    let lower p1 p2 =
        if p1.y < p2.y
        then p1 else p2

    let higher p1 p2 =
        if p1.y < p2.y
        then p2 else p1

end

module Ray = struct
    open Point

    type t = {
        src : Point.t;
        dir : Point.t
    }

    let segment r =
        (* debug function to display ray as a segment *)
        let open Point in
        (r.src, r.src +$ (100. *$ r.dir))

    let make src dir =
        { src = src; dir = dir }

    let emit src dst =
        {
            src = src;
            dir = normalize (dst -$ src)
        }

    let mirror r =
        {
            src = r.src;
            dir = Point.opposite r.dir
        }

    let bisect p1 p2 =
        let m = Point.middle p1 p2 in
        let n = Point.normal_vector p1 p2 in
        make m n

    let solve_intersect_rays r1 r2 =
        (* M(r1,t1) = M(r2,t2) solves for t1,t2
           m1 + t1 * d1 = m2 + t2 * d2 

           m1 . n1 = m2 . n1 + t2 * (d2 . n1)
           t2 = (m1 - m2) . n1 / d2 . n1
        *)
        let open Point in
        let n1 = normal r1.dir in
        let n2 = normal r2.dir in
        let t2 = dot_product (r1.src -$ r2.src) n1 /. dot_product r2.dir n1 in
        let t1 = dot_product (r2.src -$ r1.src) n2 /. dot_product r1.dir n2 in
        (t1, t2)

    let intersect_segment r a b =
        let rab = emit a b in
        let t, tab = solve_intersect_rays r rab in
        (* rab.dir est unitaire : tab varie de 0 à |ab| sur le segment *)
        if tab >= 0. && tab <= norm (b -$ a)
        then Some t
        else None

    let intersect_rays r1 r2 =
        let t1, t2 = solve_intersect_rays r1 r2 in
        if t1 >= 0. && t2 >= 0.
        then Some t1
        else None

end

module AABB = struct
    type t = {
        ul : Point.t; 
        dr : Point.t
    }

    let width b =
        b.dr.x -. b.ul.x

    let height b =
        b.dr.y -. b.ul.y

    let make ul dr =
        { ul = ul; dr = dr }
    
    let intersect box ray =
        let open Point in
        let open Ray in
        let a, b = ray.dir.x, ray.dir.y in
        let tleft = 
            if a = 0. then 1e10
            else (box.ul.x -. ray.src.x) /. a in
        let tright = 
            if a = 0. then 1e10
            else (box.dr.x -. ray.src.x) /. a in
        let ttop = 
            if b = 0. then 1e10
            else (box.ul.y -. ray.src.y) /. b in
        let tbottom = 
            if b = 0. then 1e10
            else (box.dr.y -. ray.src.y) /. b in
        let tleft = if tleft < 0. then 1e10 else tleft in
        let tright = if tright < 0. then 1e10 else tright in
        let ttop = if ttop < 0. then 1e10 else ttop in
        let tbottom = if tbottom < 0. then 1e10 else tbottom in
        let t = min (min tleft tright) (min ttop tbottom) in
        ray.src +$ (t *$ ray.dir)

    let vertices box =
        let open Point in
        [ box.ul; box.dr;
          Point.make box.ul.x box.dr.y;
          Point.make box.dr.x box.ul.y ]

    let bounds l =
        let open Point in
        let l_x = List.map (fun p -> p.x) l in
        let l_y = List.map (fun p -> p.y) l in
        let x_max = 
            List.fold_left max (List.hd l_x) l_x in
        let x_min = 
            List.fold_left min (List.hd l_x) l_x in
        let y_max = 
            List.fold_left max (List.hd l_y) l_y in
        let y_min = 
            List.fold_left min (List.hd l_y) l_y in
        { ul = Point.make x_min y_min;
          dr = Point.make x_max y_max }

end

module Triangle  = struct
    type t = Point.t * Point.t * Point.t

    let det (x1,x2,x3) (y1,y2,y3) =
           (x1 *. y2 -. x2 *. y1)
        -. (x1 *. y3 -. x3 *. y1)
        +. (x2 *. y3 -. x3 *. y2)

    let area (p1,p2,p3) =
        let open Point in
        2. *. det (p1.x, p2.x, p3.x) (p1.y, p2.y, p3.y)

    let is_direct (a,b,c) =
        let open Point in
        let ab = b -$ a in
        let ac = c -$ a in
        ab.x *. ac.y -. ab.y *. ac.x > 0.0

    let circumscribed_circle_center (p1,p2,p3) =
        let open Point in
        let delta = area (p1,p2,p3) in
        let x_o = (1. /. delta)
            *. det 
                (p1.x *. p1.x +. p1.y *. p1.y,
                 p2.x *. p2.x +. p2.y *. p2.y,
                 p3.x *. p3.x +. p3.y *. p3.y)
                (p1.y, p2.y, p3.y)
        and y_o = (-1. /. delta)
            *. det 
                (p1.x *. p1.x +. p1.y *. p1.y,
                 p2.x *. p2.x +. p2.y *. p2.y,
                 p3.x *. p3.x +. p3.y *. p3.y)
                (p1.x, p2.x, p3.x) in
        Point.make x_o y_o 

    let make p1 p2 p3 = 
        (p1, p2, p3)
end

module Polygon = struct
    type t = Point.t list

    let inside pol p =
        let b = List.hd pol in
        let rec aux p l =
            match l with
             [] -> true
            | [a] ->
                   not (Triangle.is_direct (Triangle.make a b p))
            | a::b::q ->
                   not (Triangle.is_direct (Triangle.make a b p))
                && aux p (b::q)
        in aux p pol
    
    let intersect ?(cycling=true) r pol = 
        (* Convex hypothesis is crucial here *)
        match pol with
        | [] -> None
        | p0::_ ->
            let rec aux l =
                match l with
                | [p] when cycling -> Ray.intersect_segment r p p0
                | [_] | [] -> None
                | p1::p2::q ->
                    let i = Ray.intersect_segment r p1 p2 in
                    if i = None
                    then aux (p2::q)
                    else i
            in aux pol
end

module Intersection = struct
    let best i1 i2 =
        match i1, i2 with
        | None, _ -> i2
        | _, None -> i1
        | Some t, _ when t < 0. -> i2
        | _, Some t when t < 0. -> i1
        | Some t1, Some t2 -> Some (min t1 t2)

    let point r i =
        match i with
        | None -> r.Ray.src
        | Some t -> 
            let open Point in
            let open Ray in
                r.src +$ (t *$ r.dir)
end


