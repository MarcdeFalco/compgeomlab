open Basic

let compute points =
    if List.length points < 3
    then points
    else begin
        let open Point in
        let rec leftmost l =
            match l with
            | [] -> failwith "empty"
            | [p] -> p
            | t::q -> let lq = leftmost q in
                if lq.x < t.x || (lq.x = t.x && lq.y < t.y)
                then lq else t
        in
        let next p =
            let rec search l =
                match l with
                | [] -> p (* tous les points sont égaux à p *)
                | t::q ->
                    if t = p
                    then search q
                    else t
            in
            let rec aux q l =
                match l with
                | [] -> q
                | r::l' when r = q || r = p -> 
                    aux q l'
                | r::l' ->
                    if Triangle.is_direct (Triangle.make p q r)
                    || (not (Triangle.is_direct (Triangle.make p r q))
                        && norm (r -$ p) > norm (q -$ p))
                    then aux r l'
                    else aux q l'
            in aux (search points) points
        in
        let start = leftmost points in
        let rec wrap p =
            let q = next p in
            if q = start
            then [p;q]
            else p :: wrap q
        in wrap start
    end

