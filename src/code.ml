(* SIMPLE OCAML OVERVIEW *)

(* Basic variable binding *)

    let x = 5;;



(* Imperative-styled function declaration and scope *)

    let add x y = x + y
    in
    add 5 5;;



(* Anonymous-styled function declaration *)

    let add = fun x y -> x + y
    in  
    add 10 10;;



(* Explicit types can be declared *)

    let add (x: int) (y: int) : int =
        x + y 
    in 
    add 5 5;;



(* Parametric polymorphism can be used, but no function overloading *)

    let append (x: 'a list) (y: 'a list) : 'a list =
        List.append x y
    in 
    append [1; 2] [3; 4];;
    
    

(* Functions are first class *)

    let double = fun x -> x * 2
    in  
    List.map double [1; 2; 3];;



(* Higher order functions *)

    let add = fun x y -> x + y
    in
    let add2 = add 2
    in
    add2 10;;



(* Currying *)

    let add = fun x -> fun y -> x + y
    in
    let add2 = add 2
    in
    add2 10;;



(* Recursive functions and matching *)

    let rec factorial x = 
        match x with 
            | 1 -> 1
            | n -> n * factorial (n - 1)
    in
    factorial 5;;





(* BASIC LISTS *)

(* Define a list as a disjunctive type *)

    type 'a lst = Empty | Cons of 'a * 'a lst;;
    
    
    
(* Some classic list operations *)

    let head l =
        match l with
            | Cons (h, _) -> h
            | _           -> failwith "Not possible";;
        
    let tail l =
        match l with
            | Cons (_, t) -> t
            | _           -> failwith "Not possible";;
            
    let rec nth i l =
        match l with 
            | Cons (h, t) -> if   i = 0 
                             then h 
                             else nth (i - 1) t 
            | _           -> failwith "Not possible";;
            
    let rec pick i l =
        if   i < 1 
        then []
        else head l :: pick (i - 1) (tail l);;
    
    let rec map f l =
        match l with
            | Cons (h, t) -> Cons (f h, map f t)
            | _           -> Empty;;
            
    let rec filter f l =
        match l with 
            | Cons (h, t) -> if   f h 
                             then Cons (h, filter f t)
                             else filter f t
            | _           -> Empty;;
            
    
            
            
(* List tests *)

    let x = Cons (5, Empty) 
    in 
    assert ( head x = 5 );;
    
    let x = Cons (5, Empty) 
    in 
    let y = Cons (10, x)
    in 
    assert ( tail y = x );;

    let x = Cons (0, Cons (1, Cons (2, Empty))) 
    in 
    assert ( nth 2 x = 2 );;
    
    let x = Cons (2, Cons (4, Cons (6, Empty))) 
    in 
    assert ( pick 2 x = [2; 4] );;
    
    let x = Cons (2, Cons (4, Empty)) 
    and y = Cons (4, Cons (8, Empty)) 
    in 
    assert ( map (fun x -> 2 * x) x = y );;
    
    let x = Cons (2, Cons (4, Cons (6, Empty))) 
    and y = Cons (4, Empty) 
    in 
    let f = fun x -> x = 4 
    in
    assert ( filter f x = y );;





(* THUNK STREAMS *)

(* unit = () *)

    5;;

    "word";;

    ();;



(* Basic thunk concept *)

    let x = 45;;

    x;;

    let x = fun () -> 45;;
    
    x;;
    
    x ();;
    
    
    
(* Stream type -- like list but with a thunk *)

    type 'a stream = Cons of 'a * (unit -> 'a stream);;



(* Example thunk streams *)

    let rec ones = 
        Cons (1, fun () -> ones);;
    
    let nats =
        let rec inc i = 
            Cons (i, fun () -> inc (i + 1)) 
        in 
        inc 0;;
        
    let fibs =
        let rec fibsaux i j = 
            Cons(i, fun () -> fibsaux j (i + j)) 
        in 
        fibsaux 1 1;;



(* Stream analogs of the list operations *)

    let head s =
        let Cons (h, _) = s 
        in 
        h;;

    let tail s =
        let Cons (_, t) = s 
        in 
        t ();;

    let rec nth i s =
        let Cons (h, t) = s
        in 
        if   i = 0 
        then h 
        else nth (i - 1) (t ());;
        
    let rec pick i s =
        if   i < 1
        then []
        else head s :: pick (i - 1) (tail s);;
    
    let rec map f s =
        let Cons (h, t) = s 
        in 
        Cons (f h, fun () -> map f (t ()));;
    
    let rec filter f s =
        let Cons (h, t) = s 
        in 
        if   f h 
        then Cons (h, fun () -> filter f (t ()))
        else filter f (t ());;



(* A few mappings *)

    let increment = fun i -> i + 1;;
    
    let double = fun i -> i * 2;;
    
    let evens = fun i -> i mod 2 = 0;;
    
    let odds = fun i -> i mod 2 = 1;;
    
    

(* Stream tests *)

    assert ( head ones = 1 );;
    
    assert ( head (tail ones) = 1 );;

    assert ( pick 3 ones = [1; 1; 1] );;
    
    assert ( pick 3 nats = [0; 1; 2] );;
    
    assert ( pick 5 fibs = [1; 1; 2; 3; 5] );;
    
    assert ( pick 3 (map increment ones) = [2; 2; 2] );;
    
    assert ( pick 3 (map double nats) = [0; 2; 4] );;
    
    assert ( pick 3 (filter evens nats) = [0; 2; 4] );;
    
    assert ( pick 3 (filter odds nats) = [1; 3; 5] );;
    
   

(* Functions on two thunk streams *)

    let rec map2 f s t =
        let (Cons (h1, t1), Cons (h2, t2)) = (s, t) 
        in 
        Cons (f h1 h2, fun () -> map2 f (t1 ()) (t2 ()));;

    let rec merge s t =
        let (Cons (h1, t1), Cons (h2, t2)) = (s, t) 
        in 
        Cons (h1, fun () -> Cons (h2, fun () -> merge (t1 ()) (t2 ())));;
    


(* Tests for two thunk streams *)   

    let add = fun x y -> x + y;;

    let twos = map2 add ones ones;;

    assert ( pick 3 twos = [2; 2; 2] );;

    assert ( pick 4 (merge ones twos) = [1; 2; 1; 2] );;





(* LAZY STREAMS *)

(* Instrinsic lazy stream type *)

    type 'a stream = Cons of 'a * 'a stream Lazy.t;;
    


(* Example lazy streams *)

    let rec ones = 
        Cons(1, lazy ones);;

    let nats = 
        let rec inc i =
            Cons (i, lazy (inc (i + 1))) 
        in 
        inc 0;;
    
    let fibs =
        let rec fibsaux i j = 
            Cons(i, lazy (fibsaux j (i + j))) 
        in 
        fibsaux 1 1;;



(* Lazy analogs of the list operations *)

    let head s =
        let Cons (h, _) = s 
        in 
        h;;

    let tail s =
        let Cons (_, t) = s 
        in 
        Lazy.force t;;

    let rec nth i s =
        let Cons (h, t) = s
        in 
        if   i = 0 
        then h 
        else nth (i - 1) (Lazy.force t);;
        
    let rec pick i s =
        if   i < 1 
        then []
        else (head s) :: pick (i - 1) (tail s);;

    let rec map f s =
        let Cons (h, t) = s
        in 
        Cons (f h, lazy (map f (Lazy.force t)));;
    
    let rec filter f s =
        let Cons (h, t) = s 
        in 
        if   f h 
        then Cons (h, lazy (filter f (Lazy.force t)))
        else filter f (Lazy.force t);;



(* Lazy tests *)

    assert ( head ones = 1 );;
    
    assert ( head (tail ones) = 1 );;

    assert ( pick 3 ones = [1; 1; 1] );;
    
    assert ( pick 3 nats = [0; 1; 2] );;
    
    assert ( pick 5 fibs = [1; 1; 2; 3; 5] );;
    
    assert ( pick 3 (map increment ones) = [2; 2; 2] );;
    
    assert ( pick 3 (map double nats) = [0; 2; 4] );;
    
    assert ( pick 3 (filter evens nats) = [0; 2; 4] );;
    
    assert ( pick 3 (filter odds nats) = [1; 3; 5] );;



(* Functions on two lazy streams *)

    let rec map2 f s t =
        let lf = Lazy.force in
        let (Cons (h1, t1), Cons (h2, t2)) = (s, t) 
        in 
        Cons (f h1 h2, lazy (map2 f (lf t1) (lf t2)));;

    let rec merge s t =
        let lf = Lazy.force in
        let (Cons (h1, t1), Cons (h2, t2)) = (s, t) 
        in 
        Cons (h1, lazy (Cons (h2, lazy (merge (lf t1) (lf t2)))));;
    
    

(* Tests for two thunk streams *)   

    let add = fun x y -> x + y;;

    let twos = map2 add ones ones;;

    assert ( pick 3 twos = [2; 2; 2] );;

    assert ( pick 4 (merge ones twos) = [1; 2; 1; 2] );;





(* MONAD INTRODUCTION *)

(* An optionalized division function *)

    let divopt a b = 
        if   b = 0. 
        then None 
        else Some (a /. b);;



(* Example execution *)

    divopt 5. 2.;;



(* Fails with repeat application *)

    divopt (divopt 5. 2.) 2.;;

    

(* Introduce monadic functions bind and return *)

    let bind m f =
        match m with
            | Some x -> f x
            | None   -> None;;

    let ret x = Some x;;
    
    

(* Use a monadic variation of the division function *)

    let divopt a b =
        bind a 
             (fun x -> bind b 
                            (fun y -> if   y = 0. 
                                      then None 
                                      else ret (x /. y)));;
    
    

(* Rephrase the division again *)

    divopt (ret 5.) (ret 2.);;
    
    divopt (divopt (ret 5.) (ret 2.)) (ret 2.);;

    divopt (divopt (ret 5.) (ret 0.)) (ret 2.);;
   
 

(* Using infix bind combinator and rephrase *)

    let (>>=) = bind;;
    
    let f x y = if   y = 0. 
                then None 
                else ret (x /. y);;
    
    let divopt a b =
        a >>= (fun x -> 
        b >>= (fun y ->
        f x y));;
        
        
        
(* Division still works *)

    divopt (ret 5.) (ret 2.);;
    
    divopt (divopt (ret 5.) (ret 2.)) (ret 2.);;

    divopt (divopt (ret 5.) (ret 0.)) (ret 2.);;
    
    

(* As a module with lift *)

    module DivOptMonad = 
        struct

            let bind m f =
                match m with
                    | Some x -> f x
                    | None   -> None

            let ret x = Some x

            let (>>=) = bind

            let f x y = if   y = 0. 
                        then None 
                        else ret (x /. y)

            let lift f a b =
                a >>= (fun x ->
                b >>= (fun y ->
                f x y))

            let (//) = lift f

        end;;   

    let (//) = DivOptMonad.(//);;
     
    let r = DivOptMonad.ret;;

    (r 5.) // (r 2.);;

    ( (r 5.) // (r 2.) ) // (r 2.);;

    ( (r 5.) // (r 0.) ) // (r 2.);;



(* Identity monad with application extras *)

    module IdentityMonad = 
        struct

            let bind m f = f m
                
            let ret x = x

            let (>>=) = bind

        end;;   

    let (>>=) = IdentityMonad.(>>=);;
    
    let add_n n = fun x -> IdentityMonad.ret (x + n);;
    
    let add5 = add_n 5;;
    
    1 >>= add5;;
    
    1 >>= add5 >>= add5;;
    


(* List monad with application extras *)
   
    module ListMonad =
        struct

            let bind m f =
                List.concat (List.map f m)

            let ret x = [x]

            let (>>=) = bind

        end;; 

    let (>>=) = ListMonad.(>>=);;

    let add_n n = fun x -> ListMonad.ret (x + n);;

    let add5 = add_n 5;;

    [1; 2] >>= add5;;

    [1; 2] >>= add5 >>= add5;;



(* Basic IO monad *)

    module PrintfMonad = 
        struct

            let bind m f = 
                let _ = Printf.printf "%s " m
                in 
                f

            let ret x = x

            let (>>=) = bind

        end;;

    let (>>=) = PrintfMonad.(>>=);;

    let ret = PrintfMonad.ret;;
    
    "Thanks" >>= "for" >>= "coming" >>= "everyone!" >>= ret;;
    
