let isIthChar (str:string) i ch = str.[i] = ch

printfn "%A" (isIthChar "foo" 1 'o')
printfn "%A" (isIthChar "foo" 1 'x')

let rec occFromIth (str:string) i ch =
  if (i >= str.Length) then 0
  elif (isIthChar str i ch) then 1 + (occFromIth str (i+1) ch)
  else (occFromIth str (i+1) ch)

printfn ": %A" (occFromIth "abc" 5 'c')
printfn ": %A" (occFromIth "abc" 2 'c')
printfn ": %A" (occFromIth "abc" 1 'c')
printfn ": %A" (occFromIth "abcdefabcabc" 1 'c')
printfn ": %A" (occFromIth "abcdefabcabc" 3 'c')

let occInString str c = occFromIth str 0 c

printfn "OccInString abcaabcccd a: %A" (occInString "abcaabcccd" 'a')
printfn "OccInString abcaabcccd b: %A" (occInString "abcaabcccd" 'b')
printfn "OccInString abcaabcccd c: %A" (occInString "abcaabcccd" 'c')
printfn "OccInString abcaabcccd d: %A" (occInString "abcaabcccd" 'd')


let notDivisible d n = n % d <> 0

printfn "notDivisible 2 5 %A" (notDivisible 2 5)
printfn "notDivisible 3 9 %A" (notDivisible 3 9)

let rec test a b c = 
  match (a,b,c) with 
  | (a,b,c) when (a <= b) -> (notDivisible a c) && (test (a+1) b c)
  | _ -> true 

let rec test2 a b c =
  if (a <= b) then (notDivisible a c) && (test2 (a+1) b c)
  else true

printfn "test 2 2 2: %A" (test 2 2 2)
printfn "test 2 2 5: %A" (test 2 2 5)
printfn "test 3 5 13: %A" (test 3 5 13)

printfn "test2 3 3 8: %A" (test2 3 3 8)
printfn "test2 3 4 8: %A" (test2 3 4 8)

let rec prime = function
  | 1 | 2 -> true
  | n when (n > 1) -> test 2 (n-1) n
  | _ -> false

let rec nextPrime = function
  | n when (prime (n+1)) -> n+1
  | n -> nextPrime (n+1)

let rec bin = function
  | (n, k) when (n = k || k = 0) -> 1
  | (n, k) when (n <> 0 && k <> 0 && n > k) -> bin (n-1,k-1) + bin (n-1, k)
  | _ -> 0 

let rec f = function
  | (0,y) -> y
  | (x,y) -> f(x-1, x*y);;


let VAT n x = x * (1.0 + 0.01 * (float n))
let unVAT n x = x / (1.0 + 0.01 * (float n))

let min f = 
  let rec min2 f n = if (f n = 0) then n else min2 f (n+1)
  min2 f 1

let curry f x y = f (x,y) 
let uncurry g (x,y)  = g x y

let (<<.) (h1,m1,f1) (h2,m2,f2) = 
  match (f1,f2) with
    | ("AM","AM") -> (h1,m1) < (h2,m2)
    | ("PM","PM") -> (h1,m1) < (h2,m2)
    | ("AM","PM") -> true
    | ("PM","AM") -> false
    | _ -> failwith "error"


type Time = { f:string; h:int; m:int }

let (<.) x y = 
  match (x.f,y.f) with
    | ("AM","AM") -> x < y
    | ("PM","PM") -> x < y
    | ("AM","PM") -> true
    | ("PM","AM") -> false
    | _ -> failwith "error"

type Time2 = 
  | AM of hour:int * minute:int
  | PM of hour:int * minute:int

let timeLessThan t1 t2 =
  match (t1, t2) with 
  | (AM (h1,m1), AM (h2,m2)) -> (h1,m1) < (h2,m2)
  | (PM (h1,m1), PM (h2,m2)) -> (h1,m1) < (h2,m2)
  | (AM _, PM _) -> true
  | (PM _, AM _) -> false


type Currency = { pounds:int; shillings:int; pence:int}
type Currency2 = | Currency2 of pounds:int * shillings:int
type Currency3 = int*int*int


let addCurrency c1 c2 =
  let pence (a,b,c) = a*20*12 + b*12 + c
  let shilling p = p / 12
  let pound p = (shilling p ) / 20
  let penceSum = pence c1 + pence c2 
  (pound penceSum, shilling penceSum % 20, penceSum % 12)

let addCurrency2 c1 c2 =
  let pence x = x.pounds*20*12 + x.shillings*12 + x.pence
  let shillings x = x / 12
  let pounds x = (shillings x) / 20
  let penceSum = pence c1 + pence c2
  {pounds = pounds penceSum; 
   shillings = shillings penceSum % 20;
   pence = penceSum % 12}

let (.+.) (a:float,b:float) (c:float,d:float) = (a + c, b + d)
let (.*.) (a:float,b:float) (c:float,d:float) = (a*c - b*d, b*c + a*d)
let (-.) (a:float,b:float) = (-a,-b)
let (.-.) x y = x .+. (-.) y
let (./.) x (a:float,b:float) = 
  let l = a*a + b*b
  x .*. (a/l, -b/l) 


type StraightLine = { a:float; b:float }
let mirrorY l = { a = -l.a; b = l.b }
let str l = sprintf "y = %fx + %f" l.a l.b

type Solution = 
  | TwoRoot of float*float
  | OneRoot of float
  | NoRoot

type Equation = float*float*float

let solve (a,b,c) =
  let d = b*b - 4.0*a*c
  if d < 0.0 || a = 0.0 then NoRoot
  else 
    let sqrtD = sqrt d
    if sqrtD =  0.0 then OneRoot (-b / (2.0*a))
    else TwoRoot ((-b + sqrtD)/(2.0*a), (-b - sqrtD)/(2.0*a))
 

type Shape = 
  | Circle of radius:float
  | Square of width:float
  | Triangle of a:float * b:float * c:float

let area = function
  | Circle r when r > 0.0 -> System.Math.PI * r * r
  | Square w when w > 0.0 -> w * w
  | Triangle (a,b,c) 
    when a > 0.0 && b > 0.0 && c > 0.0 && 
         a < b + c && b < c + a && c < a + b -> 
    let s = (a + b + c) / 2.0
    sqrt(s * (s-a) * (s-b)*(s-c));;

// cheapter 4

let upto x = [1..x]
let downto1 n = [n..(-1)..1]

let even n = n % 2 = 0
let evenN n = [for n in 1..n do if even n then yield n]

let rec altsum = function
  | [] -> 0
  | [x] -> x
  | x0::xs -> if (xs.IsEmpty) then x0 else x0 - xs.Head + altsum xs.Tail;;

//
let rec rmodd = function
  | [] -> []
  | [x] -> [x]
  | x0::x1::xs -> x0::(rmodd xs)

let rec revens = function
  | [] -> []
  | x0::xs -> if even x0 then revens xs else x0::(revens xs)
 
let rec multiplicity n xs = 
  let rank x n' = if (x = n') then 1 else 0
  match xs with 
  | [] -> 0
  | [x] -> (rank x n)
  | x::xs' -> (rank x n) + (multiplicity n xs')

let rec split = function
  | [] -> ([],[])
  | [x] -> ([x],[])
  | x0::x1::xs -> let ys,ys' = split xs
                  (x0::ys, x1::ys')

let rec zip = function
  | ([], []) -> []
  | ([x],[y]) -> [(x,y)]
  | (x::xs,y::ys) -> (x,y)::(zip (xs, ys))
  | _ -> failwith "list length mismatch"

let rec prefix = function
  | ([x], []) -> false
  | (x::xs, y::ys) -> x = y && (prefix (xs, ys))
  | _ -> true


// 4.11

let rec count xs x = 
  match xs with
  | [] -> 0
  | (x'::xs') when x' = x -> 1 + (count xs' x)
  | (_::xs') -> count xs' x

// [1;2;4;4;7;8] 3''
// [3] 2 -> [2 3]
// [2] 3
// [2;4] 1
// [2;4] 3
// [2;4] 
let rec insert xs x =
  match xs with
  | [] -> [x]
  | (y::ys) when (x <= y) -> x::y::ys
  | (y::ys) when (x > y) -> y::(insert ys x)
  | _ -> failwith (sprintf "matched %A %d" xs x)

let rec insert2 xs x =
  match xs with
  | [] -> [x]
  | (y::ys) -> if (x <= y) then x::y::ys else y::(insert ys x)
  
// [2] []
// [3;6] [6]
// [3;4] [4]
// [4;5;6] [3;6]
let rec intersect = function
  | ([],[]) -> []
  | (xs,[]) -> []
  | ([],ys) -> []
  | (x::xs,y::ys) when (x = y) -> x::(intersect (xs,ys))
  | (x::xs,y::ys) when (x < y) -> intersect(xs,y::ys)
  | (x::xs,y::ys) when (x > y) -> intersect(x::xs, ys)
  | _ -> []

let rec intersect2 = function
  | (x::xs,y::ys) when (x < y) -> intersect(xs,y::ys)
  | (x::xs,y::ys) when (x > y) -> intersect(x::xs, ys)
  | (x::xs,y::ys) -> x::(intersect (xs,ys))
  | _ -> []

let rec intersect3 = function
  | (x::xs,y::ys) -> if (x = y)   then x::(intersect (xs,ys))
                     elif (x < y) then intersect(xs,y::ys)
                     else              intersect(x::xs,ys)
  | _ -> []

[ ([2],[]); 
  ([2],[2]); 
  ([3;6],[6]);
  ([3;4],[4]);
  ([4;5;6],[3;6]);
  ([1;1;1;2;2],[1;1;2;4])
] 
|> List.map (fun x -> printfn "intersect %A: %A" x (intersect x))
|> ignore



// [1;1;2] [1;2;4] -> [1;1;1;2;2;4]
let rec plus = function
  | (x::xs,y::ys) -> x::y::(plus (xs,ys))
  | _ -> []

// [1;1;1;2;2] [1;1;2;3] -> [1;2]
// [1;2;2] [2;3] 
// [1;1;2;3] [1;1;1;2;2] -> [3]
// [2;3] [1;2;2]
let rec minus = function
  | (x::xs,y::ys) when (x < y) -> x::(minus (xs,y::ys))
  | (x::xs,y::ys) when (x > y) -> (minus (x::xs,ys))
  | (x::xs,y::ys)              -> minus (xs,ys)
  | (xs,[]) -> xs
  | _ -> []

// 4.12
//
let rec sum (p, xs) = 
  match xs with
  | [] -> 0
  | y::ys when p(y) -> y + sum(p,ys)
  | _::ys -> sum(p,ys)

// 4.13

let min xs =
  let rec minWith m ys =
    match ys with
    | [] -> m
    | y'::ys' when y' < m -> minWith y' ys'
    | _::ys' -> minWith m ys'
  match xs with
  | [] -> failwith "empty list"
  | x'::xs' -> minWith x' xs'
  
let rec delete (a,xs) = 
  match xs with
  | y::ys when y = a -> ys
  | y::ys -> y::(delete (a,ys))
  | _ -> []

// [3; 1; 2; 7; 5] -> [1;2;3;5;7]

let naiveSort xs =
  let rec naiveSort' ys zs =
    match zs with
    | [] -> ys
    | zs' -> 
      let m = min zs'
      naiveSort' (ys@[m]) (delete (m,zs'))
  naiveSort' [] xs

let smallest ys = 
  let rec smallest' = function
  | (n,[]) -> n
  | (n,x::xs) when x < n -> smallest' (x,xs)
  | (n,x::xs) -> smallest' (n,xs)
  match ys with 
  | [] -> None
  | y'::ys' -> Some (smallest' (y', ys'))

let rec rev = function
  | [] -> []
  | x::xs -> (rev xs)@[x]

let rec revrev = function
  | [] -> []
  | x::xs -> (revrev xs)@[rev x]


// 4.22
//

type Polynomial = float list

let polynomialString (p:Polynomial) = 
  let rec polynomialString' (p:Polynomial) n = 
    let term (k,n) = (string k) + "x^" + (string n)
    match p with
    | k1::k2::ks -> (term (k1,n)) + " + " + (polynomialString' (k2::ks) (n+1))
    | [k] -> term (k,n)
    | _ -> ""
  polynomialString' p 0


let (.*.) (k:float) (p:Polynomial) = 
  let rec multiply (p':Polynomial) = 
    match p' with 
    | (c::cs) -> (k * c)::(multiply cs)
    | [] -> []
  multiply p

// a0 + a1x + a2x^2 + ... = [a2 a1 a0]
// x * (a0 + a1x + a2x^2) = [a2 a1 a0 0]
let rec multiplyByX (q:Polynomial) = 0.0::q


// (a0 + a1x1 + a2x^2) + (b0 + b1x1 + b2x^2) = (a0 + b0 + (a1+b1)x1 + ...
let (.+.) (p:Polynomial) (q:Polynomial) =
  let rec addPolynomial = function
  | (x::xs,y::ys) -> (x + y)::(addPolynomial (xs,ys))
  | (x,[]) -> x
  | ([],y) -> y
  addPolynomial(p,q)

let rec multiplyPolynomial (p:Polynomial) (q:Polynomial) :Polynomial  = 
  match (p,q) with
  | ([], Q) -> 0.0 .*. Q
  | (x::xs,Q) -> (x .*. Q) .+. (multiplyByX (multiplyPolynomial xs Q))

let p0 = [2.0; 1.0]
let q0 = [0.0; 2.0]


//(2 + x) * (0 + 2x) = 4x + 2x^2
// (2 + 3x) * (1 + 2x) = (2 + 7x + 6x^2)
let p1,q1 = [2.0; 3.0], [1.0;2.0]


type Name = string
type Phone = string
type Sex = | Male | Female 
type BirthYear = int
type Interest = string
type InterestList = Interest list

type Client = Name * Phone * Sex * BirthYear * InterestList

let clientdb = [
  ("Alice", "0412743999", Female, 1980, ["Climbing"; "Walking"; "Dancing"])
  ("Bob", "0425111332", Male, 1978, ["Climbing"])
  ("Eve", "0444222333", Female, 1984, ["Dancing"; "Yoga"])
  ("Alex", "0425454876", Male, 1975, ["Reading"; "Sailing"])
  ("OldJoe", "0123888777", Male, 1969, ["Reading"; "Walking"])
  ("OldClare", "0442787484", Female,1972 , ["Walking"; "Climbing"])
]

let profile (_, _, sex, dob, interests) = (sex, dob, interests)

let sharedInterests mine theirs = 
  let rec hasInterest = function
    | x::xs -> (List.exists (fun elem -> elem = x) theirs) || (hasInterest xs)
    | _ -> false
  hasInterest mine

let compatible client1 client2 = 
  let (sex1,dob1,interests1), (sex2, dob2, interests2) = 
    (profile client1, profile client2)
  sex1 <> sex2 && 
    (abs (dob1 - dob2)) < 10 && 
    (sharedInterests interests1 interests2)

let rec matchme (me:Client) (clients:Client list) = 
  match clients with 
  | c::cs when (compatible me c) -> c::(matchme me cs)
  | c::cs -> matchme me cs
  | _ -> []


let foldFilter (p:'a -> bool) (xs:'a list) = 
  List.foldBack (fun elem -> (fun acc -> if (p elem) then elem::acc else acc)) xs []

let foldRev xs = List.fold (fun acc elem -> elem::acc) [] xs
let foldRevRev xs = 
  List.fold (fun acc elem -> (foldRev elem)::acc) [] xs

let downto1 f (n:int) e = 
  List.fold (fun acc n -> f n acc )  e [n..(-1)..1]

let factorial2 n = downto1 (fun n' acc -> n' * acc) n 1

let gbuilder g n = downto1 (fun n' acc -> (g n')::acc) n []

let crossproduct l1 l2 =
  seq { for el1 in l1 do
    for el2 in l2 do
      yield el1, el2 };;

let dom (r:Set<'a * 'b>) :Set<'a> = Set.map (fun (a,_) -> a) r
let rng (r:Set<'a * 'b>) :Set<'b> = Set.map (fun (_,b) -> b) r

let apply (r:Set<'a * 'b>) (a:'a) :Set<'b> = 
  r |> Set.filter (fun (x,_) -> x = a) |> Set.map (fun (_,y) -> y)

let symmetricClosure (r:Set<'a * 'a>) :Set<'a * 'a> = 
  r |> Set.map (fun (a,b) -> (b,a)) |> Set.union r

let testSet0 = [(1,1);(1,2);(1,3);(2,4);(4,2)] |> set;;

let composition (r:Set<'a * 'b>) (s:Set<'b * 'c>) :Set<'a * 'c> =
  let a2c (a,b) = apply s b |> Set.map (fun sb -> (a,sb))
  Set.fold (fun acc ab -> ab |> a2c |> Set.union acc) Set.empty r

let transitiveClosure (r:Set<'a * 'a>) :Set<'a * 'a> =
  let n = Set.count r  
  let power s n' = List.fold (fun acc _ -> composition acc acc) s [2..n']
  List.fold (fun acc n' -> power r n' |> Set.union acc ) r [2..n]

