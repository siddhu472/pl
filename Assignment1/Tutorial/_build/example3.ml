
type point = Point2D of int*int 

let pt1 : point = Point2D (10,20)
 
let double_point (p : point) : point =
  match p with
    Point2D (x, y) -> Point2D (2 * x, 2 * y)

let add_point (p1 : point) (p2 : point) : point =
  match (p1, p2) with
    (Point2D (x1, y1), Point2D (x2, y2)) -> Point2D (x1 + x2, y1 + y2)

let string_of_point (p : point) : string =
  match p with
    Point2D (x, y) -> "<" ^ string_of_int x ^ ", " ^ string_of_int y ^ ">"
