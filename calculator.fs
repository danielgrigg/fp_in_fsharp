// 6.8

type Instruction =
  | Add
  | Sub
  | Mul
  | Div
  | Sin
  | Cos
  | Log
  | Exp
  | Push of float

type Stack = Stack of float list


let executeBinaryOp op sm = 
  match sm with  
  | Stack (a::b::rs) -> (op b a)::rs |> Stack
  | _ -> failwith "binary instruction overflow"

let executeUnaryOp op sm =
  match sm with
  | Stack (a::rs) -> (op a)::rs |> Stack
  | _ -> failwith "unary instruction overflow"

let executePush r (Stack rs) = Stack (r::rs)
  
let executeInstruction (sm:Stack) (op:Instruction) :Stack = 
  match op with
  | Add -> executeBinaryOp ( + ) sm
  | Sub -> executeBinaryOp ( - ) sm
  | Mul -> executeBinaryOp ( * ) sm
  | Div -> executeBinaryOp ( / ) sm
  | Sin -> executeUnaryOp sin sm
  | Cos -> executeUnaryOp cos sm
  | Log -> executeUnaryOp log sm
  | Exp -> executeUnaryOp exp sm
  | Push r -> executePush r sm

type Program = Instruction list
let executeProgram (instructions:Program) :float =
  let (Stack rs) = List.fold executeInstruction (Stack []) instructions
  List.head rs


let sm0 = Stack [1.0..2.0..9.0]
let sm1 = Stack [0.0..(3.14/4.0)..3.14]

let prog0 = [Push 2.0; Push 3.0; Add]
let prog1 = [Push 2.0; Push 3.0; Push -7.0; Add; Mul]
