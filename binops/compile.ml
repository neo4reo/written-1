open Printf

type reg =
	| EAX
	| ESP

type arg =
  | Const of int
  | Reg of reg
  | RegOffset of int * reg

type instruction =
	| IMov of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg
  | ILabel of string
  | ICmp of arg * arg
  | IJne of string
  | IJmp of string
	| IRet

type prim1 =
  | Add1
  | Sub1

type prim2 =
  | Plus
  | Minus
  | Times

type expr =
  | ELet of (string * expr) list * expr
  | EPrim1 of prim1 * expr
  | EPrim2 of prim2 * expr * expr
  | EIf of expr * expr * expr
  | ENumber of int
  | EId of string

let count = ref 0
let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

let r_to_asm (r : reg) : string =
	match r with
		| EAX -> "eax"
		| ESP -> "esp"

let arg_to_asm (a : arg) : string =
  match a with
    | Const(n) -> sprintf "%d" n
    | Reg(r) -> r_to_asm r
    | RegOffset(n, r) ->
      if n >= 0 then
        sprintf "[%s+%d]" (r_to_asm r) n
      else
        sprintf "[%s-%d]" (r_to_asm r) (-1 * n)

let i_to_asm (i : instruction) : string =
	match i with
		| IMov(dest, value) ->
			sprintf "  mov %s, %s" (arg_to_asm dest) (arg_to_asm value)
		| IAdd(dest, to_add) ->
			sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm to_add)
		| ISub(dest, to_sub) ->
			sprintf "  sub %s, %s" (arg_to_asm dest) (arg_to_asm to_sub)
		| IMul(dest, to_mul) ->
			sprintf "  imul %s, %s" (arg_to_asm dest) (arg_to_asm to_mul)
    | ICmp(left, right) ->
      sprintf "  cmp %s, %s" (arg_to_asm left) (arg_to_asm right)
    | ILabel(name) ->
			sprintf "%s:" name
    | IJne(label) ->
      sprintf "  jne %s" label
    | IJmp(label) ->
      sprintf "  jmp %s" label
		| IRet ->
			"	ret"

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (i_to_asm i)) "" is

let rec find ls x =
  match ls with
    | [] -> None
    | (y,v)::rest ->
      if y = x then Some(v) else find rest x

let rec compile_expr (e : expr) (si : int) (env : (string * int) list) : instruction list =
  match e with
    | EPrim1(op, arge) ->
      let argis = compile_expr arge si env in
      let do_op = match op with
        | Add1 -> IAdd(Reg(EAX), Const(1))
        | Sub1 -> ISub(Reg(EAX), Const(1))
      in
      argis @ [do_op]
    | EPrim2(op, el, er) ->
      let elis = compile_expr el si env in
      let save_elis = IMov(RegOffset(-4 * si, ESP), Reg(EAX)) in
      let eris = compile_expr er (si + 1) env in
      let rhs_loc = RegOffset(-4 * (si + 1), ESP) in
      let save_eris = IMov(rhs_loc, Reg(EAX)) in
      let do_op = match op with
        | Plus -> IAdd(Reg(EAX), rhs_loc)
        | Minus -> ISub(Reg(EAX), rhs_loc)
        | Times -> IMul(Reg(EAX), rhs_loc)
      in
      elis @ [save_elis] @ eris @ [save_eris] @
      [
        IMov(Reg(EAX), RegOffset(-4 * si, ESP));
        do_op
      ]
    | EId(x) -> 
      begin match find env x with
        | Some(stackloc) -> [IMov(Reg(EAX), RegOffset(-4 * stackloc, ESP))]
        | None -> failwith ("Unbound identifier" ^ x)
      end
    | EIf(cond, thn, els) ->
      let condis = compile_expr cond si env in
      let thnis = compile_expr thn si env in
      let elsis = compile_expr els si env in
      let else_label = gen_temp "else" in
      let after_label = gen_temp "after" in
      condis @
      [
        ICmp(Reg(EAX), Const(0));
        IJne(else_label);
      ] @
      thnis @
      [
        IJmp(after_label);
        ILabel(else_label);
      ] @
      elsis @
      [
        ILabel(after_label);
      ]
    | ENumber(n) -> [IMov(Reg(EAX), Const(n))]
    | ELet([], body) ->
      compile_expr body si env
    | ELet((x, ex)::binds, body) ->
      let exis = compile_expr ex si env in
      let bodyis = compile_expr (ELet(binds, body)) (si + 1) ((x, si)::env) in
      exis @
      [
        IMov(RegOffset(-4 * si, ESP), Reg(EAX))
      ] @
      bodyis
      

let compile_to_string prog =
  let prelude =
    "section .text
global our_code_starts_here
our_code_starts_here:" in
  let compiled = (compile_expr prog 1 []) in
  let as_assembly_string = (to_asm (compiled @ [IRet])) in
	sprintf "%s%s\n" prelude as_assembly_string

