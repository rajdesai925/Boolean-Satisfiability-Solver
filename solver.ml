open solver_types;;

(*      ( and ( or a ( not b ) ) ( or b ( not a ) ) )     ["(";"and";"(";"or";"a";"(";"not";"b";")";")";"(";"or";"b";"(";"not";"a";")";")";")"] ;;           *)
(*   ["(";"and";"(";"or";"a";"b";")";"TRUE";")"]   *)

let getTree (input : tree * (string list)) : tree = 
  match input with
  | (tree, lst) -> tree;;

let getList (input : tree * (string list)) : string list = 
  match input with
  | (tree, lst) -> lst;;

let rec s (input : string list) : tree * (string list) = 
  match input with 
  | [] -> (TreeNode ("",[])), []
  | "("::tl -> TreeNode ("S", [(TreeNode ("(", []));getTree (t tl);(TreeNode (")", []))]), getList (t tl)
  | ")"::tl -> s tl
  | "and"::tl | "or"::tl | "not"::tl -> s (getList (t tl))
  | hd::tl -> TreeNode ("S", [(TreeNode (hd, []))]), tl
and t (input :string list) : tree * (string list) =
  match input with
  | [] -> (TreeNode ("",[])), []
  | "not"::tl -> (TreeNode ("T", [(TreeNode ("not", [])); getTree (s tl)])),  (getList (s tl))
  | "and"::tl -> TreeNode ("T", [(TreeNode ("and", [])); getTree (s tl); getTree (s (getList (s tl)))]), getList (s (getList (s tl)))
  | "or"::tl -> TreeNode ("T", [(TreeNode ("or", [])); getTree (s tl); getTree (s (getList (s tl)))]), getList (s (getList (s tl)))
  | hd::tl -> s input;;

let buildParseTree (input : string list) : tree =
  getTree (s input);;
                  
let rec buildAbstractSyntaxTree (input : tree) : tree = 
  match input with
  | TreeNode ("S", (TreeNode (str1, lst1))::(TreeNode (str2, lst2))::tl) -> 
      if str1 = "(" then buildAbstractSyntaxTree (TreeNode (str2, lst2)) 
      else buildAbstractSyntaxTree (TreeNode (str1, lst1))
  | TreeNode ("T", (TreeNode (str1, lst1))::(TreeNode (str2, lst2))::(TreeNode (str3, lst3))::tl) ->
      TreeNode (str1, [(buildAbstractSyntaxTree (TreeNode (str2, lst2))); (buildAbstractSyntaxTree (TreeNode (str3, lst3)))])
  | TreeNode ("T", (TreeNode (str1, lst1))::(TreeNode (str2, lst2))::tl) ->
      TreeNode (str1, [(buildAbstractSyntaxTree (TreeNode (str2, lst2)))])
  | TreeNode ("S", [(TreeNode (str, lst))]) -> (TreeNode (str, lst));;

let is_alpha (input : string) : bool =
    match input with
    | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" 
    | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" -> true 
    | _ -> false;;

let rec getVars (input: string list) : string list =
    match input with
    | [] -> []
    | hd::tl -> if is_alpha hd then hd::(getVars tl)
                else getVars tl;;

let rec scanVariable (input : string list) : string list = 
  match (getVars input) with
  | [] -> []
  | hd::tl -> if List.mem hd tl then (scanVariable tl) else hd::(scanVariable tl);; 
        
let rec buildBoolList (size : int) (boolList: bool list) : bool list =
  if size <= 0 then boolList else buildBoolList (size-1) (false::boolList);;

let generateInitialAssignList (varList : string list) : (string * bool) list =
  List.combine varList (buildBoolList (List.length varList) []);;      

let rec getCarry (assignList : (string * bool) list) : bool =
  match assignList with 
  | [] -> true
  | (var, b)::tl -> if (var, b) = (var, true) then getCarry tl else false;;  

let rec increment (revList : (string * bool) list) : (string * bool) list = 
  match revList with
  | [] -> []
  | (var, b)::tl -> if (var, b) = (var, true) then (var, false):: increment tl else (var, true)::tl;;
  
let generateNextAssignList (assignList : (string * bool) list) : (string * bool) list * bool = 
  let carry = getCarry assignList in
  List.rev (increment (List.rev assignList)), carry ;; 

let rec lookupVar (assignList : (string * bool) list) (str : string) : bool = 
  match assignList with
  | [] -> false 
  | (var, b)::tl -> if var = str then b else lookupVar tl str;;
  
let rec evaluateTree (t : tree) (assignList : (string * bool) list) : bool = 
  match t with
  | TreeNode ("and", x::y::[]) -> evaluateTree x assignList && evaluateTree y assignList
  | TreeNode ("or", x::y::[]) -> evaluateTree x assignList || evaluateTree y assignList
  | TreeNode ("not", x::[]) -> not (evaluateTree x assignList)
  | TreeNode ("TRUE", []) -> true
  | TreeNode ("FALSE", []) -> false
  | TreeNode (var, []) -> lookupVar assignList var;;
  
let rec getAssignments (tree : tree) (assignList : (string * bool) list) : (string * bool) list list =
  let eval = if (evaluateTree tree assignList) = true then assignList else [] in
  let next = generateNextAssignList assignList in
  match next with
  | (lst, false) -> eval::(getAssignments tree lst)
  | (lst, true) -> [eval];;

let rec removeEmpty (input : (string * bool) list list) : (string * bool) list list =
  match input with
  | [] -> []
  | hd::tl -> if hd = [] then removeEmpty tl else hd::(removeEmpty tl);;

let satisfiable (input : string list) : (string * bool) list list = 
  let tree = buildAbstractSyntaxTree (buildParseTree input) in
  let assignList = generateInitialAssignList (scanVariable input) in
  let withEmpty = getAssignments tree assignList in
  removeEmpty withEmpty;;
