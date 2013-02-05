(**
  * A Control Flow Graph built from an AST.
  *)

(**
  * A node which has a list of predecessors, a statement and a list of successors
	*)

type node =
	| StmNode of node list * stm * node list
	| ExpNode of node list * stm * node list


(**
  * A function to build a control flow graph from a program.
	* The returned node is the root-element of the CFG.
	*)

let make_cfg (prog: Ast.program): node =
	Error.tip_not_implemented_yet "cfg.ml" "Control Flow Graph not yet implemented."