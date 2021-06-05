all: solver_types.cmo solver.cmo

solver_types.cmi: solver_types.mli
	ocamlc -c solver_types.mli

solver_types.cmo: solver_types.ml solver_types.cmi
	ocamlc -c solver_types.ml

solver.cmi: solver.mli solver_types.cmi
	ocamlc -c solver.mli

solver.cmo : solver.ml solver.mli solver_types.cmo solver.cmi
	ocamlc -c solver.ml

test: solver_types.cmo solver.cmo
	ocaml solver_types.cmo solver.cmo solver_test.ml
