open Ast_mapper;
open Ast_helper;
open Asttypes;
open Parsetree;

let resultMapper = _argv => {
  ...default_mapper,
  module_expr: (mapper, expr) =>
    switch (expr.pmod_desc) {
    | Pmod_extension(({txt: "formality", loc}, pstr)) =>
      switch (pstr) {
      | PStr([
          {
            pstr_desc:
              Pstr_eval(
                {pexp_desc: Pexp_constant(Const_string(_str, _delim))},
                _,
              ),
            _,
          },
        ]) =>
        Mod.mk(Pmod_structure([]))

      | _ =>
        raise(
          Location.Error(
            Location.error(
              ~loc,
              "[%formality] accepts a string, e.g. [%formality {| form schema |}]",
            ),
          ),
        )
      }

    | _ => default_mapper.module_expr(mapper, expr)
    },
};

let () = Ast_mapper.register("re-formality", resultMapper);