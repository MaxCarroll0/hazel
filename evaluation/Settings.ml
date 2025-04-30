open Haz3lcore

let settings = CoreSettings.on (* Note: search off *)

(* The current version of Hazel does not correctly parse any variable starting with "in" when inside a let expression *)
(* I delete any in[a-Z,0-9] to temporarily get around this *)
(* This must also be performed on the data, see ParseData module *)
let replace_inC = Re.replace ~all:true (Re.Perl.compile_pat "in([a-zA-Z0-9])") ~f:(fun g -> Re.Group.get g 1) ;;

(* Add list functions. Note: no implementations given for these in default environment yet *)
let ctx : Ctx.t =
  Builtins.ctx_init
  |> List.map (function
       | Ctx.VarEntry { name; id; typ } ->
           Ctx.VarEntry { name = replace_inC name; id; typ }
       | e -> e)
  |> List.append ([Ctx.VarEntry({name="length"; id=Id.mk(); typ= Forall(Var("A") |> TPat.fresh, Arrow(List(Var("A") |> Typ.fresh) |> Typ.fresh, Int |> Typ.fresh) |> Typ.fresh)|> Typ.fresh |> TypSlice.t_of_typ_t});
  VarEntry({name="hd"; id=Id.mk(); typ= Forall(Var("A") |> TPat.fresh, Arrow(List(Var("A") |> Typ.fresh) |> Typ.fresh, Var("A") |> Typ.fresh) |> Typ.fresh)|> Typ.fresh |> TypSlice.t_of_typ_t});
  VarEntry({name="tl"; id=Id.mk(); typ= Forall(Var("A") |> TPat.fresh, Arrow(List(Var("A") |> Typ.fresh) |> Typ.fresh, List(Var("A") |> Typ.fresh) |> Typ.fresh) |> Typ.fresh)|> Typ.fresh |> TypSlice.t_of_typ_t});
  VarEntry({name="rev"; id=Id.mk(); typ= Forall(Var("A") |> TPat.fresh, Arrow(List(Var("A") |> Typ.fresh) |> Typ.fresh, List(Var("A") |> Typ.fresh) |> Typ.fresh) |> Typ.fresh)|> Typ.fresh |> TypSlice.t_of_typ_t});
  VarEntry({name="mem"; id=Id.mk(); typ= Forall(Var("A") |> TPat.fresh, Arrow(List(Var("A") |> Typ.fresh) |> Typ.fresh, Bool |> Typ.fresh) |> Typ.fresh)|> Typ.fresh |> TypSlice.t_of_typ_t});
  VarEntry({name="nth"; id=Id.mk(); typ= Forall(Var("A") |> TPat.fresh, Arrow(List(Var("A") |> Typ.fresh) |> Typ.fresh, Arrow(Int |> Typ.fresh, Var("A") |> Typ.fresh) |> Typ.fresh) |> Typ.fresh)|> Typ.fresh |> TypSlice.t_of_typ_t});VarEntry({name="map"; id=Id.mk(); 
  typ= Forall(Var("A") |> TPat.fresh, 
        Forall(Var("B") |> TPat.fresh,
          Arrow(
            Arrow(Var("A") |> Typ.fresh, Var("B") |> Typ.fresh) |> Typ.fresh,
            Arrow(
              List(Var("A") |> Typ.fresh) |> Typ.fresh,
              List(Var("B") |> Typ.fresh) |> Typ.fresh
            ) |> Typ.fresh
          ) |> Typ.fresh
        ) |> Typ.fresh
      ) |> Typ.fresh |> TypSlice.t_of_typ_t
});

VarEntry({name="filter"; id=Id.mk(); 
  typ= Forall(Var("A") |> TPat.fresh,
        Arrow(
          Arrow(Var("A") |> Typ.fresh, Bool |> Typ.fresh) |> Typ.fresh,
          Arrow(
            List(Var("A") |> Typ.fresh) |> Typ.fresh,
            List(Var("A") |> Typ.fresh) |> Typ.fresh
          ) |> Typ.fresh
        ) |> Typ.fresh
      ) |> Typ.fresh |> TypSlice.t_of_typ_t
});

VarEntry({name="concat"; id=Id.mk(); 
  typ= Forall(Var("A") |> TPat.fresh,
        Arrow(
          List(List(Var("A") |> Typ.fresh) |> Typ.fresh) |> Typ.fresh,
          List(Var("A") |> Typ.fresh) |> Typ.fresh
        ) |> Typ.fresh
      ) |> Typ.fresh |> TypSlice.t_of_typ_t
});

VarEntry({name="fold_left"; id=Id.mk(); 
  typ= Forall(Var("A") |> TPat.fresh,
        Forall(Var("B") |> TPat.fresh,
          Arrow(
            Arrow(Var("A") |> Typ.fresh, Arrow(Var("B") |> Typ.fresh, Var("A") |> Typ.fresh) |> Typ.fresh) |> Typ.fresh,
            Arrow(
              Var("A") |> Typ.fresh,
              Arrow(
                List(Var("B") |> Typ.fresh) |> Typ.fresh,
                Var("A") |> Typ.fresh
              ) |> Typ.fresh
            ) |> Typ.fresh
          ) |> Typ.fresh
        ) |> Typ.fresh
      ) |> Typ.fresh |> TypSlice.t_of_typ_t
});

VarEntry({name="fold_right"; id=Id.mk(); 
  typ= Forall(Var("A") |> TPat.fresh,
        Forall(Var("B") |> TPat.fresh,
          Arrow(
            Arrow(Var("B") |> Typ.fresh, Arrow(Var("A") |> Typ.fresh, Var("A") |> Typ.fresh) |> Typ.fresh) |> Typ.fresh,
            Arrow(
              List(Var("B") |> Typ.fresh) |> Typ.fresh,
              Arrow(
                Var("A") |> Typ.fresh,
                Var("A") |> Typ.fresh
              ) |> Typ.fresh
            ) |> Typ.fresh
          ) |> Typ.fresh
        ) |> Typ.fresh
      ) |> Typ.fresh |> TypSlice.t_of_typ_t
});

VarEntry({name="find"; id=Id.mk(); 
  typ= Forall(Var("A") |> TPat.fresh,
        Arrow(
          Arrow(Var("A") |> Typ.fresh, Bool |> Typ.fresh) |> Typ.fresh,
          Arrow(
            List(Var("A") |> Typ.fresh) |> Typ.fresh,
            Var("A") |> Typ.fresh
          ) |> Typ.fresh
        ) |> Typ.fresh
      ) |> Typ.fresh |> TypSlice.t_of_typ_t
});

VarEntry({name="is_empty"; id=Id.mk(); 
  typ= Forall(Var("A") |> TPat.fresh,
        Arrow(
          List(Var("A") |> Typ.fresh) |> Typ.fresh,
          Bool |> Typ.fresh
        ) |> Typ.fresh
      ) |> Typ.fresh |> TypSlice.t_of_typ_t
});

VarEntry({name="combine"; id=Id.mk(); 
  typ= Forall(Var("A") |> TPat.fresh,
        Forall(Var("B") |> TPat.fresh,
          Arrow(
            List(Var("A") |> Typ.fresh) |> Typ.fresh,
            Arrow(
              List(Var("B") |> Typ.fresh) |> Typ.fresh,
              List(Prod([
                Var("A") |> Typ.fresh;
                Var("B") |> Typ.fresh
              ]) |> Typ.fresh) |> Typ.fresh
            ) |> Typ.fresh
          ) |> Typ.fresh
        ) |> Typ.fresh
      ) |> Typ.fresh |> TypSlice.t_of_typ_t
});

VarEntry({name="split"; id=Id.mk(); 
  typ= Forall(Var("A") |> TPat.fresh,
        Forall(Var("B") |> TPat.fresh,
          Arrow(
            List(Prod([
              Var("A") |> Typ.fresh;
              Var("B") |> Typ.fresh
            ]) |> Typ.fresh) |> Typ.fresh,
            Prod([
              List(Var("A") |> Typ.fresh) |> Typ.fresh;
              List(Var("B") |> Typ.fresh) |> Typ.fresh
            ]) |> Typ.fresh
          ) |> Typ.fresh
        ) |> Typ.fresh
      ) |> Typ.fresh |> TypSlice.t_of_typ_t
});

  ])
