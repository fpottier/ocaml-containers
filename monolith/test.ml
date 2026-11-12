(******************************************************************************)
(*                                                                            *)
(*                                  Skeleton                                  *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

open Monolith

(* This is the reference implementation. *)
module R = Reference

(* This is the candidate implementation. *)
module C = CCRAL

(* -------------------------------------------------------------------------- *)

(* The abstract type of random access lists. *)

(* This type is equipped with a well-formedness check,
   which ignores the model (the reference side). *)

let check _model =
  C.check,
  constant "check"

let t =
  declare_abstract_type ~check ()

(* The type of elements. *)

let elem =
  semi_open_interval 0 16

(* Recognizing a nonempty list. *)

let nonempty xs =
  not (R.is_empty xs)

(* Picking a valid index in a list. *)

let offset xs =
  semi_open_interval 0 (R.length xs)

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

(* The comments indicate operations that exist in the candidate
   implementation [CCRAL] but are currently not tested. *)

let () =

  let spec = t in
  declare "empty" spec R.empty C.empty;

  let spec = t ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = elem ^> t ^> t in
  declare "cons" spec R.cons C.cons;

  let spec = elem ^> t in
  declare "return" spec R.return C.return;

  (* [map] *)
  (* [mapi] *)

  let spec = nonempty % t ^> elem in
  declare "hd" spec R.hd C.hd;

  let spec = nonempty % t ^> t in
  declare "tl" spec R.tl C.tl;

  (* [front] *)
  (* [front_exn] *)

  let spec = t ^> int in
  declare "length" spec R.length C.length;

  (* [get] *)

  let spec = t ^>> fun xs -> offset xs ^> elem in
  declare "get_exn" spec R.get_exn C.get_exn;

  let spec = t ^>> fun xs -> offset xs ^> elem ^> t in
  declare "set" spec R.set C.set;

  (* [remove] *)
  (* [get_and_remove_exn] *)

  let spec = t ^> t ^> t in
  declare "append" spec R.append C.append;

  (* [filter] *)
  (* [filter_map] *)
  (* [flat_map] *)
  (* [flatten] *)
  (* [app] *)

  let spec = t ^>> fun xs -> semi_open_interval 0 (R.length xs + 16) ^> t in
  declare "Fun.flip take" spec (Fun.flip R.take) (Fun.flip C.take);

  (* [take_while] *)

  let spec = t ^>> fun xs -> semi_open_interval 0 (R.length xs + 16) ^> t in
  declare "Fun.flip drop" spec (Fun.flip R.drop) (Fun.flip C.drop);

  (* [drop_while] *)
  (* [take_drop] *)
  (* [iter] *)
  (* [iteri] *)
  (* [fold] *)
  (* [fold_rev] *)
  (* [rev_map] *)
  (* [rev] *)
  (* [equal] *)
  (* [compare] *)
  (* [make] *)
  (* [repeat] *)
  (* [range] *)
  (* [add_list] *)
  (* [of_list] *)
  (* [to_list] *)
  (* [of_list_map] *)
  (* [of_array] *)
  (* [add_array] *)
  (* [to_array] *)
  (* [add_iter] *)
  (* [of_iter] *)
  (* [to_iter] *)
  (* [add_gen] *)
  (* [of_gen] *)
  (* [to_gen] *)
  (* [Infix] *)
  (* [pp] *)

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let prologue () =
    dprintf "          open CCRAL;;\n";
    ()
  in
  let fuel = 16 in
  main ~prologue fuel
