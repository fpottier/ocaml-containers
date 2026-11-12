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

let empty = []
let is_empty = List.is_empty
let cons x xs = x :: xs
let return x = [x]
let hd = List.hd
let tl = List.tl
let length = List.length
let get_exn = List.nth
let set xs i x = CCList.set_at_idx i x xs
let append = (@)
let take = List.take
let drop = List.drop
