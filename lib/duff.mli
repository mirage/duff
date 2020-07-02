(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Rabin's fingerprint and diff algorithm. *)

module Bigarray = Bigarray_compat
type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type index
(** The type of the index. *)

val pp_index : index Fmt.t
(** Pretty-printer of {!index}. *)

val make : bigstring -> index
(** [make source] returns a fingerprint of [source]. *)

(** The type of the compression. *)
type hunk =
  | Copy of (int * int)
  (** It's the copy ([(off, len)]) {i opcode} to copy {i len} byte(s) range from
     the source at {i off} to the target. *)
  | Insert of (int * int)
  (** It's the insert ([off, len]) {i opcode} to keep a specific byte range of
     {i len} bytes of the target at {i off}. *)

val pp_hunk : hunk Fmt.t
(** Pretty-printer of {!hunk}. *)

val delta: index -> source:bigstring -> target:bigstring -> hunk list
(** [delta index ~source ~target] returns a compression list between the Rabin's
   fingerprint of a [source] [index] with the target [target].

   {b Note.}

   The given [source] must be the same (not necessary physically) than the
   source used to produce [index] with {!make}. *)
