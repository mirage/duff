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

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type index
(** The type of the index. *)

val pp_index : index Fmt.t
(** Pretty-printer of {!index}. *)

val memory_size : index -> int
(** [memory_size t] returns how many word(s) is needed to store [t]. *)

val make: ?copy:bool -> bigstring -> index
(** [make ?copy raw] returns a Rabin's fingerprint of [raw]. [?copy] signals to
   copy the input buffer [raw] or not because [make] expects to take the
   ownership of [raw].

   So, if the user want to change (by side-effect) [raw] then, returned value by
   [make] is not valid anymore. However, if [copy] is true, [make] will allocate
   a new {!Cstruct.t} and copy [raw] to this new {!Cstruct.t}. *)

(** The type of the compression. *)
type hunk =
  | Copy of (int * int)
  (** It's the copy {i opcode} to copy the byte range from the source to the
     target. *)
  | Insert of (int * int)
  (** It's the insert {i opcode} to keep a specific byte range of the target. *)

val pp_hunk : hunk Fmt.t
(** Pretty-printer of {!hunk}. *)

val delta: index -> bigstring -> hunk list
(** [delta index trg] returns a compression list between the Rabin's fingerprint
   of a source with the target [trg]. *)
