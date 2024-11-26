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

(** Rabin's fingerprint and diff algorithm.

    Let's take a random buffer [a]. We can produce from it an {i index} which is
    a (lightweight) simple table of occurences ([string]) found into [a].

    From this {i index}, we can get a {i diff} with another random buffer [b]
    such as:

    {[
      let index = make a in
      let diff = delta index ~source:a ~target:b in
      assert (apply a diff = b)
    ]}

    A {i diff} is a list of {!hunk}. [apply] is a simple function which needs
    the source [a] to reconstruct [b]. *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type index
(** The type of the index. *)

val pp_index : Format.formatter -> index -> unit
(** Pretty-printer of {!index}. *)

val make : bigstring -> index
(** [make source] returns a {i fingerprint} of [source]. *)

(** The type of the compression. *)
type hunk =
  | Copy of (int * int)
      (** It's the copy ([(off, len)]) {i opcode} to copy {i len} byte(s) range
          from the source at {i off} to the target. *)
  | Insert of (int * int)
      (** It's the insert ([off, len]) {i opcode} to keep a specific byte range
          of {i len} bytes of the target at {i off}. *)

val pp_hunk : Format.formatter -> hunk -> unit
(** Pretty-printer of {!hunk}. *)

val delta : index -> source:bigstring -> target:bigstring -> hunk list
(** [delta index ~source ~target] returns a compression list between the Rabin's
    fingerprint of a [source] [index] with the target [target].

    {b Note:} the given [source] must be the same (not necessary physically)
    than the source used to produce [index] with {!make}. *)
