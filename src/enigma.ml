(************************************************************
   Copyright (C) 2021 Cornell University.
   Created by Michael Clarkson (mrc26@cornell.edu) and CS 3110 course staff.
   You may not redistribute this assignment, distribute any derivatives,
   or use it for commercial purposes.
 ************************************************************)

(** CS 3110 Fall 2021 Assignment A1 Enigma

    @author Nicholas Runje (njr85) *)

(* TODO: complete the academic integrity statement below, then delete
   this TODO comment. *)

(************************************************************

  Academic Integrity Statement

  I, the person named in the author comment above, have fully reviewed
  the course academic integrity policies. I have adhered to those
  policies in solving the assignment.

  The policies do permit some limited collaboration among students
  currently enrolled in the course. If I did engage in such
  collaboration, here is the list of other students with whom I
  collaborated, and a brief summary of that collaboration:

  - Nicholas Runje

  ************************************************************)

(** [index c] is the 0-based index of [c] in the alphabet. Requires: [c]
    is an uppercase letter in A..Z. *)
let index (c : char) : int = Char.code c - 65

(** [map_r_to_l wiring top_letter input_pos] is the left-hand output
    position at which current would appear when current enters at
    right-hand input position [input_pos] to a rotor whose wiring
    specification is given by [wiring]. The orientation of the rotor is
    given by [top_letter], which is the top letter appearing to the
    operator in the rotor's present orientation. Requires: [wiring] is a
    valid wiring specification, [top_letter] is in A..Z, and [input_pos]
    is in 0..25. *)
let map_r_to_l (wiring : string) (top_letter : char) (input_pos : int) :
    int =
  let initial_pos = (input_pos + index top_letter) mod 26 in
  let w = index wiring.[initial_pos] in

  (* Below is output *)
  let output = w - index top_letter in
  (output + 26) mod 26

(** [map_l_to_r] computes the same function as [map_r_to_l], except for
    current flowing left to right. *)
let map_l_to_r (wiring : string) (top_letter : char) (input_pos : int) :
    int =
  let enters_left = (input_pos + index top_letter) mod 26 in
  let w_inverse =
    String.index wiring "ABCDEFGHIJKLMNOPQRSTUVWXYZ".[enters_left]
  in
  let output = w_inverse - index top_letter in
  (output + 26) mod 26

(** [map_refl wiring input_pos] is the output position at which current
    would appear when current enters at input position [input_pos] to a
    reflector whose wiring specification is given by [wiring]. Requires:
    [wiring] is a valid reflector specification, and [input_pos] is in
    0..25. *)
let map_refl (wiring : string) (input_pos : int) : int =
  let output = map_r_to_l wiring 'A' input_pos in
  output

(** [map_plug plugs c] is the letter to which [c] is transformed by the
    plugboard [plugs]. Requires: [plugs] is a valid plugboard, and [c]
    is in A..Z. *)
let rec map_plug (plugs : (char * char) list) (c : char) : char =
  match plugs with
  | [] -> c
  | [ (f, l) ] when f = c -> l
  | [ (f, l) ] when l = c -> f
  | (f, l) :: t when f = c -> l
  | (f, l) :: t when l = c -> f
  | (f, l) :: t -> map_plug t c

type rotor = {
  wiring : string;  (** A valid rotor wiring specification. *)
  turnover : char;
      (** The turnover of the rotor, which must be an uppercase letter.
          This field will not be used in the assignment until you
          implement stepping in the excellent scope. *)
}
(** [rotor] represents an Enigma rotor. *)

type oriented_rotor = {
  rotor : rotor;  (** The rotor. *)
  top_letter : char;  (** The top letter showing on the rotor. *)
}
(** [oriented_rotor] represents a rotor that is installed on the spindle
    hence has a top letter. *)

type config = {
  refl : string;  (** A valid reflector wiring specification. *)
  rotors : oriented_rotor list;
      (** The rotors as they are installed on the spindle from left to
          right. There may be any number of elements in this list: 0, 1,
          2, 3, 4, 5, etc. The order of elements in list represents the
          order in which the rotors are installed on the spindle, **from
          left to right**. So, the head of the list is the leftmost
          rotor on the spindle, and the last element of the list is the
          rightmost rotor on the spindle. *)
  plugboard : (char * char) list;
      (** A valid plugboard. The order of characters in the pairs does
          not matter, and the order of pairs in the list does not
          matter. *)
}
(** [config] represents the configuration of an Enigma machine. *)

(** [cipher_char config c] is the letter to which the Enigma machine
    ciphers input [c] when it is in configuration [config]. Requires:
    [config] is a valid configuration, and [c] is in A..Z. *)
let cipher_char (config : config) (c : char) : char =
  raise (Failure "Unimplemented: Enigma.cipher_char")

(** [step config] is the new configuration to which the Enigma machine
    transitions when it steps beginning in configuration [config].
    Requires: [config] is a valid configuration. *)
let step (config : config) : config =
  raise (Failure "Unimplemented: Enigma.step")

(** [cipher config s] is the string to which [s] enciphers when the
    Enigma machine begins in configuration [config]. Requires: [config]
    is a valid configuration, and [s] contains only uppercase letters. *)
let rec cipher (config : config) (s : string) : string =
  raise (Failure "Unimplemented: Enigma.cipher")

(* TODO: set the value below to the number of hours you spent working on
   this assignment, rounded to the nearest integer, then delete this
   TODO comment. *)

let hours_worked = 9
