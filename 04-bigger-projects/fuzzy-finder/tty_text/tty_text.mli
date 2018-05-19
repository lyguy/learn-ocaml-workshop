open! Core
open! Async

(** Represents interaction between a terminal and a user *)
type t

(** Widgets can be rendered to the screen.
    [horizontal_group] allows for horizontal grouping on a single line.
    [vertical_group] allows for there to be multiple lines printed.

    The types do not stop things like a [vertical_group] being placed
    inside of a [vertical_group], the library does not do anything
    clever to detect such situations. *)
module Widget : sig
  type t

  val of_char : char -> t

  val of_string : string -> t

  (** [hbox ts] stacks widgets horizontally, aligned to the top. *)
  val hbox : t list -> t

  (** [vbox ts] stacks widgets vertically, aligned to the left. *)
  val vbox : t list -> t
end

module Dimensions : sig
  type t = { width: int
           ; height: int }
end

module User_input : sig
  type t =
    | Ctrl_c
    | Escape
    | Backspace
    | Return (* Enter key *)
    | Char of char
    | Up
    | Down
  [@@deriving sexp_of]
end

(** [with_rendering f] will start rendering to the terminal, and
    will return a pipe for reading user input, as well as a [t].
    When [f] becomes determined, the screen rendering will end.  *)
val with_rendering
  :  ((User_input.t Pipe.Reader.t * t) -> 'a Deferred.t)
  -> 'a Deferred.t

(** [dimensions t] returns the terminal dimensions that were
    determined when [with_rendering] was invoked. *)
val dimensions : t -> Dimensions.t

(** [render t w] requests that the widget [w] be rendered upon the screen.
    There is no horizontal or vertical alignment performed; the screen
    is cleared, and then [w] is drawn.

    [render] will stop drawing immediately after the last horizontal element
    provided to it in [w], there is no guarantee that will be the last row
    in the terminal.

    Users are encouraged to pad their widgets with empty [text]s in order to
    pad height *)
val render : t -> Widget.t -> unit Deferred.t
