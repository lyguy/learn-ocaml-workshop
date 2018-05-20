open! Base
open! Import

module Model : sig
  type t

  (** Returns the [empty] model, with no data  *)
  val create : now:Time.t -> t

  (** Incrementally returns the widget to display and the currently
     selected line, if any. *)
  val widget_and_selected
    :  t Incr.t
    -> (Tty_text.Widget.t * string option) Incr.t
end

(** The set of things that the application can ask to do. *)
module Action : sig
  type t =
    | Exit
    | Exit_and_print
end

(** Handles a character hit by the user *)
val handle_user_input : Model.t -> Tty_text.User_input.t -> Model.t * Action.t option

(** Handles a new line of data from the input stream being analyzed *)
val handle_line : Model.t -> string -> Model.t

(** Handles the completion of the input stream being analyzed *)
val handle_closed : Model.t -> Time.t -> Model.t

(** Inform the model as to the dimensions of the screen *)
val set_dim : Model.t -> Tty_text.Dimensions.t -> Model.t
