open Tsdl
open Result

type game_state = {
    player_x : float;
    player_y : float;
    last_time_milli : Sdl.uint32;
    has_key_up : bool;
    has_key_down : bool;
    has_key_left : bool;
    has_key_right : bool; }
;;

let make_initial_state () = {
    player_x = 0.;
    player_y = 0.;
    last_time_milli = 0l;
    has_key_up = false;
    has_key_down = false;
    has_key_left = false;
    has_key_right = false; }
;;

let is_up_key kc = kc = Sdl.K.w || kc = Sdl.K.up;;
let is_down_key kc = kc = Sdl.K.s || kc = Sdl.K.down;;
let is_left_key kc = kc = Sdl.K.a || kc = Sdl.K.left;;
let is_right_key kc = kc = Sdl.K.d || kc = Sdl.K.right;;

let calc_velocity_uvec state_data =
    let ux = (if state_data.has_key_right then 1. else 0.) +. (if state_data.has_key_left then -1. else 0.)
    and uy = (if state_data.has_key_down then 1. else 0.) +. (if state_data.has_key_up then -1. else 0.) in
    if ux = 0. && uy = 0. then
        (* handle this case separately otherwise divide by zero! *)
        (0., 0.)
    else
        (* convert to unit vector by dividing by the magnitude/norm/length/... *)
        let norm = sqrt (ux *. ux +. uy *. uy) in
        (ux /. norm, uy /. norm)
;;

(* return None if you want to stop the game *)
let rec my_update state_data =
    let evt = Sdl.Event.create () in
    if not (Sdl.poll_event (Some evt)) then
        let current_time_milli = Sdl.get_ticks () in
        let delta_time_milli = Int32.sub current_time_milli state_data.last_time_milli in
        let dt = Int32.to_float delta_time_milli /. 1000.
        and (dx, dy) = calc_velocity_uvec state_data in
        Some {
            state_data with player_x = state_data.player_x +. 150. *. dx *. dt;
                            player_y = state_data.player_y +. 150. *. dy *. dt;
                            last_time_milli = current_time_milli; }
    else begin
        (* apart from the quit event handler, all others recursively call my_update *)
        let evt_type = Sdl.Event.get evt Sdl.Event.typ in
        if evt_type = Sdl.Event.quit then
            None
        else if evt_type = Sdl.Event.key_down then
            (* don't care if the key is repeating *)
            let kc = Sdl.Event.get evt Sdl.Event.keyboard_keycode in
            let has_key_up = state_data.has_key_up || is_up_key kc
            and has_key_down = state_data.has_key_down || is_down_key kc
            and has_key_left = state_data.has_key_left || is_left_key kc
            and has_key_right = state_data.has_key_right || is_right_key kc in
            my_update { state_data with has_key_up; has_key_down; has_key_left; has_key_right; }
        else if evt_type = Sdl.Event.key_up then
            (* don't care if the key is repeating *)
            let kc = Sdl.Event.get evt Sdl.Event.keyboard_keycode in
            let has_key_up = not (not state_data.has_key_up || is_up_key kc)
            and has_key_down = not (not state_data.has_key_down || is_down_key kc)
            and has_key_left = not (not state_data.has_key_left || is_left_key kc)
            and has_key_right = not (not state_data.has_key_right || is_right_key kc) in
            my_update { state_data with has_key_up; has_key_down; has_key_left; has_key_right; }
        else
            (* you would handle some more events
            * (such as mouse clicks and stuff)
            * here! *)
            my_update state_data
    end
;;

(* renders stuff *)
let my_render renderer state_data =
    Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> ignore;
    Sdl.render_clear renderer |> ignore;

    (* keeping the *terrible* gfx tradition, out player will just be a red box (for now) *)
    Sdl.set_render_draw_color renderer 0xFF 0x00 0x00 0xFF |> ignore;
    let xpos = int_of_float state_data.player_x
    and ypos = int_of_float state_data.player_y in
    Sdl.render_fill_rect renderer (Some (Sdl.Rect.create xpos ypos 20 20)) |> ignore;
;;

(* this does my_update -> my_render -> blit to renderer -> ... *)
let game_loop renderer =
    let rec inner_loop state =
        let next_state = my_update state in
        match next_state with
        | None -> ()
        | Some datum -> begin
            (* only do rendering and continue loop if we are continuing *)
            my_render renderer datum;
            Sdl.render_present renderer;
            inner_loop datum;
        end in
    (* start the inner loop with inital game state *)
    inner_loop (make_initial_state ())
;;

let main () = match Sdl.init Sdl.Init.video with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
| Ok () ->
    match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
        (* need to acquire a renderer context then we can contine from there *)
        match Sdl.create_renderer ~flags:Sdl.Renderer.accelerated w with
        | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
        | Ok renderer -> game_loop renderer;
        Sdl.destroy_renderer renderer;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0
;;

let () = main ()

(*
> ocamlfind ocamlc -package tsdl -linkpkg -o main.byte main.ml
> ocamlfind ocamlopt -package tsdl -linkpkg -o main.native main.ml
*)
