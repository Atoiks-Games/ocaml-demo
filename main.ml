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

let make_initial_state () = {
    player_x = 0.;
    player_y = 0.;
    last_time_milli = 0l;
    has_key_up = false;
    has_key_down = false;
    has_key_left = false;
    has_key_right = false; }

let is_up_key kc = kc = Sdl.K.w || kc = Sdl.K.up
let is_down_key kc = kc = Sdl.K.s || kc = Sdl.K.down
let is_left_key kc = kc = Sdl.K.a || kc = Sdl.K.left
let is_right_key kc = kc = Sdl.K.d || kc = Sdl.K.right

(* converts a vector into a unit vector, if was zero vector, do nothing *)
let normalize_vec vx vy =
    if vx = 0. && vy = 0. then
        (0., 0.)
    else
        let norm = sqrt (vx *. vx +. vy *. vy) in
        (vx /. norm, vy /. norm)

let calc_velocity_uvec state_data =
    let lmr_sum left neutral right op bool_left bool_right =
        op (if bool_left then left else neutral) (if bool_right then right else neutral) in
    let raw_velocity = lmr_sum (-1.) (0.) (1.) (+.) in
    let x = raw_velocity state_data.has_key_left state_data.has_key_right in
    let y = raw_velocity state_data.has_key_up state_data.has_key_down in
    normalize_vec x y

(* return None if you want to stop the game *)
let rec my_update state_data =
    let evt = Sdl.Event.create () in
    if Sdl.poll_event (Some evt) then
        (* apart from the quit event handler, all others recursively call my_update *)
        let evt_type = Sdl.Event.get evt Sdl.Event.typ in
        if evt_type = Sdl.Event.quit then
            None
        else if evt_type = Sdl.Event.key_down then
            (* don't care if the key is repeating *)
            let kc = Sdl.Event.get evt Sdl.Event.keyboard_keycode in
            let has_key_up = state_data.has_key_up || is_up_key kc in
            let has_key_down = state_data.has_key_down || is_down_key kc in
            let has_key_left = state_data.has_key_left || is_left_key kc in
            let has_key_right = state_data.has_key_right || is_right_key kc in
            my_update { state_data with has_key_up; has_key_down; has_key_left; has_key_right; }
        else if evt_type = Sdl.Event.key_up then
            (* don't care if the key is repeating *)
            let kc = Sdl.Event.get evt Sdl.Event.keyboard_keycode in
            let has_key_up = not (not state_data.has_key_up || is_up_key kc) in
            let has_key_down = not (not state_data.has_key_down || is_down_key kc) in
            let has_key_left = not (not state_data.has_key_left || is_left_key kc) in
            let has_key_right = not (not state_data.has_key_right || is_right_key kc) in
            my_update { state_data with has_key_up; has_key_down; has_key_left; has_key_right; }
        else
            (* you would handle some more events
            * (such as mouse clicks and stuff)
            * here! *)
            my_update state_data
    else
        let current_time_milli = Sdl.get_ticks () in
        let delta_time_milli = Int32.sub current_time_milli state_data.last_time_milli in
        let dt = Int32.to_float delta_time_milli /. 1000. in
        let (dx, dy) = calc_velocity_uvec state_data in
        Some {
            state_data with player_x = state_data.player_x +. 150. *. dx *. dt;
                            player_y = state_data.player_y +. 150. *. dy *. dt;
                            last_time_milli = current_time_milli; }

(* renders stuff *)
let my_render renderer state_data =
    Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> ignore;
    Sdl.render_clear renderer |> ignore;

    (* keeping the *terrible* gfx tradition, out player will just be a red box (for now) *)
    Sdl.set_render_draw_color renderer 0xFF 0x00 0x00 0xFF |> ignore;
    let xpos = int_of_float state_data.player_x in
    let ypos = int_of_float state_data.player_y in
    Sdl.render_fill_rect renderer (Some (Sdl.Rect.create xpos ypos 20 20)) |> ignore

(* this does my_update -> my_render -> blit to renderer -> ... *)
let game_loop renderer =
    let rec inner_loop state =
        (match my_update state with
        | None -> ()
        | Some datum ->
            (* only do rendering and continue loop if we are continuing *)
            my_render renderer datum;
            Sdl.render_present renderer;
            inner_loop datum) in
    (* start the inner loop with inital game state *)
    inner_loop (make_initial_state ())

(* follow C's footsteps, return 0 is success, everything else is failure! *)
let main () =
    match Sdl.init Sdl.Init.video with
    | Error (`Msg e) ->
        Sdl.log "Init error: %s" e;
        1
    | Ok () ->
        (match Sdl.create_window ~w:640 ~h:480 "OCaml Demo" Sdl.Window.opengl with
        | Error (`Msg e) -> Sdl.log "Create window error: %s" e
        | Ok window ->
            (* need to acquire a renderer context then we can contine from there *)
            (match Sdl.create_renderer ~flags:Sdl.Renderer.accelerated window with
            | Error (`Msg e) -> Sdl.log "Create renderer error: %s" e
            | Ok renderer ->
                game_loop renderer;
                Sdl.destroy_renderer renderer);
            Sdl.destroy_window window);
        Sdl.quit ();
        0

let () = exit (main ())

(*
> ocamlfind ocamlc -package tsdl -linkpkg -o main.byte main.ml
> ocamlfind ocamlopt -package tsdl -linkpkg -o main.native main.ml
*)
