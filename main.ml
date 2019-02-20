open Tsdl
open Result

type game_state =
    | Next of {
        player_x : int;
        player_y : int; }
    | Halt
;;

let make_initial_state () =
    Next {
        player_x = 0;
        player_y = 0; }
;;

(* return Halt if you want to stop the game *)
let my_update gstate =
    let evt = Sdl.Event.create () in
    if not (Sdl.poll_event (Some evt)) then
        gstate
    else begin
        let evt_type = Sdl.Event.get evt Sdl.Event.typ in
        if evt_type = Sdl.Event.quit then
            Halt
        else if evt_type = Sdl.Event.key_down then
            (* don't care if the key is repeating *)
            let kc = Sdl.Event.get evt Sdl.Event.keyboard_keycode in
            let dy =
                let dw = if kc = Sdl.K.w || kc = Sdl.K.up then 1 else 0
                and ds = if kc = Sdl.K.s || kc = Sdl.K.down then -1 else 0 in
                dw + ds
            and dx =
                let dd = if kc = Sdl.K.d || kc = Sdl.K.right then 1 else 0
                and da = if kc = Sdl.K.a || kc = Sdl.K.left then -1 else 0 in
                da + dd in
            match gstate with
            | Next {player_x = x; player_y = y} -> Next {
                player_x = x + dx;
                player_y = y + dy; }
            | Halt -> Halt
        else
            (* you would handle some more events
             * (such as mouse clicks and stuff)
             * here! *)
            gstate
    end
;;

(* renders stuff *)
let my_render renderer = function
    | Halt -> () (* this should not happen, oh well... *)
    | Next state_data -> begin
        Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> ignore;
        Sdl.render_clear renderer |> ignore;

        (* keeping the *terrible* gfx tradition, out player will just be a red box (for now) *)
        Sdl.set_render_draw_color renderer 0xFF 0x00 0x00 0xFF |> ignore;
        Sdl.render_fill_rect renderer (Some (Sdl.Rect.create state_data.player_x state_data.player_y 20 20)) |> ignore;
    end
;;

(* this does my_update -> my_render -> blit to renderer -> ... *)
let game_loop renderer =
    let rec inner_loop state =
        let next_state = my_update state in
        match next_state with
        | Halt -> ()
        | Next datum -> begin
            (* only do rendering and continue loop if we are continuing *)
            my_render renderer (Next datum);
            Sdl.render_present renderer;
            inner_loop next_state;
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
