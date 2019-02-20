open Tsdl
open Result

(* return false if you want to stop the game *)
let my_update () =
    let evt = Sdl.Event.create () in
    if not (Sdl.poll_event (Some evt)) then
        true
    else begin
        let evt_type = Sdl.Event.get evt Sdl.Event.typ in
        if evt_type = Sdl.Event.quit then
            false
        else
            (* you would handle some more events
             * (such as mouse clicks and stuff)
             * here! *)
            true
    end
;;

(* renders stuff *)
let my_render renderer =
    Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> ignore;
    Sdl.render_clear renderer |> ignore;

    Sdl.set_render_draw_color renderer 0xFF 0x00 0x00 0xFF |> ignore;
    Sdl.render_fill_rect renderer (Some (Sdl.Rect.create 10 10 20 20)) |> ignore;
;;

(* this does my_update -> my_render -> blit to renderer -> ... *)
let rec game_loop renderer =
    if not (my_update ()) then
        ()
    else begin
        my_render renderer;
        Sdl.render_present renderer;
        game_loop renderer;
    end
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
