(*********************************************************************************)
(*                OCamldot                                                       *)
(*                                                                               *)
(*    Copyright (C) 2005-2012 Institut National de Recherche en Informatique et  *)
(*    en Automatique. All rights reserved.                                       *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Lesser General Public License for more details.                        *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser Public License           *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*********************************************************************************)

(* $Id: odot_view.ml 662 2008-09-09 07:25:32Z zoggy $ *)

(** A Lablgtk2 box to view dot graphs.*)

let default_dot_ppi = 72.0

let p_dbg s = ()
(*let p_dbg = prerr_endline*)

type dot_program = Dot | Fdp | Neato | Twopi | Circo | Sfdp | Patchwork | Osage

let string_of_dot_program = function
  Dot -> "dot"
| Fdp -> "fdp"
| Circo -> "circo"
| Neato -> "neato"
| Twopi -> "twopi"
| Sfdp -> "sfdp"
| Patchwork -> "patchwork"
| Osage -> "osage"

(*c==v=[String.split_string]=1.0====*)
let split_string s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" -> iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
    (*/c==v=[String.split_string]=1.0====*)

let show image file zoom_file zoom =
  let com = Printf.sprintf "convert -resize %d%% %s %s"
    zoom
      (Filename.quote file)
      (Filename.quote zoom_file)
  in
  match Sys.command com with
    0 -> image#set_file zoom_file
  | n -> failwith (Printf.sprintf "Exec error %d: %s" n com)

let get_graph_bounding_box stmt_list =
  let rec iter = function
    [] -> raise Not_found
  | (Odot.Stmt_attr (Odot.Attr_graph attr_list)) :: q ->
      begin
        match Odot.attr_value (Odot.Simple_id "bb") attr_list with
          Some (Odot.Simple_id v)
        | Some (Odot.Double_quoted_id v) ->
            begin
              let (x1,y1,x2,y2) =
                match split_string v [','] with
                | ["0"; b; c; "0"] -> ("0","0",c,b)
                | [x1; y1; x2; y2] -> (x1,y1,x2,y2)
                | _ -> raise Not_found
              in
              (float_of_string x1, float_of_string y1,
               float_of_string x2, float_of_string y2)
            end
        | _ -> iter q
      end
  | _ :: q -> iter q
  in
  iter stmt_list

let analyse_annot_dot_file f =
  try
    let graph = Odot.parse_file f in
    let (_,_,width,height) = get_graph_bounding_box graph.Odot.stmt_list in
    p_dbg (Printf.sprintf "width=%f,height=%f" width height);
    let rec iter acc = function
      [] -> acc
    |	stmt :: q ->
        match stmt with
          Odot.Stmt_node (node_id,attr_list) ->
            p_dbg "Stmt_node";
            begin
              try
                let w =
                  match Odot.attr_value (Odot.Simple_id "width") attr_list with
                  | Some (Odot.Simple_id v)
                  | Some (Odot.Double_quoted_id v) -> float_of_string v
                  | _ -> raise Not_found
                in
                p_dbg (Printf.sprintf "width=%.2f" w);
                let h =
                  match Odot.attr_value (Odot.Simple_id "height") attr_list with
                  | Some (Odot.Simple_id v)
                  | Some (Odot.Double_quoted_id v) -> float_of_string v
                  | _ -> raise Not_found
                in
                p_dbg (Printf.sprintf "height=%.2f" h);
                let (x,y) =
                  match Odot.attr_value (Odot.Simple_id "pos") attr_list with
                  | Some (Odot.Simple_id v)
                  | Some (Odot.Double_quoted_id v) ->
                      begin
                        match split_string v [','] with
                          [x;y] -> (float_of_string x, float_of_string y)
                        | _ -> raise Not_found
                      end
                  | _ -> raise Not_found
                in
                p_dbg (Printf.sprintf "x=%f, y=%f" x y);
                let w = w *. default_dot_ppi in
                let h = h *. default_dot_ppi in
                let x1 = x -. w /. 2.0 in
                let y1 = y -. h /. 2.0 in
                let x2 = x +. w /. 2.0 in
                let y2 = y +. h /. 2.0 in
                let s_id = Odot.string_of_node_id node_id in
                p_dbg (Printf.sprintf "id %s: x1=%f y1=%f x2=%f y2=%f"
                 s_id x1 y1 x2 y2);
                iter ((x1,y1,x2,y2,s_id)::acc) q
              with
                Not_found ->
                  iter acc q
              | e ->
                  p_dbg (Printexc.to_string e); iter acc q
            end
        | Odot.Stmt_subgraph g ->
            iter acc (g.Odot.sub_stmt_list @ q)
        | Odot.Stmt_equals _
        | Odot.Stmt_edge _
        | Odot.Stmt_attr _ -> iter acc q
    in
    (width, height, iter [] graph.Odot.stmt_list)
  with
    e ->
      p_dbg (Printexc.to_string e);
      (1., 1., [])


class virtual box ?(dot_program=Dot) ~tmp_hash () =
  let dot_file = Printf.sprintf "%s.dot" tmp_hash in
  let annot_dot_file = Printf.sprintf "%s.dot_annot" tmp_hash in
  let png_file = Printf.sprintf "%s.png" tmp_hash in
  let vbox = GPack.vbox () in
  let hbox = GPack.hbox ~spacing:5 ~packing:(vbox#pack ~expand: false) () in
  let _ = GMisc.label ~text: "Zoom:" ~packing: (hbox#pack ~padding: 4 ~expand: false) () in
  let zooms =
    [ 10 ; 20 ; 30 ; 40 ; 50 ; 60 ; 70 ; 80 ; 90 ; 100 ; 120 ]
  in
  let (wcombo, (store,column)) = GEdit.combo_box_text
    ~strings: (List.map (fun s -> Printf.sprintf "%d%%" s) zooms)
    ~active:9 ()
  in
  let () = wcombo#misc#set_tooltip_text "Zoom" in
  let () = hbox#pack ~expand: false wcombo#coerce in
  let wb_refresh = GButton.button ~label: "Refresh"
    ~packing: (hbox#pack ~expand: false ~padding: 4) ()
  in
  let wscroll = GBin.scrolled_window
    ~vpolicy: `AUTOMATIC
      ~hpolicy: `AUTOMATIC
      ~packing: (vbox#pack ~expand: true)
      ()
  in
  let evt_box = GBin.event_box ~packing: wscroll#add_with_viewport () in
  let image = GMisc.image ~file: png_file ~packing:evt_box#add () in
  let _ = image#set_xalign 0.0 in
  let _ = image#set_yalign 0.0 in
  object(self)
    val mutable current_zoom = 100.0
    val mutable dot_width = 1.
    val mutable dot_height = 1.
    val mutable ids = []

    method virtual build_graph : Odot.graph
    method virtual refresh_data : unit
    method virtual on_button1_press : x: float -> y: float -> string option -> unit

    method private zoom_file_of_zoom zoom =
      Printf.sprintf "%s_%d%%.png" (Filename.chop_extension png_file) zoom

    method box = vbox

    method zoom () =
      let z =
        match wcombo#active_iter with
        | None -> None
        | Some row ->
            let s = wcombo#model#get ~row ~column in
            try Scanf.sscanf s "%d%%" (fun a -> Some a)
            with _ -> None
      in
      match z with
        None -> ()
      |	Some 100 ->
          current_zoom <- 100.0;
          image#set_file png_file
      |	Some z ->
          let f = self#zoom_file_of_zoom z in
          if Sys.file_exists f then
            image#set_file f
          else
            show image png_file f z;
          current_zoom <- float z

    method update_info =
      let (w,h,l) = analyse_annot_dot_file annot_dot_file in
      dot_width <- w;
      dot_height <- h;
      ids <- l

    method clean_files =
      List.iter (fun f -> try Sys.remove f with _ -> ())
        [ dot_file ; annot_dot_file ; png_file];
      List.iter (fun z -> try Sys.remove (self#zoom_file_of_zoom z) with _ -> ()) zooms;

    method refresh () =
      self#clean_files;
      self#refresh_data ;
      let g = self#build_graph in
      Odot.print_file dot_file g;
      let com = Printf.sprintf
        "%s -s%d -y %s > %s && %s -s%d -T png -o %s %s "
          (string_of_dot_program dot_program)
          (int_of_float default_dot_ppi)
          (Filename.quote dot_file)
          (Filename.quote annot_dot_file)
          (string_of_dot_program dot_program)
          (int_of_float default_dot_ppi)
          (Filename.quote png_file)
          (Filename.quote dot_file)
      in
      (
       match Sys.command com with
         0 ->
           self#update_info ;
           self#zoom ()
       | n -> GToolbox.message_box "Error"
           (Printf.sprintf "Exec error %d: %s" n com)
      );

    method private on_button1_press_cb x y =
      p_dbg (Printf.sprintf "Button 1 pressed ! x=%f y=%f" x y);
      let px = image#pixbuf in
      let dc =
        {
          Gobject.kind = `INT ;
          Gobject.proj = (function `INT n -> n | _ -> assert false) ;
          Gobject.inj = (fun n -> `INT n);
        }
      in
      let image_width = Gobject.Property.get px
        { Gobject.name = "width" ; Gobject.conv = dc }
      in
      let image_height = Gobject.Property.get px
        { Gobject.name = "height" ; Gobject.conv = dc }
      in
      let ratio_x = (float image_width) /. dot_width in
      let ratio_y = (float image_height) /. dot_height in
      p_dbg
        (Printf.sprintf "image width=%d height=%d ratio_x=%f ratio_y=%f => x=%f; y=%f"
         image_width image_height ratio_x ratio_y (x /. ratio_x) (y /. ratio_y));
      let id_opt =
        p_dbg (Printf.sprintf "looking in %d ids" (List.length ids));
        try
          let (x1,y1,x2,y2,id) = List.find
            (fun (x1,y1,x2,y2,id) ->
               x1 *. ratio_x <= x && x <= x2 *. ratio_x &&
                 y1 *. ratio_y <= y && y <= y2 *. ratio_y
            )
              ids
          in
          p_dbg (Printf.sprintf
           "Id %s clicked pixels: x1=%f x2=%f y1=%f y2=%f ratio_x=%f ratio_y=%f"
             id
             (x1 *. ratio_x) (x2 *. ratio_x)
             (y1 *. ratio_y) (y2 *. ratio_y)
             ratio_x ratio_y
          );
          Some id
        with Not_found ->
          p_dbg "No id found";
          None
      in
      self#on_button1_press ~x ~y id_opt

    method on_button3_press x y =
      let entries = List.mapi
        (fun i z ->
           let t = Printf.sprintf "%d%%" z in
           `I (t, fun () -> wcombo#set_active i)
        )
          zooms
      in
      GToolbox.popup_menu ~entries ~button: 3 ~time: Int32.zero

    initializer
      ignore (vbox#connect#destroy (fun () -> self#clean_files));
      (*wcombo#entry#set_editable false;*)
      (*wcombo#entry#set_text "100%";*)
      ignore(wcombo#connect#changed self#zoom);
      ignore (wb_refresh#connect#clicked self#refresh);
      ignore
        (evt_box#event#connect#button_press ~callback:
         (fun evt ->
            match GdkEvent.Button.button evt with
              1 ->
                GdkEvent.get_type evt = `BUTTON_PRESS &&
                  (
                   let x = GdkEvent.Button.x evt in
                   let y = GdkEvent.Button.y evt in
                   self#on_button1_press_cb x y;
                   true
                  )
            | 3 ->
                GdkEvent.get_type evt = `BUTTON_PRESS &&
                  (
                   let x = GdkEvent.Button.x evt in
                   let y = GdkEvent.Button.y evt in
                   self#on_button3_press x y;
                   true
                  )
            | n -> true
         )
        );
      if not (Sys.file_exists annot_dot_file) then
        self#refresh ()
      else
        (
         self#refresh_data;
         self#update_info
        )

  end

