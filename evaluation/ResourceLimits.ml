exception ExceededMemoryLimit of int
exception ExceededTimeLimit of float

let mem_limit = 1000000000
let time_limit = 60.

(* Note: only triggers alarm on a major garbage collection, so actual time & memory used will differ *)
let run_with_limits f =
  let alarm_mem =
    Gc.create_alarm (fun () ->
        let mem = Gc.(quick_stat ()).heap_words in
        if mem > mem_limit / (Sys.word_size / 8) then
          raise (ExceededMemoryLimit mem_limit))
  in
  let start_time = Sys.time () in
  let alarm_time =
    Gc.create_alarm (fun () ->
        if Sys.time () -. start_time > time_limit then
          raise (ExceededTimeLimit time_limit))
  in
  Fun.protect f ~finally:(fun () ->
      Gc.delete_alarm alarm_time;
      Gc.delete_alarm alarm_mem;
      Gc.compact ())
