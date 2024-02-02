open Haz3lcore;
open Sexplib.Std;
open Js_of_ocaml;
[@deriving (show({with_path: false}), sexp, yojson)]
type key = string;

[@deriving (show({with_path: false}), sexp, yojson)]
type request = (key, DHExp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type eval_result =
  | EvaluationOk(ProgramResult.t)
  | EvaluationFail(ProgramEvaluatorError.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type response = (key, eval_result);

module Request = {
  [@deriving (sexp, yojson)]
  type t = request;
  type u = string;

  let serialize = program => program |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

module Response = {
  [@deriving (sexp, yojson)]
  type t = response;
  type u = string;

  let serialize = r => Sexplib.(r |> sexp_of_t |> Sexp.to_string);
  let deserialize = sexp => Sexplib.(sexp |> Sexp.of_string |> t_of_sexp);
};
/*

 let evaluator_subscribe_callback =
     (schedule_action: UpdateAction.t => unit, (key: key, r: eval_result))
     : unit => {
   print_endline("EvalClient: evaluator_subscribe_callback");
   let cr: Haz3lcore.ModelResult.current =
     switch (r) {
     | EvaluationOk(r) =>
       print_endline("66666 SetMeta(ResultOk)");
       ResultOk(r);
     | EvaluationFail(reason) =>
       print_endline("66666 SetMeta(ResultFail)");
       ResultFail(reason);
     };
   schedule_action(SetMeta(Result(key, cr)));
 };

 let init = () => Worker.create("worker.js");
 let worker: Js.t(Worker.worker(string, string)) = init();

 let request = (schedule_action, req: Request.t): unit => {
   worker##.onmessage :=
     Dom.handler(evt => {
       print_endline("EvalClient: onmessage");
       let res = evt##.data |> Response.deserialize;
       evaluator_subscribe_callback(schedule_action, res);
       Js._true;
     });
   print_endline("EvalClient: About to post message to worker");
   /* Post request to worker. */
   worker##postMessage(req |> Request.serialize);
 };
 //let terminate_worker = () => worker##terminate;
 */
let evaluator_subscribe_callback =
    (schedule_action: UpdateAction.t => unit, (key: key, r: eval_result))
    : unit => {
  print_endline("EvalClient: evaluator_subscribe_callback");
  let cr: Haz3lcore.ModelResult.current =
    switch (r) {
    | EvaluationOk(r) =>
      print_endline("66666 SetMeta(ResultOk)");
      ResultOk(r);
    | EvaluationFail(reason) =>
      print_endline("66666 SetMeta(ResultFail)");
      ResultFail(reason);
    };
  schedule_action(SetMeta(Result(key, cr)));
};

let initWorker = () => Worker.create("worker.js");

let workerRef: ref(Js.t(Worker.worker(string, string))) =
  ref(initWorker());
let timeoutId = ref(None);
let timeoutDuration = 8000; // Timeout in milliseconds (adjust as needed)

let request = (schedule_action, req: Request.t): unit => {
  let setupWorkerMessageHandler = worker => {
    worker##.onmessage :=
      Dom.handler(evt => {
        switch (timeoutId.contents) {
        | Some(id) => Dom_html.window##clearTimeout(id)
        | None => ()
        };
        timeoutId.contents = None; // Clear the timeout after receiving response
        print_endline("EvalClient: onmessage");
        let res = evt##.data |> Response.deserialize;
        evaluator_subscribe_callback(schedule_action, res);
        Js._true;
      });
  };

  // If there's an ongoing request, terminate the worker and reinitialize
  switch (timeoutId.contents) {
  | Some(id) =>
    print_endline("EvalClient: Terminating ongoing request");
    Dom_html.window##clearTimeout(id);
    workerRef.contents##terminate;
    workerRef.contents = initWorker();
  | None => ()
  };

  setupWorkerMessageHandler(workerRef.contents);

  print_endline("EvalClient: About to post message to worker");
  workerRef.contents##postMessage(req |> Request.serialize);

  let onTimeout = () => {
    print_endline("EvalClient: Worker timeout, terminating worker");
    workerRef.contents##terminate;
    workerRef.contents = initWorker();
    setupWorkerMessageHandler(workerRef.contents);
  };

  timeoutId.contents =
    Some(
      Dom_html.window##setTimeout(
        Js.wrap_callback(onTimeout),
        float_of_int(timeoutDuration),
      ),
    );
};
