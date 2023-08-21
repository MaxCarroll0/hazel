open API;
open Util.OptUtil.Syntax;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type chat_models =
  | GPT4
  | GPT3_5Turbo
  | Azure_GPT4
  | Azure_GPT3_5Turbo
  | Llama2;

[@deriving (show({with_path: false}), sexp, yojson)]
type role =
  | System
  | User
  | Assistant
  | Function;

[@deriving (show({with_path: false}), sexp, yojson)]
type message = {
  role,
  content: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type prompt = list(message);

[@deriving (show({with_path: false}), sexp, yojson)]
let string_of_chat_model =
  fun
  | GPT4 => "gpt-4"
  | GPT3_5Turbo => "gpt-3.5-turbo"
  | Azure_GPT4 => "azure-gpt-4"
  | Azure_GPT3_5Turbo => "azure-gpt-3.5-turbo"
  | Llama2 => "llama-2";

let string_of_role =
  fun
  | System => "system"
  | User => "user"
  | Assistant => "assistant"
  | Function => "function";

let mk_message = ({role, content}) =>
  `Assoc([
    ("role", `String(string_of_role(role))),
    ("content", `String(content)),
  ]);

let body = (~llm, messages: prompt): Json.t => {
  `Assoc([
    ("model", `String(string_of_chat_model(llm))),
    ("messages", `List(List.map(mk_message, messages))),
  ]);
};

/* SAMPLE OPENAI CHAT RESPONSE:
    {
      "id":"chatcmpl-6y5167eYM6ovo5yVThXzr5CB8oVIO",
      "object":"chat.completion",
      "created":1679776984,
      "model":"gpt-3.5-turbo-0301",
      "usage":{
         "prompt_tokens":25,
         "completion_tokens":1,
         "total_tokens":26
      },
      "choices":[
         {
            "message":{
               "role":"assistant",
               "content":"576"
            },
            "finish_reason":"stop",
            "index":0
         }
      ]
   }*/

let chat = (~body, ~handler): unit =>
  switch (Store.Generic.load("OpenAI")) {
  | None => print_endline("API: OpenAI KEY NOT FOUND")
  | Some(api_key) =>
    print_endline("API: POSTing OpenAI request");
    request(
      ~method=POST,
      ~url="https://api.openai.com/v1/chat/completions",
      ~headers=[
        ("Content-Type", "application/json"),
        ("Authorization", "Bearer " ++ api_key),
      ],
      ~body,
      handler,
    );
  };

let local_chat = (~body, ~handler): unit =>
  request(
    ~method=POST,
    ~url="http://127.0.0.1:8081/chat/completions",
    ~headers=[("Content-Type", "application/json")],
    ~body,
    handler,
  );

module Azure = {
  let chat =
      (~key, ~resource, ~deployment, ~api_version, ~body, ~handler): unit =>
    switch (Store.Generic.load(key)) {
    | None => print_endline("API: KEY '" ++ key ++ "' NOT FOUND")
    | Some(api_key) =>
      print_endline("API: POSTing Azure request");
      request(
        ~method=POST,
        ~url=
          Printf.sprintf(
            "https://%s.openai.azure.com/openai/deployments/%s/chat/completions?api-version=%s",
            resource,
            deployment,
            api_version,
          ),
        ~headers=[
          ("Content-Type", "application/json"),
          ("api-key", api_key),
        ],
        ~body,
        handler,
      );
    };
};

module AzureGPT3_5 = {
  let chat = (~body, ~handler): unit =>
    Azure.chat(
      ~key="AZURE",
      ~resource="hazel",
      ~deployment="gpt35turbo",
      ~api_version="2023-05-15",
      ~body,
      ~handler,
    );
};

module AzureGPT4 = {
  let chat = (~body, ~handler): unit =>
    Azure.chat(
      ~key="AZURE4",
      ~resource="hazel2",
      ~deployment="hazel-gpt-4",
      ~api_version="2023-05-15",
      ~body,
      ~handler,
    );
};

//let body_simple = (~llm, prompt) => body(~llm, [(User, prompt)]);

let start_chat = (~llm, prompt: prompt, handler): unit => {
  let body = body(~llm, prompt);
  switch (llm) {
  | Azure_GPT3_5Turbo => AzureGPT3_5.chat(~body, ~handler)
  | Azure_GPT4 => AzureGPT4.chat(~body, ~handler)
  | GPT3_5Turbo
  | GPT4 => chat(~body, ~handler)
  | Llama2 => local_chat(~body, ~handler)
  };
};

let reply_chat = (~llm, prompt: prompt, ~assistant, ~user, handler): unit => {
  let body =
    body(
      ~llm,
      prompt
      @ [{role: Assistant, content: assistant}, {role: User, content: user}],
    );
  switch (llm) {
  | Azure_GPT3_5Turbo => AzureGPT3_5.chat(~body, ~handler)
  | Azure_GPT4 => AzureGPT4.chat(~body, ~handler)
  | GPT3_5Turbo
  | GPT4 => chat(~body, ~handler)
  | Llama2 => local_chat(~body, ~handler)
  };
};

let handle_chat = (request: request): option(string) =>
  switch (receive(request)) {
  | Some(json) =>
    let* choices = Json.dot("choices", json);
    let* choices = Json.list(choices);
    let* hd = Util.ListUtil.hd_opt(choices);
    let* message = Json.dot("message", hd);
    let* content = Json.dot("content", message);
    Json.str(content);
  | _ =>
    print_endline("API: handle_chat: no response");
    None;
  };
