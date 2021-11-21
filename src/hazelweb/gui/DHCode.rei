open Virtual_dom;

let view_of_hole_instance:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~width: int,
    ~pos: int=?,
    ~selected_instance: option(HoleInstance.t),
    ~settings: Settings.Evaluation.t,
    ~font_metrics: FontMetrics.t,
    HoleInstance.t
  ) =>
  Vdom.Node.t;

let view_of_var: string => Vdom.Node.t;

let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~settings: Settings.Evaluation.t,
    ~selected_instance: option(HoleInstance.t),
    ~show_steppable: bool=?,
    ~font_metrics: FontMetrics.t,
    ~width: int,
    ~pos: int=?,
    DHExp.t
  ) =>
  Vdom.Node.t;

let view_of_results:
  (
    ~result_states: list(DHExp.t),
    ~inject: ModelAction.t => Vdom.Event.t,
    ~selected_instance: option(HoleInstance.t),
    ~settings: Settings.t,
    ~model: Model.t
  ) =>
  list(Vdom.Node.t);
