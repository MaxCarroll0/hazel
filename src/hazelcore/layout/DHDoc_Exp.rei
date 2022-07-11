let mk:
  (
    ~settings: DHSettings.t,
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    ~selected_instance: option(HoleInstance.t),
    ~selected_tag_hole: option(MetaVar.t),
    DHExp.t
  ) =>
  DHDoc.t;
