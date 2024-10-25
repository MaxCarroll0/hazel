let prompt = Ex_DerivationEmpty_prompt.prompt

let exercise : Exercise.spec =
  Exercise.transition
    {
      header =
        {
          title = "Derivation Playground 3";
          version = 1;
          module_name = "Ex_DerivationPlayground_3";
          prompt;
        };
      pos = Proof Prelude;
      model =
        Proof
          {
            version = Haz3lcore.RuleImage.GradualALFA;
            prelude = "";
            setup = "";
            trees = [ Node (Just { jdmt = ""; rule = None }, []) ];
          };
    }
