module ExerciseEnv = {
  type node = unit;
  let default = ();
  let output_header = Exercise.output_header_grading;
};

module TutorialEnv = {
  type node = unit;
  let default = ();
  let output_header = Tutorial.output_header_grading;
};

module Tutorial = Tutorial.D(TutorialEnv);

module GradingT = TutorialGrading.D(TutorialEnv);

module Exercise = Exercise.F(ExerciseEnv);

module Grading = Grading.F(ExerciseEnv);
