type gen_state = {
  randgen : string;
  rnd_state : Random.State.t;
  weighted_grammar : WeightedGrammar.weighted_grammar;
  first_rule : string }
