Statemachine M {
  #VAR x = 5

  Start State A {
    ON 1 IF x < 10 GO C
  }

  Final State C {
  }
}