Statemachine M {
  #VAR x = 5

  Start State A {
    ON push GO C {
      DO x = x + 1
    }
  }

  Final State C {
  }

}