Statemachine M {
  #VAR x = 5
  INPUT open close lock unlock
  Start State A {
    ON open GO C

  }

  Final State C {
  }
}