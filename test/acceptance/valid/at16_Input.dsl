Statemachine M {
  #VAR x = 5
  INPUT open close lock unlock
  Start State A {
    ON open GO B
    ON unlock GO C

  }
  State B{
    ON close GO C
  }

  Final State C {
    ON lock GO A
  }
}