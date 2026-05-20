Statemachine M {
  # VAR x = 5

  Start State A {
    ON Push IF x < 10 GO B
  }

  Final State B {
  }
}