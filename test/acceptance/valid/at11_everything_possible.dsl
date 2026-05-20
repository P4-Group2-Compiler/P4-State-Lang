Statemachine M {
  # VAR x = 5
  StartFinal State A {
    ON Push IF x < 10 GO B
    ON Pull OR Punch GO C
  }

  Final State B{

  }
  Final State C{

  }

  State D{

  }
}