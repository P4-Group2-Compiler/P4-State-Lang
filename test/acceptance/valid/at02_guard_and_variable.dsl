Statemachine M {
<<<<<<< HEAD
  # VAR x = 5

  Start State A {
    ON Push IF x > 10 GO B
=======
  #VAR x = 5

  Start State A {
    ON 1 IF x > 10 GO B ELSE GO C
>>>>>>> dev
  }

  Final State B {
  }
}