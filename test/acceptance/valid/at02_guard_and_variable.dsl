Statemachine M {
  int x = 5;

  Start State A {
    ON 1 IF x > 10 GO B ELSE GO C;
  }

  State B {
  }

  Final State C {
  }
}