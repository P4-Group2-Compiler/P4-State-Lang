Statemachine M {
  # VAR x = 5
  # VAR y = 7
  StartFinal State A {
    ON Push IF x <= 10 GO B
    ON Pull IF x > 10 OR y >= 5 GO C

  }

  Final State B{
    ON Punch IF x > 4 AND y > 5 GO C
    ON Pack IF x + 2 == y GO C
    ON Grab IF x != y GO C
  }
  Final State C{
    ON Lay IF x - 2 < y * 2 GO D
  }

  State D{

  }
}
