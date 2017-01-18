package com.footballradar.sudoku

import cafesat.api.FormulaBuilder._
import cafesat.api.Formulas._
import cafesat.api.Solver

object SudokuSolver {
  type Row = Int
  type Col = Int
  type Value = Int

  def all(f: Iterable[Formula]): Formula = f.reduce(_ && _)
  def any(f: Iterable[Formula]): Formula = f.reduce(_ || _)

  implicit class FormulaWithImplies(x: Formula) {
    def ===>(y: Formula): Formula = {
      !x || y
    }
  }

  def main(args: Array[String]): Unit = {
    val props =
      for {
        row <- 1.to(9)
        col <- 1.to(9)
        value <- 1.to(9)
      } yield (
        row,
        col,
        value,
        propVar(s"${row}_${col}_${value}")
      )

    def eachProp(f: ((Row, Col, Value, Formula)) => Formula): Formula = all(props.map(f))

    val rowValueOnlyOccursOnce =
      eachProp { case (row, col, value, prop) =>
        prop ===> !any(
          for {
            (row_, col_, value_, prop_) <- props
            if row_ == row
            if col_ != col
            if value_ == value
          } yield prop_
        )
      }

    val colValueOnlyOccursOnce = eachProp { case (row, col, value, prop) =>
      prop ===> !any(
        for {
          (row_, col_, value_, prop_) <- props
          if row_ != row
          if col_ == col
          if value_ == value
        } yield prop_
      )
    }

    def inSameSquare(a: Int, b: Int): Boolean =
      ((a - 1) / 3) == ((b - 1) / 3)

    val squareValueOnlyOccursOnce =
      eachProp { case (row, col, value, prop) =>
        prop ===> !any(
          for {
            (row_, col_, value_, prop_) <- props
            if inSameSquare(row_, row)
            if inSameSquare(col_, col)
            if row_ != row || col_ != col
            if value_ == value
          } yield prop_
        )
      }

    val eachCellHoldsValue = all {
      val cellProps =
        props.groupBy { case (row, col, _, _) =>
          (row, col)
        }.values
          .map { propsWithMetadata =>
            propsWithMetadata.map { case (_, _, _, prop) =>
              prop
            }
          }
      cellProps.map(any)
    }

    val partialSolution: Set[(Row, Col, Value)] = Set(
      (1, 1, 8),
      (2, 3, 3),
      (3, 2, 7),
      (2, 4, 6),
      (3, 5, 9),
      (3, 7, 2),
      (4, 2, 5),
      (4, 6, 7),
      (5, 5, 4),
      (5, 6, 5),
      (6, 4, 1),
      (5, 7, 7),
      (6, 8, 3),
      (7, 3, 1),
      (8, 3, 8),
      (9, 2, 9),
      (8, 4, 5),
      (9, 7, 4),
      (7, 8, 6),
      (8, 8, 1),
      (7, 9, 8)
    )

    val partialSolutionMustBeFilledIn = all {
      for {
        (row_, col_, value_, prop_) <- props
        if partialSolution.contains((row_, col_, value_))
      } yield prop_
    }

    val model = rowValueOnlyOccursOnce &&
      colValueOnlyOccursOnce &&
      squareValueOnlyOccursOnce &&
      eachCellHoldsValue &&
      partialSolutionMustBeFilledIn

    val Some(rawSolution) = Solver.solveForSatisfiability(model)

    val solution = props.collect { case (row, col, value, prop) if rawSolution(prop) => (row, col, value) }

    solution.foreach { case (row, col, value) =>
      if (col == 1 && row != 1) {
        print('\n')
        if (row == 4 || row == 7) {
          println("-" * 11)
        }
      }
      print(value)
      if (col == 3 || col == 6) {
        print("|")
      }

    }
    print('\n')
  }
}
