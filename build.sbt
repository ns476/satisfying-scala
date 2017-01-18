name := "sudoku"

version := "1.0"

scalaVersion := "2.11.8"

unmanagedJars in Compile += file("lib/cafesat_2.11-0.01.jar")

mainClass in Compile := Some("com.footballradar.sudoku.SudokuSolver")
