package com.paolobroglio.lox.interpreter.model.scanner


case class ScannerState(line: Int, currentCharPosition: Int)

object ScannerState {
  def apply(): ScannerState = ScannerState(0, 0)

}
