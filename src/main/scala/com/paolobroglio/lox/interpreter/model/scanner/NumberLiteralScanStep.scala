package com.paolobroglio.lox.interpreter.model.scanner

case class NumberLiteralScanStep(state: ScannerState, accumulatedLiteral: String)

object NumberLiteralScanStep {
  def toScanStep(numberLiteralScanStep: NumberLiteralScanStep): ScanStep = {
    val number =
      if (numberLiteralScanStep.accumulatedLiteral.contains("."))
        numberLiteralScanStep.accumulatedLiteral.toDouble
      else
        numberLiteralScanStep.accumulatedLiteral.toInt

    ScanStep(Tokens.numberLiteral(number), numberLiteralScanStep.state)
  }
}
