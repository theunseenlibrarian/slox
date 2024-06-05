package com.paolobroglio.lox.interpreter.model.scanner

case class StringLiteralScanStep(state: ScannerState, accumulatedLiteral: String)

object StringLiteralScanStep {
  def toScanStep(stringLiteralScanStep: StringLiteralScanStep): ScanStep =
    ScanStep(Tokens.stringLiteral(stringLiteralScanStep.accumulatedLiteral), stringLiteralScanStep.state)
}
