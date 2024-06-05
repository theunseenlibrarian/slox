package com.paolobroglio.lox.interpreter.model.scanner

case class ScanStep(maybeToken: Option[Token], state: ScannerState)

object ScanStep {
  def apply(token: Token, state: ScannerState): ScanStep = ScanStep(Some(token), state)
  def apply(state: ScannerState): ScanStep = ScanStep(None, state)
}
