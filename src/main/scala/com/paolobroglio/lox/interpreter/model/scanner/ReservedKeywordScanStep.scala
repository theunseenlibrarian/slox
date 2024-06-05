package com.paolobroglio.lox.interpreter.model.scanner

case class ReservedKeywordScanStep(state: ScannerState, token: Token)

object ReservedKeywordScanStep {
  def apply(state: ScannerState, accumulatedIdentifier: String): ReservedKeywordScanStep =
    ReservedKeywordScanStep(state, ReservedIdentifiers.getTypedReservedKeyword(accumulatedIdentifier))
  def toScanStep(reservedKeywordScanStep: ReservedKeywordScanStep): ScanStep = {
    ScanStep(reservedKeywordScanStep.token, reservedKeywordScanStep.state)
  }
}
