package com.paolobroglio.lox.interpreter.model.error

sealed trait SloxError {
  def message: String
}


object ScannerErrors {
  case class ScannerUndefinedSymbolError(line: Int, char: Char) extends SloxError {
    override def message: String = s"Scanning error at line: $line - Undefined symbol $char"
  }
  case class ScannerNonTerminatingStringError() extends SloxError {
    override def message: String = s"Scanning error: non terminating string literal"
  }
  case class ScannerNonTerminatingNumberError() extends SloxError {
    override def message: String = s"Scanning error: non terminating number literal"
  }
}
