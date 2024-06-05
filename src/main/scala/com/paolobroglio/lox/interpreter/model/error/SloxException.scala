package com.paolobroglio.lox.interpreter.model.error

case class SloxException(error: SloxError) extends RuntimeException(error.message)

object SloxException {}
