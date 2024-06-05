package com.paolobroglio.lox.interpreter.model.scanner

import com.paolobroglio.lox.interpreter.model.scanner.Literal.DoubleOrString
import com.paolobroglio.lox.interpreter.model.scanner.TokenType.TokenType


object Literal {
  type DoubleOrString = Either[Double, String]
}

sealed trait Token {
  def lexeme: String
  def tokenType: TokenType
}

case class LiteralToken(tokenType: TokenType, literal: DoubleOrString) extends Token {
  override def lexeme: String = ""

  override def toString: String =
    literal match {
      case Left(value) => s"Num[$value]"
      case Right(value) => s"Str[$value]"
    }
}
case class KeywordToken(tokenType: TokenType) extends Token {
  override def lexeme: String = tokenType.toString
  override def toString: String = s"'$lexeme'"
}

case class UserDefinedIdentifier(lexeme: String) extends Token {
  override def tokenType: TokenType = TokenType.Identifier

  override def toString: String = s"UserDefined[$lexeme]"
}
