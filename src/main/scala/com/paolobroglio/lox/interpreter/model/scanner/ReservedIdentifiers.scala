package com.paolobroglio.lox.interpreter.model.scanner

import com.paolobroglio.lox.interpreter.model.scanner.TokenType.TokenType

object ReservedIdentifiers {
  val keywords: Map[String, Token] = scala.collection.immutable.Map(
    "and" -> Tokens.And,
    "class" -> Tokens.Class,
    "else" -> Tokens.Else,
    "false" -> Tokens.False,
    "for" -> Tokens.For,
    "fun" -> Tokens.Function,
    "if" -> Tokens.If,
    "nil" -> Tokens.Nil,
    "or" -> Tokens.Or,
    "print" -> Tokens.Print,
    "return" -> Tokens.Return,
    "super" -> Tokens.Super,
    "this" -> Tokens.This,
    "true" -> Tokens.True,
    "var" -> Tokens.Var,
    "while" -> Tokens.While
  )

  def getTypedReservedKeyword(key: String): Token = keywords.getOrElse(key, UserDefinedIdentifier(key))
}
