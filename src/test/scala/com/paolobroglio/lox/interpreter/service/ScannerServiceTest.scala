package com.paolobroglio.lox.interpreter.service

import com.paolobroglio.lox.interpreter.model.scanner.{KeywordToken, TokenType, Tokens, UserDefinedIdentifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import flatspec._
import matchers._

class ScannerServiceTest extends AnyFlatSpec {
  "ScannerService" should "scan correctly one char tokens" in {
    val res = ScannerService.scan("(){}+-*,;")

    assert(res === List(
      Tokens.LeftParen,
      Tokens.RightParen,
      Tokens.LeftBrace,
      Tokens.RightBrace,
      Tokens.Plus,
      Tokens.Minus,
      Tokens.Star,
      Tokens.Comma,
      Tokens.SemiColon,
      Tokens.Eof
    ))
  }
  "ScannerService" should "scan correctly operators" in {
    val res = ScannerService.scan("! != > >= < <= = ==")

    assert(res === List(
      Tokens.Bang,
      Tokens.BangEqual,
      Tokens.Greater,
      Tokens.GreaterEqual,
      Tokens.Lesser,
      Tokens.LesserEqual,
      Tokens.Equal,
      Tokens.EqualEqual,
      Tokens.Eof
    ))
  }
  "ScannerService" should "ignore correctly line comments" in {
    val res = ScannerService.scan("// a comment\n+")

    assert(res === List(
      Tokens.Plus,
      Tokens.Eof
    ))
  }
  "ScannerService" should "scan correctly multiline source" in {
    val source =
      """
        |// this is a comment
        |(( )){} // grouping stuff
        |!*+-/=<> <= == // operators
        |"and this is a string"
        |12345
        |1 + 1 = 2
        |x = x + 1
        |if else
        |""".stripMargin

    val res = ScannerService.scan(source)

    println(res)
  }
  "ScannerService" should "scan correctly string literal" in {
    val source = "\"astring\""

    val res = ScannerService.scan(source)

    assert(res === List(
      Tokens.stringLiteral("astring"),
      Tokens.Eof
    ))
  }
  "ScannerService" should "scan correctly string literal with newline" in {
    val source = "\"astring\nanotherstring\""

    val res = ScannerService.scan(source)

    assert(res === List(
      Tokens.stringLiteral("astring\nanotherstring"),
      Tokens.Eof
    ))
  }
  "ScannerService" should "scan correctly number literal" in {
    val source = "12345"

    val res = ScannerService.scan(source)

    assert(res === List(
      Tokens.numberLiteral(12345),
      Tokens.Eof
    ))
  }
  "ScannerService" should "scan correctly number literal with point" in {
    val source = "12345.1230"

    val res = ScannerService.scan(source)

    assert(res === List(
      Tokens.numberLiteral(12345.1230),
      Tokens.Eof
    ))
  }
  "ScannerService" should "scan correctly reserved keyword" in {
    val source = "fun"

    val res = ScannerService.scan(source)

    assert(res === List(
      Tokens.Function,
      Tokens.Eof
    ))
  }
  "ScannerService" should "scan correctly user defined identifier" in {
    val source = "x"

    val res = ScannerService.scan(source)

    assert(res === List(
      UserDefinedIdentifier("x"),
      Tokens.Eof
    ))
  }
}
