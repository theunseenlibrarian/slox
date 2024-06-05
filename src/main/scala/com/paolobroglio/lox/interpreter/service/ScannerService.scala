package com.paolobroglio.lox.interpreter.service

import com.paolobroglio.lox.interpreter.model.error.ScannerErrors.{ScannerNonTerminatingNumberError, ScannerNonTerminatingStringError, ScannerUndefinedSymbolError}
import com.paolobroglio.lox.interpreter.model.error.SloxException
import com.paolobroglio.lox.interpreter.model.scanner.{KeywordToken, NumberLiteralScanStep, ReservedIdentifiers, ReservedKeywordScanStep, ScanStep, ScannerState, StringLiteralScanStep, Token, Tokens}

import scala.annotation.tailrec

object ScannerService {

  private def isAtEnd(source: String, position: Int): Boolean = position >= source.length

  private def advance(state: ScannerState, lineInc: Int, charPosInc: Int): ScannerState =
    state.copy(line = state.line + lineInc, currentCharPosition = state.currentCharPosition + charPosInc)

  private def peek(source: String, state: ScannerState): Char = source.charAt(state.currentCharPosition + 1)

  private def matchesNext(source: String, state: ScannerState,  toMatch: Char, matchingResult: KeywordToken, defaultResult: KeywordToken): ScanStep =
    if (peek(source, state) == toMatch)
      ScanStep(matchingResult, advance(state, 0, 2))
    else
      ScanStep(defaultResult, advance(state, 0, 1))

  private def scanComment(source: String, state: ScannerState): ScanStep = {
    @tailrec
    def loop(state: ScannerState): ScanStep =
      if (source.charAt(state.currentCharPosition) == '\n' || isAtEnd(source, state.currentCharPosition))
        ScanStep(state)
      else
        loop(advance(state, 0, 1))

    if (peek(source, state) == '/')
      loop(advance(state, 0, 1))
    else
      ScanStep(Tokens.Slash, advance(state, 0, 1))
  }

  private def scanStringLiteral(source: String, state: ScannerState): ScanStep = {
    @tailrec
    def accumulateLiteral(state: ScannerState, literal: String): StringLiteralScanStep =
      if (isAtEnd(source, state.currentCharPosition))
        throw new SloxException(ScannerNonTerminatingStringError())
      else if (source.charAt(state.currentCharPosition) == '"')
        StringLiteralScanStep(advance(state, 0, 2), literal)
      else {
        val newState =
          if (source.charAt(state.currentCharPosition) == '\n')
            advance(state, 1, 1)
          else
            advance(state, 0, 1)

        accumulateLiteral(newState, literal.appended(source.charAt(state.currentCharPosition)))
      }

    StringLiteralScanStep.toScanStep(accumulateLiteral(advance(state, 0, 1), ""))
  }

  private def scanNumberLiteral(source: String, state: ScannerState): ScanStep = {
    def notAllowed(char: Char): Boolean = !isDigit(char) && char != '.'
    @tailrec
    def accumulateLiteral(state: ScannerState, literal: String): NumberLiteralScanStep =
      if (isAtEnd(source, state.currentCharPosition))
        NumberLiteralScanStep(state, literal)
      else {
        val currentChar = source.charAt(state.currentCharPosition)
        if (notAllowed(currentChar))
          NumberLiteralScanStep(advance(state, 0, 1), literal)
        else
          accumulateLiteral(advance(state, 0, 1), literal.appended(currentChar))
      }

    NumberLiteralScanStep.toScanStep(accumulateLiteral(state, ""))
  }

  private def scanReservedKeyword(source: String, state: ScannerState): ScanStep = {
    @tailrec
    def accumulateIdentifier(state: ScannerState, identifier: String): ReservedKeywordScanStep = {
      if (isAtEnd(source, state.currentCharPosition))
        ReservedKeywordScanStep(state, identifier)
      else {
        val char = source.charAt(state.currentCharPosition)
        if (!isAlphaNumeric(char))
          ReservedKeywordScanStep(state, identifier)
        else
          accumulateIdentifier(advance(state, 0, 1), identifier.appended(char))
      }
    }

    ReservedKeywordScanStep.toScanStep(accumulateIdentifier(state, ""))
  }

  private def isDigit(char: Char): Boolean =
    char >= '0' && char <= '9'

  private def isAlphaNumeric(char: Char): Boolean =
    isAlpha(char) || isDigit(char)

  private def isAlpha(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || c == '_'

  def scan(source: String): List[Token] = {
    def getToken(state: ScannerState): ScanStep = {
      val currentChar = source.charAt(state.currentCharPosition)
      currentChar match {
        case '(' => ScanStep(Tokens.LeftParen, advance(state, 0, 1))
        case ')' => ScanStep(Tokens.RightParen, advance(state, 0, 1))
        case '{' => ScanStep(Tokens.LeftBrace, advance(state, 0, 1))
        case '}' => ScanStep(Tokens.RightBrace, advance(state, 0, 1))
        case ',' => ScanStep(Tokens.Comma, advance(state, 0, 1))
        case '.' => ScanStep(Tokens.Dot, advance(state, 0, 1))
        case '-' => ScanStep(Tokens.Minus, advance(state, 0, 1))
        case '+' => ScanStep(Tokens.Plus, advance(state, 0, 1))
        case '*' => ScanStep(Tokens.Star, advance(state, 0, 1))
        case ';' => ScanStep(Tokens.SemiColon, advance(state, 0, 1))
        case '!' => matchesNext(source, state, '=', Tokens.BangEqual, Tokens.Bang)
        case '=' => matchesNext(source, state, '=', Tokens.EqualEqual, Tokens.Equal)
        case '>' => matchesNext(source, state, '=', Tokens.GreaterEqual, Tokens.Greater)
        case '<' => matchesNext(source, state, '=', Tokens.LesserEqual, Tokens.Lesser)
        case '/' => scanComment(source, state)
        case '"' => scanStringLiteral(source, state)
        case c if isDigit(c) => scanNumberLiteral(source, state)
        case c if isAlpha(c) => scanReservedKeyword(source, state)
        case ' ' | '\t' | '\r' => ScanStep(advance(state, 0, 1))
        case '\n' => ScanStep(advance(state, 1, 1))
        case _ => throw new SloxException(ScannerUndefinedSymbolError(state.line, currentChar))
      }
    }

    @tailrec
    def loop(state: ScannerState, tokens: List[Token]): List[Token] =
      if (isAtEnd(source, state.currentCharPosition))
        tokens :+ Tokens.Eof
      else {
        val scanResult = getToken(state)
        val newTokenList = scanResult.maybeToken.map(t => tokens :+ t).getOrElse(tokens)

        loop(scanResult.state, newTokenList)
      }

    loop(ScannerState(), List.empty)
  }
}
