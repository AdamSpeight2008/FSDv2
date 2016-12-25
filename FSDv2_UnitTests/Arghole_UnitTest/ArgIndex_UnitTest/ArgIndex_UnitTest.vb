Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass>
Public Class ArgIndex_UnitTest
  '
  ' ArgIndex ::= Digits Whitespaces?
  '

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _00_()
    Dim Text = ""
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Index.TryParse(FirstPos, False)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(FormatString.ArgHole.Index))
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.EoT, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _01_()
    Dim Text = "0"
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Index.TryParse(FirstPos, False)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Index))
    Assert.AreEqual(TokenKind.ArgHole_Index, res.Kind)
    Assert.AreEqual(1, res.InnerTokens.Count)
    Assert.AreEqual(TokenKind.Digits, res(0).Kind)
    Assert.AreEqual(1, res(0).InnerTokens.Count)
    Assert.AreEqual(TokenKind.Digit, res(0)(0).Kind)
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _02_()
    Dim Text = "01"
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Index.TryParse(FirstPos, False)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Index))
    Assert.AreEqual(TokenKind.ArgHole_Index, res.Kind)
    Assert.AreEqual(1, res.InnerTokens.Count)
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
    Assert.AreEqual(TokenKind.Digits, res(0).Kind)
    Assert.AreEqual(2, res(0).InnerTokens.Count)
    Assert.AreEqual("(  0:  2)", res(0).Span.ToString)
    Assert.AreEqual("01", res(0).Span.Text)
    Assert.AreEqual(TokenKind.Digit, res(0)(0).Kind)
    Assert.AreEqual(TokenKind.Digit, res(0)(1).Kind)
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _03_()
    Dim Text = "0  "
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Index.TryParse(FirstPos, False)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Index))
    Assert.AreEqual(TokenKind.ArgHole_Index, res.Kind)
    Assert.AreEqual("(  0:  3)", res.Span.ToString)
    Assert.AreEqual(2, res.InnerTokens.Count)
    Assert.AreEqual(TokenKind.Digits, res(0).Kind)

    Assert.AreEqual("(  0:  1)", res(0).Span.ToString)
    Assert.AreEqual("0", res(0).Span.Text)
    Assert.AreEqual(1, res(0).InnerTokens.Count)
    Assert.AreEqual(TokenKind.Digit, res(0)(0).Kind)

    Assert.AreEqual(TokenKind.Whitespaces, res(1).Kind)
    Assert.AreEqual("(  1:  2)", res(1).Span.ToString)
    Assert.AreEqual(2, res(1).InnerTokens.Count)
    Assert.AreEqual("  ", res(1).Span.Text)
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _04_()
    Dim Text = "01  "
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Index.TryParse(FirstPos, False)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Index))
    Assert.AreEqual(TokenKind.ArgHole_Index, res.Kind)
    Assert.AreEqual("(  0:  4)", res.Span.ToString)
    Assert.AreEqual(2, res.InnerTokens.Count)

    Assert.AreEqual(TokenKind.Digits, res.InnerTokens(0).Kind)
    Assert.AreEqual("(  0:  2)", res(0).Span.ToString)
    Assert.AreEqual("01", res(0).Span.Text)
    Assert.AreEqual(2, res(0).InnerTokens.Count)
    Assert.AreEqual(TokenKind.Digit, res(0)(0).Kind)
    Assert.AreEqual(TokenKind.Digit, res(0)(1).Kind)

    Assert.AreEqual(TokenKind.Whitespaces, res.InnerTokens(1).Kind)
    Assert.AreEqual("(  2:  2)", res(1).Span.ToString)
    Assert.AreEqual("  ", res(1).Span.Text)
    Assert.AreEqual(2, res(1).InnerTokens.Count)
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _05_()
    Dim Text = " 0"
        ' (0:2) ArgIndex
        ' [0] (0:1) ParseError (Why:= UnexpectedChacters)
        '   [0] (0:1) Digits ?
        ' [1] (1:1) Digits
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim ParseResult = FormatString.ArgHole.Index.TryParse(FirstPos, False)
    Assert.IsNotNull(ParseResult)
    Assert.IsInstanceOfType(ParseResult, GetType(FormatString.ArgHole.Index))
    Dim Expected =
"(  0:  2)  ArgHole_Index
  [ 0]  (  0:  1)  ParseError.UnexpectedCharacter
  [ 1]  (  1:  1)  Digits
    [ 0]  (  1:  1)  Digit
"
    Dim Actual = ParseResult.AsString
    Assert.AreEqual(Expected, Actual)

  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _06_()
    Dim Text = " 0  "
        ' (0:2) ArgIndex
        ' [0] (0:1) ParseError (Why:= UnexpectedChacters)
        '   [0] (0:1) Digits ?
        ' [1] (1:1) Digits
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim ParseResult = FormatString.ArgHole.Index.TryParse(FirstPos, False)
    Assert.IsNotNull(ParseResult)
    Assert.IsInstanceOfType(ParseResult, GetType(FormatString.ArgHole.Index))

    Dim Expected =
"(  0:  4)  ArgHole_Index
  [ 0]  (  0:  1)  ParseError.UnexpectedCharacter
  [ 1]  (  1:  1)  Digits
    [ 0]  (  1:  1)  Digit
  [ 2]  (  2:  2)  Whitespaces
    [ 0]  (  2:  1)  Whitespace
    [ 1]  (  3:  1)  Whitespace
"
    Dim Actual = ParseResult.AsString
    Assert.AreEqual(Expected, Actual)
  End Sub

End Class