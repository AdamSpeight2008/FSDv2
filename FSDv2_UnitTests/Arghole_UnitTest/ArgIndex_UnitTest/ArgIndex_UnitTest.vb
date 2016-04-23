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
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Index.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(FormatString.ArgHole.Index))
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.EoT, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _01_()
    Dim Text = "0"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Index.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Index))
    Assert.AreEqual(TokenKind.ArgHole_Index, res.Kind)
    Assert.AreEqual(1, res.Inner.Count)
    Assert.AreEqual(TokenKind.Digits, res.Inner(0).Kind)
    Assert.AreEqual(1, res.Inner(0).Inner.Count)
    Assert.AreEqual(TokenKind.Digit, res.Inner(0).Inner(0).Kind)
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _02_()
    Dim Text = "01"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Index.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Index))
    Assert.AreEqual(TokenKind.ArgHole_Index, res.Kind)
    Assert.AreEqual(1, res.Inner.Count)
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
    Assert.AreEqual(TokenKind.Digits, res.Inner(0).Kind)
    Assert.AreEqual(2, res.Inner(0).Inner.Count)
    Assert.AreEqual("(  0:  2)", res.Inner(0).Span.ToString)
    Assert.AreEqual("01", res.Inner(0).Span.Text)
    Assert.AreEqual(TokenKind.Digit, res.Inner(0).Inner(0).Kind)
    Assert.AreEqual(TokenKind.Digit, res.Inner(0).Inner(1).Kind)
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _03_()
    Dim Text = "0  "
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Index.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Index))
    Assert.AreEqual(TokenKind.ArgHole_Index, res.Kind)
    Assert.AreEqual("(  0:  3)", res.Span.ToString)
    Assert.AreEqual(2, res.Inner.Count)
    Assert.AreEqual(TokenKind.Digits, res.Inner(0).Kind)

    Assert.AreEqual("(  0:  1)", res.Inner(0).Span.ToString)
    Assert.AreEqual("0", res.Inner(0).Span.Text)
    Assert.AreEqual(1, res.Inner(0).Inner.Count)
    Assert.AreEqual(TokenKind.Digit, res.Inner(0).Inner(0).Kind)

    Assert.AreEqual(TokenKind.Whitespaces, res.Inner(1).Kind)
    Assert.AreEqual("(  1:  2)", res.Inner(1).Span.ToString)
    Assert.AreEqual(2, res.Inner(1).Inner.Count)
    Assert.AreEqual("  ", res.Inner(1).Span.Text)
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _04_()
    Dim Text = "01  "
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Index.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Index))
    Assert.AreEqual(TokenKind.ArgHole_Index, res.Kind)
    Assert.AreEqual("(  0:  4)", res.Span.ToString)
    Assert.AreEqual(2, res.Inner.Count)

    Assert.AreEqual(TokenKind.Digits, res.Inner(0).Kind)
    Assert.AreEqual("(  0:  2)", res.Inner(0).Span.ToString)
    Assert.AreEqual("01", res.Inner(0).Span.Text)
    Assert.AreEqual(2, res.Inner(0).Inner.Count)
    Assert.AreEqual(TokenKind.Digit, res.Inner(0).Inner(0).Kind)
    Assert.AreEqual(TokenKind.Digit, res.Inner(0).Inner(1).Kind)

    Assert.AreEqual(TokenKind.Whitespaces, res.Inner(1).Kind)
    Assert.AreEqual("(  2:  2)", res.Inner(1).Span.ToString)
    Assert.AreEqual("  ", res.Inner(1).Span.Text)
    Assert.AreEqual(2, res.Inner(1).Inner.Count)
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _05_()
    Dim Text = " 0"
    ' (0:2) ArgIndex
    ' [0] (0:1) ParseError (Why:= UnexpectedChacters)
    '   [0] (0:1) Digits ?
    ' [1] (1:1) Digits
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Index.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Index))
    Assert.AreEqual(TokenKind.ArgHole_Index, res.Kind)
    Assert.AreEqual(2, res.Inner.Count)

    Assert.AreEqual(TokenKind.ParseError, res.Inner(0).Kind)
    Dim t0 = TryCast(res.Inner(0), ParseError)
    Assert.IsNotNull(t0)
    Assert.AreEqual(ParseError.Reason.UnexpectedCharacter, t0.Why)
    Assert.AreEqual("(  0:  1)", t0.Span.ToString)
    Assert.AreEqual(" ", t0.Span.Text)

    Assert.AreEqual(TokenKind.Digits, res.Inner(1).Kind)
    Assert.AreEqual("(  1:  1)", res.Inner(1).Span.ToString)
    Assert.AreEqual("0", res.Inner(1).Span.Text)
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgIndex")>
  Public Sub _06_()
    Dim Text = " 0  "
    ' (0:2) ArgIndex
    ' [0] (0:1) ParseError (Why:= UnexpectedChacters)
    '   [0] (0:1) Digits ?
    ' [1] (1:1) Digits
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Index.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Index))
    Assert.AreEqual(TokenKind.ArgHole_Index, res.Kind)
    Assert.AreEqual(3, res.Inner.Count)

    Assert.AreEqual(TokenKind.ParseError, res.Inner(0).Kind)
    Dim t0 = TryCast(res.Inner(0), ParseError)
    Assert.IsNotNull(t0)
    Assert.AreEqual(ParseError.Reason.UnexpectedCharacter, t0.Why)
    Assert.AreEqual("(  0:  1)", t0.Span.ToString)
    Assert.AreEqual(" ", t0.Span.Text)

    Assert.AreEqual(TokenKind.Digits, res.Inner(1).Kind)
    Assert.AreEqual("(  1:  1)", res.Inner(1).Span.ToString)
    Assert.AreEqual("0", res.Inner(1).Span.Text)

    Assert.AreEqual(TokenKind.Whitespaces, res.Inner(2).Kind)
    Assert.AreEqual(2, res.Inner(2).Inner.Count)
    Assert.AreEqual("(  2:  2)", res.Inner(2).Span.ToString)
    Assert.AreEqual("  ", res.Inner(2).Span.Text)
  End Sub

End Class