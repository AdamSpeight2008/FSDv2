Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass>
Public Class Tokens_Common_HexDigits_UnitTest

  <TestMethod, TestCategory("Tokens.Common.HexDigits")>
  Public Sub _00_()
    Dim Text = ""
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigits.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.EoT, DirectCast(res, ParseError).Why)


  End Sub

  <TestMethod, TestCategory("Tokens.Common.HexDigits")>
  Public Sub _01_()
    Dim Text = "0123456789ABCDEFabcdef"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigits.TryParse(FirstPos)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigits))
    Assert.AreEqual(TokenKind.HexDigits, res.Kind)
    Assert.AreEqual(0, res.Span.Start.Index)
    Assert.AreEqual(22, res.Span.Size)
    Assert.AreEqual("(  0: 22)", res.Span.ToString)
    Assert.AreEqual(22, res.InnerTokens.Count)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(0).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(1).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(2).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(3).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(4).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(5).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(6).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(7).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(8).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(9).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(10).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(11).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(12).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(13).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(14).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(15).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(16).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(17).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(18).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(19).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(20).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.InnerTokens(21).Kind)

  End Sub

End Class