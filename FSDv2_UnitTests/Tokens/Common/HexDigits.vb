Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass>
Public Class Tokens_Common_HexDigits_UnitTest

  <TestMethod, TestCategory("Tokens.Common.HexDigits")>
  Public Sub _00_()
    Dim Text = ""
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
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
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigits.TryParse(FirstPos)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigits))
    Assert.AreEqual(TokenKind.HexDigits, res.Kind)
    Assert.AreEqual(0, res.Span.Start.Index)
    Assert.AreEqual(22, res.Span.Size)
    Assert.AreEqual("(  0: 22)", res.Span.ToString)
    Assert.AreEqual(22, res.InnerTokens.Count)
    Assert.AreEqual(TokenKind.HexDigit, res(0).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(1).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(2).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(3).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(4).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(5).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(6).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(7).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(8).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(9).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(10).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(11).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(12).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(13).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(14).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(15).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(16).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(17).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(18).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(19).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(20).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res(21).Kind)

  End Sub

End Class