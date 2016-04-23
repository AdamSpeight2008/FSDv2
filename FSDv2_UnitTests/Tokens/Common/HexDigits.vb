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
    Assert.IsNull(res)


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
    Assert.AreEqual(22, res.Inner.Count)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(0).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(1).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(2).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(3).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(4).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(5).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(6).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(7).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(8).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(9).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(10).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(11).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(12).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(13).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(14).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(15).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(16).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(17).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(18).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(19).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(20).Kind)
    Assert.AreEqual(TokenKind.HexDigit, res.Inner(21).Kind)

  End Sub

End Class