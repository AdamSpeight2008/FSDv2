Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass>
Public Class Tokens_Common_Digits_UnitTest

  <TestMethod, TestCategory("Tokens.Common.Digits")>
  Public Sub _00_()
    Dim Text = ""
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digits.TryParse(FirstPos)


  End Sub

  <TestMethod, TestCategory("Tokens.Common.Digits")>
  Public Sub _01_()
    Dim Text = "0123456789"
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digits.TryParse(FirstPos)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Digits))
    Assert.AreEqual(TokenKind.Digits, res.Kind)
    Assert.AreEqual(0, res.Span.Start.Index)
    Assert.AreEqual(10, res.Span.Size)
    Assert.AreEqual("(  0: 10)", res.Span.ToString)
    Assert.AreEqual(10, res.InnerTokens.Count)
    Assert.AreEqual(TokenKind.Digit, res(0).Kind)
    Assert.AreEqual(TokenKind.Digit, res(1).Kind)
    Assert.AreEqual(TokenKind.Digit, res(2).Kind)
    Assert.AreEqual(TokenKind.Digit, res(3).Kind)
    Assert.AreEqual(TokenKind.Digit, res(4).Kind)
    Assert.AreEqual(TokenKind.Digit, res(5).Kind)
    Assert.AreEqual(TokenKind.Digit, res(6).Kind)
    Assert.AreEqual(TokenKind.Digit, res(7).Kind)
    Assert.AreEqual(TokenKind.Digit, res(8).Kind)
    Assert.AreEqual(TokenKind.Digit, res(9).Kind)

  End Sub

End Class