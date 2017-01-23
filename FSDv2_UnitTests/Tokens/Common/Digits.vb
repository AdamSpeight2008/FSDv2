Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass>
Public Class Tokens_Common_Digits_UnitTest

  Const Cat = "Parser:Tokens.Common.Digits"

  <TestMethod, TestCategory("Parser"), TestCategory(Cat)>
  Public Sub _00_()
    Dim Text = ""
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digits.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(ParseError.EoT))

  End Sub

  <TestMethod, TestCategory("Parser"), TestCategory(Cat)>
  Public Sub _01_()
    Dim Text = "0123456789"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digits.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Digits))
    Assert.AreEqual(TokenKind.Digits, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(10, res.Span?.Size)
    Assert.AreEqual("(  0: 10)", res.Span.ToString)
    Assert.AreEqual(10, res.InnerTokens.Count)
    For i = 0 To 9
      Assert.AreEqual(TokenKind.Digit, res(i).Kind)
      Dim digit = TryCast(res(i), FormatString.Common.Digit)
      Assert.IsNotNull(digit)
      Dim value = digit.GetValue
      Assert.AreEqual(True, value.HasValue)
      Assert.AreEqual(CType(i, Numerics.BigInteger), value.Value)
    Next
  End Sub

End Class