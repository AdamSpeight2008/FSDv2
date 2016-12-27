Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass>
Public Class Tokens_Common_Whitespace_UnitTest

  Const Cat = "Tokens.Common.Whitespace"

  <TestMethod, TestCategory(Cat)>
  Public Sub _00_()
    Dim Text = ""
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Whitespace.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(ParseError.EoT))
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _01_()
    Dim Text = " "
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Whitespace.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Whitespace))
    Assert.AreEqual(TokenKind.Whitespace, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub

End Class