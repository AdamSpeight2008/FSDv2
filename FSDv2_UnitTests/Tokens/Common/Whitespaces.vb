Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass>
Public Class Tokens_Common_Whitespaces_UnitTest

  <TestMethod, TestCategory("Tokens.Common.Whitespaces")>
  Public Sub _00_()
    Dim Text = ""
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Whitespaces.TryParse(FirstPos, False)


  End Sub

  <TestMethod, TestCategory("Tokens.Common.Whitespaces")>
  Public Sub _01_()
    Dim Text = " "
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Whitespaces.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Whitespaces))
    Assert.AreEqual(TokenKind.Whitespaces, res.Kind)
    Assert.AreEqual(0, res.Span.Start.Index)
    Assert.AreEqual(1, res.Span.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
    Assert.AreEqual(1, res.InnerTokens.Count)
    Assert.AreEqual(TokenKind.Whitespace, res(0).Kind)

  End Sub

End Class