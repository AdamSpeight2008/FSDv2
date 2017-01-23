Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass>
Public Class Tokens_Common_HexDigit_UnitTest

  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _00_()
    Dim Text = ""
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
  End Sub

  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _01_D0()
    Dim Text = "0"
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _02_D1()
    Dim Text = "1"
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _03_D2()
    Dim Text = "2"
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _04_D3()
    Dim Text = "3"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _05_D4()
    Dim Text = "4"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _06_D5()
    Dim Text = "5"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _07_D6()
    Dim Text = "6"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _08_D7()
    Dim Text = "7"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _09_D8()
    Dim Text = "8"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _10_D9()
    Dim Text = "9"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub

  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _11_DA()
    Dim Text = "A"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _12_DB()
    Dim Text = "B"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _13_DC()
    Dim Text = "C"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _14_DD()
    Dim Text = "D"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _15_DE()
    Dim Text = "E"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _16_DF()
    Dim Text = "F"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub

  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _17_Dla()
    Dim Text = "a"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _18_Dlb()
    Dim Text = "b"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _19_Dlc()
    Dim Text = "c"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _20_Dld()
    Dim Text = "d"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _21_Dle()
    Dim Text = "e"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _22_Dlf()
    Dim Text = "f"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.AreEqual(TokenKind.HexDigit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub

  <TestMethod, TestCategory("Tokens.Common.HexDigit")>
  Public Sub _23_X()
    Dim Text = "X"
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.HexDigit.TryParse(FirstPos, False)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(FormatString.Common.HexDigit))
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.NullParse, DirectCast(res, ParseError).Why)
  End Sub

End Class