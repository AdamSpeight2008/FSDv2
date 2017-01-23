Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass>
Public Class Tokens_Common_Digit_UnitTest
  Const Cat = "Tokens.Common.Digit"

  <TestMethod, TestCategory(Cat)>
  Public Sub _00_()
    Dim Text = ""
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digit.TryParse(FirstPos, False)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _01_D0()
    Dim Text = "0"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Digit))
    Assert.AreEqual(TokenKind.Digit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span?.ToString)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _02_D1()
    Dim Text = "1"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Digit))
    Assert.AreEqual(TokenKind.Digit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span?.ToString)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _03_D2()
    Dim Text = "2"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Digit))
    Assert.AreEqual(TokenKind.Digit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _04_D3()
    Dim Text = "3"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Digit))
    Assert.AreEqual(TokenKind.Digit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _05_D4()
    Dim Text = "4"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Digit))
    Assert.AreEqual(TokenKind.Digit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _06_D5()
    Dim Text = "5"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Digit))
    Assert.AreEqual(TokenKind.Digit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _07_D6()
    Dim Text = "6"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Digit))
    Assert.AreEqual(TokenKind.Digit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _08_D7()
    Dim Text = "7"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Digit))
    Assert.AreEqual(TokenKind.Digit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _09_D8()
    Dim Text = "8"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Digit))
    Assert.AreEqual(TokenKind.Digit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _10_D9()
    Dim Text = "9"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digit.TryParse(FirstPos, False)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Digit))
    Assert.AreEqual(TokenKind.Digit, res.Kind)
    Assert.AreEqual(0, res.Span?.Start?.Index)
    Assert.AreEqual(1, res.Span?.Size)
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _11_X()
    Dim Text = "X"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Digit.TryParse(FirstPos, False)
    Assert.IsNotInstanceOfType(res, GetType(FormatString.Common.Digit))
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.NullParse, DirectCast(res, ParseError).Why)
  End Sub

End Class