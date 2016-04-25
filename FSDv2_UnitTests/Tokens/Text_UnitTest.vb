Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2
Imports FSDv2.FormatString

<TestClass>
Public Class Text_UnitTests


  Const Cat = "Tokens.Text"

  <TestMethod, TestCategory(Cat)>
  Public Sub _00_()
    Dim Text = ""
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Text.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Text))
    Assert.AreEqual("( -1:  0)", res.Span.ToString)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _01_()
    Dim Text = "abc"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Text.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Text))
    Assert.AreEqual("(  0:  3)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _02_()
    Dim Text = "abc\b"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Text.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Text))
    Assert.AreEqual("(  0:  5)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _03_()
    Dim Text = "abc\b"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Text.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Text))
    Assert.AreEqual("(  0:  5)", res.Span.ToString)
    Assert.AreEqual(2, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(Text))
    Assert.IsInstanceOfType(res.Inner(1), GetType(Common.Esc.Sequence.Simple))
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _04_()
    Dim Text = "abc\q"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Text.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Text))
    Assert.AreEqual("(  0:  5)", res.Span.ToString)
    Assert.AreEqual(2, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(Text))
    Assert.IsInstanceOfType(res.Inner(1), GetType(ParseError))
  End Sub
End Class
