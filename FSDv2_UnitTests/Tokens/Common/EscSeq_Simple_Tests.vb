Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2
Imports FSDv2.FormatString.Common
<TestClass> Public Class EscSeq_Simple_Tests

  Const Cat = "Tokens.Common.Esc.Seq.Simple"

  <TestMethod, TestCategory(Cat)>
  Public Sub _00_()
    Dim Text = "\'"
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.Unsupported, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _01_()
    Dim Text = "\'"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Verbatum, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.Unsupported, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _02_()
    Dim Text = "\'"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.Simple))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _03_()
    Dim Text = "\""" ' \"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.Simple))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _04_()
    Dim Text = "\\"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.Simple))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _05_()
    Dim Text = "\0"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.Simple))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _06_()
    Dim Text = "\a"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.Simple))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _07_()
    Dim Text = "\b"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.Simple))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _08_()
    Dim Text = "\f"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.Simple))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _09_()
    Dim Text = "\n"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.Simple))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _10_()
    Dim Text = "\r"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.Simple))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _11_()
    Dim Text = "\t"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.Simple))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _12_()
    Dim Text = "\v"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.Simple))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
  End Sub
End Class