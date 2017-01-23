Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2
Imports FSDv2.FormatString.Common
<TestClass>
Public Class EscSeq_Compound_HexaDecimal

  Const Cat = "Tokens.Common.Esc.Seq.Compound.HexaDecimal"

  <TestMethod, TestCategory(Cat)>
  Public Sub _00_()
    Dim Text = "\x"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.Unsupported, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _01_()
    Dim Text = "\x"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Verbatum, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.Unsupported, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _02_()
    Dim Text = "\x"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Dim pe As ParseError = DirectCast(res, ParseError)
    Assert.AreEqual(ParseError.Reason.Partial, pe.Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _03_()
    Dim Text = "\x0"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.HexaDecimal))
    Dim hx = DirectCast(res, Esc.Sequence.HexaDecimal)
    Assert.AreEqual("(  0:  3)", hx.Span.ToString)
    Assert.AreEqual("\x0", hx.Span.Text)
    Assert.AreEqual(2, hx.InnerTokens.Count)
    Assert.IsInstanceOfType(hx(0), GetType(Esc.SeqHead))
    Assert.IsInstanceOfType(hx(1), GetType(FormatString.Common.HexDigits))
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _04_()
    Dim Text = "\x00"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.HexaDecimal))
    Dim hx = DirectCast(res, Esc.Sequence.HexaDecimal)
    Assert.AreEqual("(  0:  4)", hx.Span.ToString)
    Assert.AreEqual("\x00", hx.Span.Text)
    Assert.AreEqual(2, hx.InnerTokens.Count)
    Assert.IsInstanceOfType(hx(0), GetType(Esc.SeqHead))
    Assert.IsInstanceOfType(hx(1), GetType(FormatString.Common.HexDigits))
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _05_()
    Dim Text = "\x000"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.HexaDecimal))
    Dim hx = DirectCast(res, Esc.Sequence.HexaDecimal)
    Assert.AreEqual("(  0:  5)", hx.Span.ToString)
    Assert.AreEqual("\x000", hx.Span.Text)
    Assert.AreEqual(2, hx.InnerTokens.Count)
    Assert.IsInstanceOfType(hx.InnerTokens(0), GetType(Esc.SeqHead))
    Assert.IsInstanceOfType(hx.InnerTokens(1), GetType(FormatString.Common.HexDigits))
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _06_()
    Dim Text = "\x0000"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.HexaDecimal))
    Dim hx = DirectCast(res, Esc.Sequence.HexaDecimal)
    Assert.AreEqual("(  0:  6)", hx.Span.ToString)
    Assert.AreEqual("\x0000", hx.Span.Text)
    Assert.AreEqual(2, hx.InnerTokens.Count)
    Assert.IsInstanceOfType(hx(0), GetType(Esc.SeqHead))
    Assert.IsInstanceOfType(hx(1), GetType(FormatString.Common.HexDigits))
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _07_()
    Dim Text = "\x0000F"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.HexaDecimal))
    Dim hx = DirectCast(res, Esc.Sequence.HexaDecimal)
    Assert.AreEqual("(  0:  6)", hx.Span.ToString)
    Assert.AreEqual("\x0000", hx.Span.Text)
    Assert.AreEqual(2, hx.InnerTokens.Count)
    Assert.IsInstanceOfType(hx(0), GetType(Esc.SeqHead))
    Assert.IsInstanceOfType(hx(1), GetType(FormatString.Common.HexDigits))
  End Sub

End Class