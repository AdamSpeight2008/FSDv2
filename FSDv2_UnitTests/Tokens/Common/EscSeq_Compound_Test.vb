Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2
Imports FSDv2.FormatString.Common
<TestClass> Public Class EscSeq_Tests

  Const Cat = "Tokens.Common.Esc.Seq.Compound"

  <TestMethod, TestCategory(Cat)>
  Public Sub _00_()
    Dim Text = "\x"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.Unsupported, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _01_()
    Dim Text = "\x"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Verbatum)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.Unsupported, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _02_()
    Dim Text = "\x"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(Esc.Sequence.Simple))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
  End Sub

End Class