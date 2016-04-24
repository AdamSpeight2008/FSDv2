Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2
Imports FSDv2.FormatString.Common
<TestClass>
Public Class EscSeq_Compound_Unicode

  Const Cat = "Tokens.Common.Esc.Seq.Compound.Unicode"

#Region "\u UnitTest"
  <TestMethod, TestCategory(Cat)>
  Public Sub _00_()
    Dim Text = "\u"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.Unsupported, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _01_()
    Dim Text = "\u"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Verbatum)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.Unsupported, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _02_()
    Dim Text = "\u"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Dim pe As ParseError = DirectCast(res, ParseError)
    Assert.AreEqual(ParseError.Reason.Invalid, pe.Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _03_()
    Dim Text = "\u0"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _04_()
    Dim Text = "\u00"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _05_()
    Dim Text = "\u000"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _06_()
    Dim Text = "\u0000"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _07_()
    Dim Text = "\u0000F"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Esc.Sequence.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
  End Sub
#End Region

End Class