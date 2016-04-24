Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass>
Public Class ArgFormat_UnitTests

  '
  '  ArgFormat ::= Align_Head Align_Body
  ' Format_Head ::= Comma Whitespaces? 
  ' Format_Body ::= MinusSign? Digits Whitespaces?
  '
  Const Cat = "Tokens.Arghole.ArgFormat"

  <TestMethod, TestCategory(Cat)>
  Public Sub _00_()
    Dim Text = ""
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Format.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.EoT, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _01_()
    Dim Text = ":"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Format.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Format))
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
    Assert.AreEqual(1, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(FormatString.ArgHole.Format.Head))
#Region "Check Format.Head"
    Dim Head = res.Inner(0)
    Assert.AreEqual("(  0:  1)", Head.Span.ToString)
    Assert.AreEqual(1, Head.Inner.Count)
    Assert.IsInstanceOfType(Head.Inner(0), GetType(FormatString.ArgHole.Format.Colon))
    Assert.AreEqual("(  0:  1)", Head.Inner(0).Span.ToString)
#End Region
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _02_()
    Dim Text = ":{{"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Format.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Format))
    Assert.AreEqual("(  0:  3)", res.Span.ToString)
    Assert.AreEqual(2, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(FormatString.ArgHole.Format.Head))
    Assert.IsInstanceOfType(res.Inner(1), GetType(FormatString.ArgHole.Format.Body))
#Region "Check Format.Head"
    Dim Head = res.Inner(0)
    Assert.AreEqual("(  0:  1)", Head.Span.ToString)
    Assert.AreEqual(1, Head.Inner.Count)
    Assert.IsInstanceOfType(Head.Inner(0), GetType(FormatString.ArgHole.Format.Colon))
    Assert.AreEqual("(  0:  1)", Head.Inner(0).Span.ToString)
#End Region
#Region "Check Format.Body"
    Dim Body = res.Inner(1)
    Assert.AreEqual("(  1:  2)", Body.Span.ToString)
    Assert.AreEqual(1, Body.Inner.Count)
    Dim BodyText = Body.Inner(0)
    Assert.IsInstanceOfType(BodyText, GetType(FormatString.Text))
    Assert.AreEqual("(  1:  2)", BodyText.Span.ToString)
    Assert.AreEqual(1, BodyText.Inner.Count)
    Assert.IsInstanceOfType(BodyText.Inner(0), GetType(FormatString.Common.Brace.Esc.Opening))

#End Region
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _03_()
    Dim Text = ":{"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Format.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Format))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
    Assert.AreEqual(2, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(FormatString.ArgHole.Format.Head))
    Assert.IsInstanceOfType(res.Inner(1), GetType(FormatString.ArgHole.Format.Body))
#Region "Check Format.Head"
    Dim Head = res.Inner(0)
    Assert.AreEqual("(  0:  1)", Head.Span.ToString)
    Assert.AreEqual(1, Head.Inner.Count)
    Assert.IsInstanceOfType(Head.Inner(0), GetType(FormatString.ArgHole.Format.Colon))
    Assert.AreEqual("(  0:  1)", Head.Inner(0).Span.ToString)
#End Region
#Region "Check Format.Body"
    Dim Body = res.Inner(1)
    Assert.AreEqual("(  1:  1)", Body.Span.ToString)
    Assert.AreEqual(1, Body.Inner.Count)
    Dim BodyText = Body.Inner(0)
    Assert.IsInstanceOfType(BodyText, GetType(FormatString.Text))
    Assert.AreEqual("(  1:  1)", BodyText.Span.ToString)
    Assert.AreEqual(1, BodyText.Inner.Count)
    Assert.IsInstanceOfType(BodyText.Inner(0), GetType(ParseError))
    Dim pe = DirectCast(BodyText.Inner(0), ParseError)
    Assert.AreEqual(ParseError.Reason.Invalid, pe.Why)
    Assert.AreEqual("(  1:  1)", pe.Span.ToString)
    Assert.AreEqual(1, pe.Inner.Count)
    Assert.IsInstanceOfType(pe.Inner(0), GetType(FormatString.Common.Brace.Opening))


#End Region

  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _04_()
    Dim Text = ":{}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Format.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Format))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
    Assert.AreEqual(2, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(FormatString.ArgHole.Format.Head))
    Assert.IsInstanceOfType(res.Inner(1), GetType(FormatString.ArgHole.Format.Body))
#Region "Check Format.Head"
    Dim Head = res.Inner(0)
    Assert.AreEqual("(  0:  1)", Head.Span.ToString)
    Assert.AreEqual(1, Head.Inner.Count)
    Assert.IsInstanceOfType(Head.Inner(0), GetType(FormatString.ArgHole.Format.Colon))
    Assert.AreEqual("(  0:  1)", Head.Inner(0).Span.ToString)
#End Region
#Region "Check Format.Body"
    Dim Body = res.Inner(1)
    Assert.AreEqual("(  1:  1)", Body.Span.ToString)
    Assert.AreEqual(1, Body.Inner.Count)
    Dim BodyText = Body.Inner(0)
    Assert.IsInstanceOfType(BodyText, GetType(FormatString.Text))
    Assert.AreEqual("(  1:  1)", BodyText.Span.ToString)
    Assert.AreEqual(1, BodyText.Inner.Count)
    Assert.IsInstanceOfType(BodyText.Inner(0), GetType(ParseError))
    Dim pe As ParseError = DirectCast(BodyText.Inner(0), ParseError)
    Assert.AreEqual(ParseError.Reason.Invalid, pe.Why)
    Assert.AreEqual("(  1:  1)", pe.Span.ToString)
    Assert.AreEqual(1, pe.Inner.Count)
    Assert.IsInstanceOfType(pe.Inner(0), GetType(FormatString.Common.Brace.Opening))
#End Region

  End Sub
End Class