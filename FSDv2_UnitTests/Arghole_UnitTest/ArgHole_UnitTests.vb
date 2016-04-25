Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2
Imports FSDv2.FormatString

<TestClass>
Public Class ArgHole_UnitTests

  '
  '  ArgFormat ::= Align_Head Align_Body
  ' Format_Head ::= Comma Whitespaces? 
  ' Format_Body ::= MinusSign? Digits Whitespaces?
  '
  Const Cat = "Tokens.Arghole"

  <TestMethod, TestCategory(Cat)>
  Public Sub _00_()
    Dim Text = ""
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.EoT, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _01_()
    Dim Text = "{"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(ArgHole))
    Assert.AreEqual("(  0:  1)", res.Span.ToString)
    Assert.AreEqual(1, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(Common.Brace.Opening))

  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _02_()
    Dim Text = "{}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(ArgHole))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
    Assert.AreEqual(2, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(Common.Brace.Opening))
    Assert.IsInstanceOfType(res.Inner(1), GetType(Common.Brace.Closing))

  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _03_()
    Dim Text = "{0}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(ArgHole))
    Assert.AreEqual("(  0:  3)", res.Span.ToString)
    Assert.AreEqual(3, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(Common.Brace.Opening))
    Assert.IsInstanceOfType(res.Inner(1), GetType(ArgHole.Index))
    Assert.IsInstanceOfType(res.Inner(2), GetType(Common.Brace.Closing))

  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _04_()
    Dim Text = "{0 }"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(ArgHole))
    Assert.AreEqual("(  0:  4)", res.Span.ToString)
    Assert.AreEqual(3, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(Common.Brace.Opening))
    Assert.IsInstanceOfType(res.Inner(1), GetType(ArgHole.Index))
    Assert.IsInstanceOfType(res.Inner(2), GetType(Common.Brace.Closing))

  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _05_()
    Dim Text = "{0 ,}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(ArgHole))
    Assert.AreEqual("(  0:  5)", res.Span.ToString)
    Assert.AreEqual(4, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(Common.Brace.Opening))
    Assert.IsInstanceOfType(res.Inner(1), GetType(ArgHole.Index))
    Assert.IsInstanceOfType(res.Inner(2), GetType(ArgHole.Align))
    Assert.IsInstanceOfType(res.Inner(3), GetType(Common.Brace.Closing))

  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _06_()
    Dim Text = "{0 , :}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(ArgHole))
    Assert.AreEqual("(  0:  7)", res.Span.ToString)
    Assert.AreEqual(5, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(Common.Brace.Opening))
    Assert.IsInstanceOfType(res.Inner(1), GetType(ArgHole.Index))
    Assert.IsInstanceOfType(res.Inner(2), GetType(ArgHole.Align))
    Assert.IsInstanceOfType(res.Inner(3), GetType(ArgHole.Format))
    Assert.IsInstanceOfType(res.Inner(4), GetType(Common.Brace.Closing))

  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _07_()
    Dim Text = "{0 , 1}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(ArgHole))
    Assert.AreEqual("(  0:  7)", res.Span.ToString)
    Assert.AreEqual(4, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(Common.Brace.Opening))
    Assert.IsInstanceOfType(res.Inner(1), GetType(ArgHole.Index))
    Assert.IsInstanceOfType(res.Inner(2), GetType(ArgHole.Align))
    Assert.IsInstanceOfType(res.Inner(3), GetType(Common.Brace.Closing))

  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _08_()
    Dim Text = "{0 , -1}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(ArgHole))
    Assert.AreEqual("(  0:  8)", res.Span.ToString)
    Assert.AreEqual(4, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(Common.Brace.Opening))
    Assert.IsInstanceOfType(res.Inner(1), GetType(ArgHole.Index))
    Assert.IsInstanceOfType(res.Inner(2), GetType(ArgHole.Align))
    Assert.IsInstanceOfType(res.Inner(3), GetType(Common.Brace.Closing))

  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _09_()
    Dim Text = "{0 :{{}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(ArgHole))
    Assert.AreEqual("(  0:  7)", res.Span.ToString)
    Assert.AreEqual(4, res.Inner.Count)
    Assert.IsInstanceOfType(res.Inner(0), GetType(Common.Brace.Opening))
    Assert.IsInstanceOfType(res.Inner(1), GetType(ArgHole.Index))
    Assert.IsInstanceOfType(res.Inner(2), GetType(ArgHole.Format))
    Assert.IsInstanceOfType(res.Inner(3), GetType(Common.Brace.Closing))

  End Sub
End Class