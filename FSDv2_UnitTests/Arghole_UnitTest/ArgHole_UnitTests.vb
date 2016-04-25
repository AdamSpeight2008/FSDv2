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
    Dim Input = "{"
    Dim TheSource = Source.Create(Input, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim ParseResult = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(ParseResult)
    Assert.IsNotInstanceOfType(ParseResult, GetType(ParseError))
    Assert.IsInstanceOfType(ParseResult, GetType(ArgHole))
    Dim Actual = ParseResult.AsString()
    Dim Expected =
"(  0:  1)  ArgHole
  [ 0]  (  0:  1)  Brace_Opening
  [ 1]  (  1:  0)  ParseError.EoT
"
    Assert.AreEqual(Expected, Actual)

  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _02_()
    Dim Text = "{}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim ParseResult = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(ParseResult)
    Assert.IsNotInstanceOfType(ParseResult, GetType(ParseError))
    Assert.IsInstanceOfType(ParseResult, GetType(ArgHole))
    'Assert.AreEqual("(  0:  2)", ParseResult.Span.ToString)
    'Assert.AreEqual(2, ParseResult.Inner.Count)
    'Assert.IsInstanceOfType(ParseResult.Inner(0), GetType(Common.Brace.Opening))
    'Assert.IsInstanceOfType(ParseResult.Inner(1), GetType(Common.Brace.Closing))
    Dim Actual = ParseResult.AsString()
    Dim Expected =
"(  0:  2)  ArgHole
  [ 0]  (  0:  1)  Brace_Opening
  [ 1]  (  1:  0)  ParseError.Partial
    [ 0]  (  1:  1)  Brace_Closing
  [ 2]  (  1:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Actual)

  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _03_()
    Dim Text = "{0}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim ParseResult = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(ParseResult)
    Assert.IsNotInstanceOfType(ParseResult, GetType(ParseError))
    Assert.IsInstanceOfType(ParseResult, GetType(ArgHole))
    'Assert.AreEqual("(  0:  3)", res.Span.ToString)
    'Assert.AreEqual(3, res.Inner.Count)
    'Assert.IsInstanceOfType(res.Inner(0), GetType(Common.Brace.Opening))
    'Assert.IsInstanceOfType(res.Inner(1), GetType(ArgHole.Index))
    'Assert.IsInstanceOfType(res.Inner(2), GetType(Common.Brace.Closing))
    Dim Actual = ParseResult.AsString()
    Dim Expected =
"(  0:  3)  ArgHole
  [ 0]  (  0:  1)  Brace_Opening
  [ 1]  (  1:  1)  ArgHole_Index
    [ 0]  (  1:  1)  Digits
      [ 0]  (  1:  1)  Digit
    [ 1]  (  2:  0)  Whitespaces
  [ 2]  (  2:  0)  ParseError.Partial
    [ 0]  (  2:  1)  Brace_Closing
  [ 3]  (  2:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Actual)

  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _04_()
    Dim Text = "{0 }"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim ParseResult = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(ParseResult)
    Assert.IsNotInstanceOfType(ParseResult, GetType(ParseError))
    Assert.IsInstanceOfType(ParseResult, GetType(ArgHole))
    'Assert.AreEqual("(  0:  4)", ParseResult.Span.ToString)
    'Assert.AreEqual(3, ParseResult.Inner.Count)
    'Assert.IsInstanceOfType(ParseResult.Inner(0), GetType(Common.Brace.Opening))
    'Assert.IsInstanceOfType(ParseResult.Inner(1), GetType(ArgHole.Index))
    'Assert.IsInstanceOfType(ParseResult.Inner(2), GetType(Common.Brace.Closing))
    Dim Actual = ParseResult.AsString()
    Dim Expected =
"(  0:  4)  ArgHole
  [ 0]  (  0:  1)  Brace_Opening
  [ 1]  (  1:  2)  ArgHole_Index
    [ 0]  (  1:  1)  Digits
      [ 0]  (  1:  1)  Digit
    [ 1]  (  2:  1)  Whitespaces
      [ 0]  (  2:  1)  Whitespace
  [ 2]  (  3:  0)  ParseError.Partial
    [ 0]  (  3:  1)  Brace_Closing
  [ 3]  (  3:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Actual)

  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _05_()
    Dim Text = "{0 ,}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim ParseResult = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(ParseResult)
    Assert.IsNotInstanceOfType(ParseResult, GetType(ParseError))
    Assert.IsInstanceOfType(ParseResult, GetType(ArgHole))
    Dim Actual = ParseResult.AsString()
    Dim Expected =
"(  0:  5)  ArgHole
  [ 0]  (  0:  1)  Brace_Opening
  [ 1]  (  1:  2)  ArgHole_Index
    [ 0]  (  1:  1)  Digits
      [ 0]  (  1:  1)  Digit
    [ 1]  (  2:  1)  Whitespaces
      [ 0]  (  2:  1)  Whitespace
  [ 2]  (  3:  1)  ArgHole_Align
    [ 0]  (  3:  1)  ArgHole_Align_Head
      [ 0]  (  3:  1)  Comma
      [ 1]  (  4:  0)  Whitespaces
  [ 3]  (  4:  0)  ParseError.Partial
    [ 0]  (  4:  1)  Brace_Closing
  [ 4]  (  4:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Actual)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _06_()
    Dim Text = "{0 , :}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim ParseResult = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(ParseResult)
    Assert.IsNotInstanceOfType(ParseResult, GetType(ParseError))
    Assert.IsInstanceOfType(ParseResult, GetType(ArgHole))
    'Assert.AreEqual("(  0:  7)", ParseResult.Span.ToString)
    Dim Actual = ParseResult.AsString()
    Dim Expected =
"(  0:  7)  ArgHole
  [ 0]  (  0:  1)  Brace_Opening
  [ 1]  (  1:  2)  ArgHole_Index
    [ 0]  (  1:  1)  Digits
      [ 0]  (  1:  1)  Digit
    [ 1]  (  2:  1)  Whitespaces
      [ 0]  (  2:  1)  Whitespace
  [ 2]  (  3:  2)  ArgHole_Align
    [ 0]  (  3:  2)  ArgHole_Align_Head
      [ 0]  (  3:  1)  Comma
      [ 1]  (  4:  1)  Whitespaces
        [ 0]  (  4:  1)  Whitespace
  [ 3]  (  5:  1)  ArgHole_Format
    [ 0]  (  5:  1)  ArgHole_Format_Head
      [ 0]  (  5:  1)  Colon
    [ 1]  (  6:  0)  ArgHole_Format_Body
  [ 4]  (  6:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Actual)

  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _07_()
    Dim Text = "{0 , 1}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim ParseResult = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(ParseResult)
    Assert.IsNotInstanceOfType(ParseResult, GetType(ParseError))
    Assert.IsInstanceOfType(ParseResult, GetType(ArgHole))
    Dim Actual = ParseResult.AsString()
    Dim Expected =
"(  0:  7)  ArgHole
  [ 0]  (  0:  1)  Brace_Opening
  [ 1]  (  1:  2)  ArgHole_Index
    [ 0]  (  1:  1)  Digits
      [ 0]  (  1:  1)  Digit
    [ 1]  (  2:  1)  Whitespaces
      [ 0]  (  2:  1)  Whitespace
  [ 2]  (  3:  3)  ArgHole_Align
    [ 0]  (  3:  2)  ArgHole_Align_Head
      [ 0]  (  3:  1)  Comma
      [ 1]  (  4:  1)  Whitespaces
        [ 0]  (  4:  1)  Whitespace
    [ 1]  (  5:  1)  ArgHole_Align_Body
      [ 0]  (  5:  1)  Digits
        [ 0]  (  5:  1)  Digit
      [ 1]  (  6:  0)  Whitespaces
  [ 3]  (  6:  0)  ParseError.Partial
    [ 0]  (  6:  1)  Brace_Closing
  [ 4]  (  6:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Actual)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub _08_()
    Dim Text = "{0 , -1}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim ParseResult = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(ParseResult)
    Assert.IsNotInstanceOfType(ParseResult, GetType(ParseError))
    Assert.IsInstanceOfType(ParseResult, GetType(ArgHole))
    Dim Actual = ParseResult.AsString()
    Dim Expected =
"(  0:  8)  ArgHole
  [ 0]  (  0:  1)  Brace_Opening
  [ 1]  (  1:  2)  ArgHole_Index
    [ 0]  (  1:  1)  Digits
      [ 0]  (  1:  1)  Digit
    [ 1]  (  2:  1)  Whitespaces
      [ 0]  (  2:  1)  Whitespace
  [ 2]  (  3:  4)  ArgHole_Align
    [ 0]  (  3:  2)  ArgHole_Align_Head
      [ 0]  (  3:  1)  Comma
      [ 1]  (  4:  1)  Whitespaces
        [ 0]  (  4:  1)  Whitespace
    [ 1]  (  5:  2)  ArgHole_Align_Body
      [ 0]  (  5:  1)  MinusSign
      [ 1]  (  6:  1)  Digits
        [ 0]  (  6:  1)  Digit
      [ 2]  (  7:  0)  Whitespaces
  [ 3]  (  7:  0)  ParseError.Partial
    [ 0]  (  7:  1)  Brace_Closing
  [ 4]  (  7:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Actual)
  End Sub
  <TestMethod, TestCategory(Cat)>
  Public Sub _09_()
    Dim Text = "{0 :{{}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim ParseResult = FormatString.ArgHole.TryParse(FirstPos)
    Assert.IsNotNull(ParseResult)
    Assert.IsNotInstanceOfType(ParseResult, GetType(ParseError))
    Assert.IsInstanceOfType(ParseResult, GetType(ArgHole))
    Dim Actual = ParseResult.AsString()
    Dim Expected =
"(  0:  7)  ArgHole
  [ 0]  (  0:  1)  Brace_Opening
  [ 1]  (  1:  2)  ArgHole_Index
    [ 0]  (  1:  1)  Digits
      [ 0]  (  1:  1)  Digit
    [ 1]  (  2:  1)  Whitespaces
      [ 0]  (  2:  1)  Whitespace
  [ 2]  (  3:  0)  ParseError.Partial
    [ 0]  (  3:  1)  Colon
  [ 3]  (  3:  3)  ArgHole_Format
    [ 0]  (  3:  1)  ArgHole_Format_Head
      [ 0]  (  3:  1)  Colon
    [ 1]  (  4:  2)  ArgHole_Format_Body
      [ 0]  (  4:  2)  Text
        [ 0]  (  4:  2)  Esc_Brace_Opening
          [ 0]  (  4:  1)  Brace_Opening
          [ 1]  (  5:  1)  Brace_Opening
  [ 4]  (  6:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Actual)
  End Sub
End Class