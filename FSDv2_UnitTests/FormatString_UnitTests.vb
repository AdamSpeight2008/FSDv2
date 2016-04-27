Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2
Imports System.Runtime.CompilerServices

Public Module Exts
  <Extension>
  Public Function AsString(Tk As Token) As String
    If Tk Is Nothing Then Return "{Nothing}"
    Dim sb As New Text.StringBuilder
    sb.AppendLine($"{Tk.ToString}")
    _AsString(Tk, sb, 1)
    Return sb.ToString
  End Function

  Private Sub _AsString(Tk As Token, sb As StringBuilder, level As Integer)
    For i = 0 To Tk.InnerTokens.Count - 1
      sb.Append(Space(level * 2))
      sb.AppendLine($"[{i,2}]  {Tk(i).ToString}")
      _AsString(Tk.InnerTokens(i), sb, level + 1)
    Next
  End Sub

End Module

<TestClass()>
Public Class FSDv2_UnitTests

  Const Cat0 = "Tokens.FormatString (CS_Standard)"

  <TestMethod, TestCategory(Cat0)>
  Public Sub _00_EmptyString()
    Dim TheText = ""
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"( -1:  0)  ParseError.NullParse
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _01_JustText()
    Dim TheText = "abc"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"(  0:  3)  FormatString
  [ 0]  (  0:  3)  Text
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _02_Text_Brace_Closing()
    Dim TheText = "}"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"(  0:  1)  FormatString
  [ 0]  (  0:  1)  ParseError.Invalid
    [ 0]  (  0:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _03_Text_Brace_Opening()
    Dim TheText = "{"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"(  0:  1)  FormatString
  [ 0]  (  0:  1)  ArgHole
    [ 0]  (  0:  1)  Brace_Opening
    [ 1]  (  1:  0)  ParseError.EoT
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _04_EscapedOpening()
    Dim TheText = "{{"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"(  0:  2)  FormatString
  [ 0]  (  0:  2)  Esc_Brace_Opening
    [ 0]  (  0:  1)  Brace_Opening
    [ 1]  (  1:  1)  Brace_Opening
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _05_EscapedClosing()
    Dim TheText = "}}"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"(  0:  2)  FormatString
  [ 0]  (  0:  2)  Esc_Brace_Closing
    [ 0]  (  0:  1)  Brace_Closing
    [ 1]  (  1:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _06_EmptyArgHole()
    Dim TheText = "{}"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"(  0:  2)  FormatString
  [ 0]  (  0:  2)  ArgHole
    [ 0]  (  0:  1)  Brace_Opening
    [ 1]  (  1:  0)  ParseError.Partial
      [ 0]  (  1:  1)  Brace_Closing
    [ 2]  (  1:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _07_EmptyArgHoles()
    Dim TheText = "{}{}"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"(  0:  4)  FormatString
  [ 0]  (  0:  2)  ArgHole
    [ 0]  (  0:  1)  Brace_Opening
    [ 1]  (  1:  0)  ParseError.Partial
      [ 0]  (  1:  1)  Brace_Closing
    [ 2]  (  1:  1)  Brace_Closing
  [ 1]  (  2:  2)  ArgHole
    [ 0]  (  2:  1)  Brace_Opening
    [ 1]  (  3:  0)  ParseError.Partial
      [ 0]  (  3:  1)  Brace_Closing
    [ 2]  (  3:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Text)
  End Sub
  <TestMethod, TestCategory(Cat0)>
  Public Sub _08_()
    '              0123456
    Dim TheText = " {} {} "
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"(  0:  7)  FormatString
  [ 0]  (  0:  1)  Text
  [ 1]  (  1:  2)  ArgHole
    [ 0]  (  1:  1)  Brace_Opening
    [ 1]  (  2:  0)  ParseError.Partial
      [ 0]  (  2:  1)  Brace_Closing
    [ 2]  (  2:  1)  Brace_Closing
  [ 2]  (  3:  1)  Text
  [ 3]  (  4:  2)  ArgHole
    [ 0]  (  4:  1)  Brace_Opening
    [ 1]  (  5:  0)  ParseError.Partial
      [ 0]  (  5:  1)  Brace_Closing
    [ 2]  (  5:  1)  Brace_Closing
  [ 4]  (  6:  1)  Text
"
    Assert.AreEqual(Expected, Text)
  End Sub
  <TestMethod, TestCategory(Cat0)>
  Public Sub _09_()
    '              0123456
    Dim TheText = "}} {} {} {{"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"(  0: 11)  FormatString
  [ 0]  (  0:  2)  Esc_Brace_Closing
    [ 0]  (  0:  1)  Brace_Closing
    [ 1]  (  1:  1)  Brace_Closing
  [ 1]  (  2:  1)  Text
  [ 2]  (  3:  2)  ArgHole
    [ 0]  (  3:  1)  Brace_Opening
    [ 1]  (  4:  0)  ParseError.Partial
      [ 0]  (  4:  1)  Brace_Closing
    [ 2]  (  4:  1)  Brace_Closing
  [ 3]  (  5:  1)  Text
  [ 4]  (  6:  2)  ArgHole
    [ 0]  (  6:  1)  Brace_Opening
    [ 1]  (  7:  0)  ParseError.Partial
      [ 0]  (  7:  1)  Brace_Closing
    [ 2]  (  7:  1)  Brace_Closing
  [ 5]  (  8:  1)  Text
  [ 6]  (  9:  2)  Esc_Brace_Opening
    [ 0]  (  9:  1)  Brace_Opening
    [ 1]  ( 10:  1)  Brace_Opening
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _10_()
    '              0123456
    Dim TheText = "{x}"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"(  0:  3)  FormatString
  [ 0]  (  0:  3)  ArgHole
    [ 0]  (  0:  1)  Brace_Opening
    [ 1]  (  1:  1)  ParseError.Partial
      [ 0]  (  2:  1)  Brace_Closing
    [ 2]  (  2:  1)  Brace_Closing
"
    Assert.AreEqual(Expected, Text)
  End Sub
End Class