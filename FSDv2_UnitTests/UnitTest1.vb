Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2
Imports System.Runtime.CompilerServices

Public Module Exts
  <Extension>
  Public Function AsString(Tk As Token) As String
    If Tk Is Nothing Then Return "{Nothing}"
    Dim sb As New Text.StringBuilder
    _AsString(Tk, sb, 0)
    Return sb.ToString
  End Function

  Private Sub _AsString(Tk As Token, sb As StringBuilder, level As Integer)
    sb.AppendLine($"{Space(level * 2)}{Tk.Span.ToString} {Tk.GetType.Name}")
    For i = 0 To Tk.Inner.Count - 1
      sb.Append(Space(level * 2))
      sb.Append($"[{i,2}]")
      _AsString(Tk.Inner(i), sb, level + 1)
    Next
  End Sub

End Module

<TestClass()>
Public Class FSDv2_UnitTests

  <TestMethod>
  Public Sub _00_EmptyString()
    Dim TheText = ""
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected = "{Nothing}"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod>
  Public Sub _01_JustText()
    Dim TheText = "abc"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"(  0:  3) FormatString
[ 0]  (  0:  3) Text
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod>
  Public Sub _02_Text_Brace_Closing()
    Dim TheText = "}"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Text = ParseResult.AsString()
    Dim Expected =
"(  0:  1) FormatString
[ 0]  (  0:  1) ParseError
"
    Assert.AreEqual(Expected, Text)
  End Sub
End Class