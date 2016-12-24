imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2
Imports System.Runtime.CompilerServices

Public Module Exts

  <Extension>
  Public Function AsString(Ix As FSDv2_Analyser.Analyser.Issues) As String
    If Ix Is Nothing Then Return "{Nothing}"
    Dim sb As New Text.StringBuilder
    For Each i In Ix.Issues
      sb.AppendLine($"{i.Span.ToString} {i.Kind.ToString}")
    Next
    Return sb.ToString
  End Function


End Module

<TestClass()>
Public Class FSDv2_UnitTests

  Const Cat0 = "Analyser (CS_Standard)"

  <TestMethod, TestCategory(Cat0)>
  Public Sub _00_EmptyString()
    Dim TheText = ""
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat )
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    Dim Expected = ""
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _01_JustText()
    Dim TheText = "abc"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat )
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    Dim Expected = ""
    Assert.AreEqual(Expected, Text)

  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _02_Text_Brace_Closing()
    Dim TheText = "}"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat )
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    Dim Expected = "(  0:  1) Invalid
"
    Assert.AreEqual(Expected, Text)

  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _03_Text_Brace_Opening()
    Dim TheText = "{"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat )
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    ' Should ultimatley: Invalid Missing Closing Brace.
    Dim Expected = "(  1:  0) Arg_Index_Missing
(  1:  0) Missing_Closing_Brace
"
    Assert.AreEqual(Expected, Text)

  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _04_EscapedOpening()
    Dim TheText = "{{"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat )
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    Dim Expected = ""
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _05_EscapedClosing()
    Dim TheText = "}}"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat )
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    Dim Expected = ""
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _06_EmptyArgHole()
    Dim TheText = "{}"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat )
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    ' Should ultimatley: Invalid Missing Arg Index.
    Dim Expected = "(  1:  0) Arg_Index_Missing
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _07_EmptyArgHoles()
    Dim TheText = "{}{}"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat )
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    ' Should ultimatley: Invalid Missing Arg Index.
    Dim Expected = "(  1:  0) Arg_Index_Missing
(  3:  0) Arg_Index_Missing
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _08_()
    '              0123456
    Dim TheText = " {} {} "
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat )
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    ' Should ultimatley: Invalid Missing Arg Index.
    Dim Expected = "(  2:  0) Arg_Index_Missing
(  5:  0) Arg_Index_Missing
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _09_()
    '              01234567890
    Dim TheText = "}} {} {} {{"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat )
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    ' Should ultimatley: Invalid Missing Arg Index.
    Dim Expected = "(  4:  0) Arg_Index_Missing
(  7:  0) Arg_Index_Missing
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _10_()
    '              0123456
    Dim TheText = "{x}"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    ' Why isn't the unexpected characters being propergated ?
    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    ' Should ultimatley:
    '   Unexpected_Characters
    '   Invalid Missing Arg Index. Would the missing arg.index at index 1 or 2?
    Dim Expected = "(  1:  1) Unexpected_Characters
(  2:  0) Arg_Index_Missing
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _11_()
    '              0123456
    Dim TheText = "{ }"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)
    ' Why isn't the unexpected characters being propergated ?
    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    ' Should ultimatley:
    '   Unexpected_Characters
    '   Invalid Missing Arg Index. Would the missing arg.index at index 1 or 2?
    Dim Expected = "(  1:  1) Unexpected_Characters
(  2:  0) Arg_Index_Missing
"
    Assert.AreEqual(Expected, Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _12_()
    '              0123456
    Dim TheText = "{ 12}"
    Dim TheSource = Source.Create(TheText, Source.SourceKind.CS_Standard, Source.StringKind.StringFormat)
    Dim ParseResult = FormatString.TryParse(TheSource.First.Value)

    Dim Analyser As New FSDv2_Analyser.Analyser()
    Dim Parameters As New FSDv2_Analyser.Analyser.Parameters()
    Dim Result = Analyser.Analyse(ParseResult, Parameters)
    Dim Text = Result.Result.Issues.AsString
    Dim Expected = "(  1:  1) Unexpected_Token" & vbCrLf &
                   "(  2:  2) Arg_Index_OutOfRange" & vbCrLf
    Assert.AreEqual(Expected, Text)
  End Sub

End Class