Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass> Public Class Brace

  <TestMethod, TestCategory("Tokens.Common.Brace")>
  Public Sub _00_()
    Dim Text = ""
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Brace.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.EoT, DirectCast(res, ParseError).Why)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.Brace")>
  Public Sub _01_()
    Dim Text = "{"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Brace.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Brace.Opening))
    Assert.AreEqual(TokenKind.Brace_Opening, res.Kind)
    Assert.AreEqual(0, res.Span.Start.Index)
    Assert.AreEqual(1, res.Span.Size)
    Assert.AreEqual(0, res.InnerTokens.Count)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.Brace")>
  Public Sub _02_()
    Dim Text = "}"
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Brace.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Brace.Closing))
    Assert.AreEqual(TokenKind.Brace_Closing, res.Kind)
    Assert.AreEqual(0, res.Span.Start.Index)
    Assert.AreEqual(1, res.Span.Size)
    Assert.AreEqual(0, res.InnerTokens.Count)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.Brace")>
  Public Sub _03_()
    Dim Text = "{{"
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Brace.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Brace.Esc.Opening))
    Assert.AreEqual(TokenKind.Esc_Brace_Opening, res.Kind)
    Assert.AreEqual(0, res.Span.Start.Index)
    Assert.AreEqual(2, res.Span.Size)
    Assert.AreEqual(2, res.InnerTokens.Count)
    Assert.AreEqual(TokenKind.Brace_Opening, res(0).Kind)
    Assert.AreEqual(0, res(0).Span.Start.Index)
    Assert.AreEqual(1, res(0).Span.Size)
    Assert.AreEqual(TokenKind.Brace_Opening, res(1).Kind)
    Assert.AreEqual(1, res(1).Span.Start.Index)
    Assert.AreEqual(1, res(1).Span.Size)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.Brace")>
  Public Sub _04_()
    Dim Text = "}}"
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Brace.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Brace.Esc.Closing))
    Assert.AreEqual(TokenKind.Esc_Brace_Closing, res.Kind)
    Assert.AreEqual(0, res.Span.Start.Index)
    Assert.AreEqual(2, res.Span.Size)
    Assert.AreEqual(2, res.InnerTokens.Count)
    Assert.AreEqual(TokenKind.Brace_Closing, res(0).Kind)
    Assert.AreEqual(0, res(0).Span.Start.Index)
    Assert.AreEqual(1, res(0).Span.Size)
    Assert.AreEqual(TokenKind.Brace_Closing, res(1).Kind)
    Assert.AreEqual(1, res(1).Span.Start.Index)
    Assert.AreEqual(1, res(1).Span.Size)
  End Sub

  <TestMethod, TestCategory("Tokens.Common.Brace")>
  Public Sub _05_()
    Dim Text = "{ "
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Brace.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Brace.Opening))
    Assert.AreEqual(TokenKind.Brace_Opening, res.Kind)
    Assert.AreEqual(0, res.Span.Start.Index)
    Assert.AreEqual(1, res.Span.Size)
    Assert.AreEqual(0, res.InnerTokens.Count)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.Brace")>
  Public Sub _06_()
    Dim Text = "} "
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Brace.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Brace.Closing))
    Assert.AreEqual(TokenKind.Brace_Closing, res.Kind)
    Assert.AreEqual(0, res.Span.Start.Index)
    Assert.AreEqual(1, res.Span.Size)
    Assert.AreEqual(0, res.InnerTokens.Count)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.Brace")>
  Public Sub _07_()
    Dim Text = "{{ "
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Brace.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Brace.Esc.Opening))
    Assert.AreEqual(TokenKind.Esc_Brace_Opening, res.Kind)
    Assert.AreEqual(0, res.Span.Start.Index)
    Assert.AreEqual(2, res.Span.Size)
    Assert.AreEqual(2, res.InnerTokens.Count)
    Assert.AreEqual(TokenKind.Brace_Opening, res(0).Kind)
    Assert.AreEqual(0, res(0).Span.Start.Index)
    Assert.AreEqual(1, res(0).Span.Size)
    Assert.AreEqual(TokenKind.Brace_Opening, res(1).Kind)
    Assert.AreEqual(1, res(1).Span.Start.Index)
    Assert.AreEqual(1, res(1).Span.Size)
  End Sub
  <TestMethod, TestCategory("Tokens.Common.Brace")>
  Public Sub _08_()
    Dim Text = "}} "
        Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
        Dim FirstPos = TheSource.First
    Dim res = FormatString.Common.Brace.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.Common.Brace.Esc.Closing))
    Assert.AreEqual(TokenKind.Esc_Brace_Closing, res.Kind)
    Assert.AreEqual(0, res.Span.Start.Index)
    Assert.AreEqual(2, res.Span.Size)
    Assert.AreEqual(2, res.InnerTokens.Count)
    Assert.AreEqual(TokenKind.Brace_Closing, res(0).Kind)
    Assert.AreEqual(0, res(0).Span.Start.Index)
    Assert.AreEqual(1, res(0).Span.Size)
    Assert.AreEqual(TokenKind.Brace_Closing, res(1).Kind)
    Assert.AreEqual(1, res(1).Span.Start.Index)
    Assert.AreEqual(1, res(1).Span.Size)
  End Sub
End Class