﻿Imports System.Text
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
  Public Sub Parser_ArgFormat__00__EmptyString()
    Dim Text = ""
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Format.TryParse(FirstPos, False)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(ParseError))
    Assert.AreEqual(ParseError.Reason.EoT, DirectCast(res, ParseError).Why)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub Parser_ArgFormat__01__HeadOnly()
    Dim Text = ":"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Format.TryParse(FirstPos, False)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Format))
    Dim Actual = res.AsString
    Dim Expected = "(  0:  1)  ArgHole_Format
  [ 0]  (  0:  1)  ArgHole_Format_Head
    [ 0]  (  0:  1)  Colon
"
    Assert.AreEqual(Expected, Actual)
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub Parser_ArgFormat__02__Head_Body()
    Dim Text = ":{{"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Format.TryParse(FirstPos, False)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Format))
    Assert.AreEqual("(  0:  3)", res.Span.ToString)
    Assert.AreEqual(2, res.InnerTokens.Count)
    Assert.IsInstanceOfType(res(0), GetType(FormatString.ArgHole.Format.Head))
    Assert.IsInstanceOfType(res(1), GetType(FormatString.ArgHole.Format.Body))
#Region "Check Format.Head"
    Dim Head = res(0)
    Assert.AreEqual("(  0:  1)", Head.Span.ToString)
    Assert.AreEqual(1, Head.InnerTokens.Count)
    Assert.IsInstanceOfType(Head(0), GetType(FormatString.ArgHole.Format.Colon))
    Assert.AreEqual("(  0:  1)", Head(0).Span.ToString)
    Dim Actual = Head.AsString
    Dim Expected =
"(  0:  1)  ArgHole_Format_Head
  [ 0]  (  0:  1)  Colon
"
    Assert.AreEqual(Expected, Actual)
#End Region
#Region "Check Format.Body"
    Dim Body = res(1)
    Assert.AreEqual("(  1:  2)", Body.Span.ToString)
    Assert.AreEqual(1, Body.InnerTokens.Count)
    Dim BodyText = Body(0)
    Assert.IsInstanceOfType(BodyText, GetType(FormatString.Text))
    Assert.AreEqual("(  1:  2)", BodyText.Span.ToString)
    Assert.AreEqual(1, BodyText.InnerTokens.Count)
    Assert.IsInstanceOfType(BodyText(0), GetType(FormatString.Common.Brace.Esc.Opening))
    Actual = Body.AsString
    Expected =
"(  1:  2)  ArgHole_Format_Body
  [ 0]  (  1:  2)  Text
    [ 0]  (  1:  2)  Esc_Brace_Opening
      [ 0]  (  1:  1)  Brace_Opening
      [ 1]  (  2:  1)  Brace_Opening
"
    Assert.AreEqual(Expected, Actual)
#End Region
  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub Parser_ArgFormat__03__Head_Body_OpeningBrace()
    Dim Text = ":{"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Format.TryParse(FirstPos, False)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Format))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
    Assert.AreEqual(2, res.InnerTokens.Count)
    Assert.IsInstanceOfType(res(0), GetType(FormatString.ArgHole.Format.Head))
    Assert.IsInstanceOfType(res(1), GetType(FormatString.ArgHole.Format.Body))
#Region "Check Format.Head"
    Dim Head = res(0)
    Assert.AreEqual("(  0:  1)", Head.Span.ToString)
    Assert.AreEqual(1, Head.InnerTokens.Count)
    Assert.IsInstanceOfType(Head(0), GetType(FormatString.ArgHole.Format.Colon))
    Assert.AreEqual("(  0:  1)", Head(0).Span.ToString)
#End Region
#Region "Check Format.Body"
    Dim Body = res(1)
    Assert.AreEqual("(  1:  1)", Body.Span.ToString)
    Assert.AreEqual(1, Body.InnerTokens.Count)
    Dim BodyText = Body(0)
    Assert.IsInstanceOfType(BodyText, GetType(FSDv2.ParseError.Invalid))
    'Assert.IsInstanceOfType(BodyText, GetType(FormatString.Text))
    Assert.AreEqual("(  1:  1)", BodyText.Span.ToString)
    Assert.AreEqual(1, BodyText.InnerTokens.Count)
    ' Assert.IsInstanceOfType(BodyText(0), GetType(ParseError))
    Dim pe = DirectCast(BodyText, ParseError)
    Assert.AreEqual(ParseError.Reason.Invalid, pe.Why)
    Assert.AreEqual("(  1:  1)", pe.Span.ToString)
    Assert.AreEqual(1, pe.InnerTokens.Count)
    Assert.IsInstanceOfType(pe(0), GetType(FormatString.Common.Brace.Opening))


#End Region

  End Sub

  <TestMethod, TestCategory(Cat)>
  Public Sub Parser_ArgFormat__04__Head_Body_OpeningClosingBrace()
    Dim Text = ":{}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard, Source.StringKind.StringFormat)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Format.TryParse(FirstPos, False)
    Assert.IsNotNull(res)
    Assert.IsNotInstanceOfType(res, GetType(ParseError))
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Format))
    Assert.AreEqual("(  0:  2)", res.Span.ToString)
    Assert.AreEqual(2, res.InnerTokens.Count)
    Assert.IsInstanceOfType(res(0), GetType(FormatString.ArgHole.Format.Head))
    Assert.IsInstanceOfType(res(1), GetType(FormatString.ArgHole.Format.Body))
#Region "Check Format.Head"
    Dim Head = res(0)
    Assert.AreEqual("(  0:  1)", Head.Span.ToString)
    Assert.AreEqual(1, Head.InnerTokens.Count)
    Assert.IsInstanceOfType(Head(0), GetType(FormatString.ArgHole.Format.Colon))
    Assert.AreEqual("(  0:  1)", Head(0).Span.ToString)
#End Region
#Region "Check Format.Body"
    Dim Body = res(1)
    Assert.AreEqual("(  1:  1)", Body.Span.ToString)
    Assert.AreEqual(1, Body.InnerTokens.Count)
    Dim BodyText = Body(0)
    ' Assert.IsInstanceOfType(BodyText, GetType(FormatString.Text))
    Assert.AreEqual("(  1:  1)", BodyText.Span.ToString)
    Assert.AreEqual(1, BodyText.InnerTokens.Count)
    Assert.IsInstanceOfType(BodyText, GetType(ParseError))
    Dim pe As ParseError = DirectCast(BodyText, ParseError)
    Assert.AreEqual(ParseError.Reason.Invalid, pe.Why)
    Assert.AreEqual("(  1:  1)", pe.Span.ToString)
    Assert.AreEqual(1, pe.InnerTokens.Count)
    Assert.IsInstanceOfType(pe(0), GetType(FormatString.Common.Brace.Opening))
#End Region

  End Sub
End Class