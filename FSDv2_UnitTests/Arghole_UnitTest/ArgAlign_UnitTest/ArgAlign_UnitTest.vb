Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass()>
Public Class ArgAlign_UnitTest

  '
  '   ArgAlign ::= Align_Head Align_Body
  ' Align_Head ::= Comma Whitespaces? 
  ' Align_Body ::= MinusSign? Digits Whitespaces?
  '

  <TestMethod, TestCategory("Tokens.Arghole.ArgAlign")>
  Public Sub _00_()
    Dim Text = ""
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Align.TryParse(FirstPos)
    Assert.IsNull(res)
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgAlign")>
  Public Sub _01_()
    Dim Text = ",1"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Align.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Align))
    Assert.AreEqual(TokenKind.ArgHole_Align, res.Kind)
    Assert.AreEqual("(  0:  2)", res.Span.ToString)

    Assert.AreEqual(2, res.Inner.Count)

    Assert.AreEqual(TokenKind.ArgHole_Align_Head, res.Inner(0).Kind)
    Assert.AreEqual(TokenKind.ArgHole_Align_Body, res.Inner(1).Kind)
    ' Check the head portion
#Region "Check the head part of the Align"
    ' Expecting: Comma Whitspace?
    Dim Head = TryCast(res.Inner(0), FormatString.ArgHole.Align.Head)
    Assert.IsNotNull(Head)
    Assert.AreEqual("(  0:  1)", Head.Span.ToString)
    Assert.AreEqual(2, Head.Inner.Count)
    Assert.AreEqual(TokenKind.Comma, Head.Inner(0).Kind)
    Assert.AreEqual("(  0:  1)", Head.Inner(0).Span.ToString)
    ' Check for the optional whitespace, there shouldn't be any.
    Assert.AreEqual(TokenKind.Whitespaces, Head.Inner(1).Kind)
    Assert.AreEqual(0, Head.Inner(1).Span.Size)
    Assert.AreEqual("(  1:  0)", Head.Inner(1).Span.ToString)
#End Region
#Region "Check the body part of the Align"
    Dim Body = TryCast(res.Inner(1), FormatString.ArgHole.Align.Body)
    Assert.IsNotNull(Body)
    Assert.AreEqual("(  1:  1)", Body.Span.ToString)
    Assert.AreEqual(1, Body.Inner.Count)
    Assert.AreEqual(TokenKind.Digits, Body.Inner(0).Kind)
    Assert.AreEqual("(  1:  1)", Body.Inner(0).Span.ToString)
    Assert.AreEqual("1", Body.Inner(0).Span.Text)


#End Region
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgAlign")>
  Public Sub _02_()
    '           0123
    Dim Text = ",  1"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Align.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Align))
    Assert.AreEqual(TokenKind.ArgHole_Align, res.Kind)
    Assert.AreEqual(2, res.Inner.Count)

    Assert.AreEqual(TokenKind.ArgHole_Align_Head, res.Inner(0).Kind)
    Assert.AreEqual(TokenKind.ArgHole_Align_Body, res.Inner(1).Kind)
    ' Check the head portion
#Region "Check the head part of the Align"
    ' Expecting: Comma Whitspace?
    Dim Head = TryCast(res.Inner(0), FormatString.ArgHole.Align.Head)
    Assert.IsNotNull(Head)
    Assert.AreEqual(2, Head.Inner.Count)
    Assert.AreEqual(TokenKind.Comma, Head.Inner(0).Kind)
    Assert.AreEqual("(  0:  1)", Head.Inner(0).Span.ToString)
    ' Check for the optional whitespace, there shouldn be some.
    Assert.AreEqual(TokenKind.Whitespaces, Head.Inner(1).Kind)
    Assert.AreEqual(2, Head.Inner(1).Span.Size)
    Assert.AreEqual("(  1:  2)", Head.Inner(1).Span.ToString)
#End Region
#Region "Check the body part of the Align"
    Dim Body = TryCast(res.Inner(1), FormatString.ArgHole.Align.Body)
    Assert.IsNotNull(Body)

    Assert.AreEqual(1, Body.Inner.Count)

    Assert.AreEqual(TokenKind.Digits, Body.Inner(0).Kind)
    Assert.AreEqual("(  3:  1)", Body.Inner(0).Span.ToString)
    Assert.AreEqual("1", Body.Inner(0).Span.Text)

#End Region

  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgAlign")>
  Public Sub _03_()
    Dim Text = ",-1"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Align.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Align))
    Assert.AreEqual(TokenKind.ArgHole_Align, res.Kind)
    Assert.AreEqual(2, res.Inner.Count)

    Assert.AreEqual(TokenKind.ArgHole_Align_Head, res.Inner(0).Kind)
    Assert.AreEqual(TokenKind.ArgHole_Align_Body, res.Inner(1).Kind)
    ' Check the head portion
#Region "Check the head part of the Align"
    ' Expecting: Comma Whitspace?
    Dim Head = TryCast(res.Inner(0), FormatString.ArgHole.Align.Head)
    Assert.IsNotNull(Head)
    Assert.AreEqual("(  0:  1)", Head.Span.ToString)
    Assert.AreEqual(2, Head.Inner.Count)
    Assert.AreEqual(TokenKind.Comma, Head.Inner(0).Kind)
    Assert.AreEqual("(  0:  1)", Head.Inner(0).Span.ToString)
    ' Check for the optional whitespace, there shouldn't be any.
    Assert.AreEqual(TokenKind.Whitespaces, Head.Inner(1).Kind)
    Assert.AreEqual(0, Head.Inner(1).Span.Size)
    Assert.AreEqual("(  1:  0)", Head.Inner(1).Span.ToString)
#End Region
#Region "Check the body part of the Align"
    Dim Body = TryCast(res.Inner(1), FormatString.ArgHole.Align.Body)
    Assert.IsNotNull(Body)
    ' Expecting: MinusSign Digits
    Assert.AreEqual("(  1:  2)", Body.Span.ToString)

    Assert.AreEqual(2, Body.Inner.Count)
    Assert.AreEqual(TokenKind.MinusSign, Body.Inner(0).Kind)
    Assert.AreEqual("(  1:  1)", Body.Inner(0).Span.ToString)

    Assert.AreEqual(TokenKind.Digits, Body.Inner(1).Kind)
    Assert.AreEqual("(  2:  1)", Body.Inner(1).Span.ToString)
    Assert.AreEqual("1", Body.Inner(1).Span.Text)


#End Region
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgAlign")>
  Public Sub _04_()
    Dim Text = ", -1"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Align.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Align))
    Assert.AreEqual(TokenKind.ArgHole_Align, res.Kind)
    Assert.AreEqual(2, res.Inner.Count)

    Assert.AreEqual(TokenKind.ArgHole_Align_Head, res.Inner(0).Kind)
    Assert.AreEqual(TokenKind.ArgHole_Align_Body, res.Inner(1).Kind)
    ' Check the head portion
#Region "Check the head part of the Align"
    ' Expecting: Comma Whitspace?
    Dim Head = TryCast(res.Inner(0), FormatString.ArgHole.Align.Head)
    Assert.IsNotNull(Head)
    Assert.AreEqual("(  0:  2)", Head.Span.ToString)
    Assert.AreEqual(2, Head.Inner.Count)
    Assert.AreEqual(TokenKind.Comma, Head.Inner(0).Kind)
    Assert.AreEqual("(  0:  1)", Head.Inner(0).Span.ToString)
    ' Check for the optional whitespace, there shouldn't be any.
    Assert.AreEqual(TokenKind.Whitespaces, Head.Inner(1).Kind)
    Assert.AreEqual(1, Head.Inner(1).Span.Size)
    Assert.AreEqual("(  1:  1)", Head.Inner(1).Span.ToString)
#End Region
#Region "Check the body part of the Align"
    Dim Body = TryCast(res.Inner(1), FormatString.ArgHole.Align.Body)
    Assert.IsNotNull(Body)
    ' Expecting: MinusSign Digits
    Assert.AreEqual("(  2:  2)", Body.Span.ToString)

    Assert.AreEqual(2, Body.Inner.Count)
    Assert.AreEqual(TokenKind.MinusSign, Body.Inner(0).Kind)
    Assert.AreEqual("(  2:  1)", Body.Inner(0).Span.ToString)

    Assert.AreEqual(TokenKind.Digits, Body.Inner(1).Kind)
    Assert.AreEqual("(  3:  1)", Body.Inner(1).Span.ToString)
    Assert.AreEqual("1", Body.Inner(1).Span.Text)


#End Region
  End Sub


  <TestMethod, TestCategory("Tokens.Arghole.ArgAlign")>
  Public Sub _05_()
    Dim Text = ",1 "
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Align.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Align))
    Assert.AreEqual(TokenKind.ArgHole_Align, res.Kind)
    Assert.AreEqual("(  0:  3)", res.Span.ToString)

    Assert.AreEqual(2, res.Inner.Count)

    Assert.AreEqual(TokenKind.ArgHole_Align_Head, res.Inner(0).Kind)
    Assert.AreEqual(TokenKind.ArgHole_Align_Body, res.Inner(1).Kind)
    ' Check the head portion
#Region "Check the head part of the Align"
    ' Expecting: Comma Whitspace?
    Dim Head = TryCast(res.Inner(0), FormatString.ArgHole.Align.Head)
    Assert.IsNotNull(Head)
    Assert.AreEqual("(  0:  1)", Head.Span.ToString)
    Assert.AreEqual(2, Head.Inner.Count)
    Assert.AreEqual(TokenKind.Comma, Head.Inner(0).Kind)
    Assert.AreEqual("(  0:  1)", Head.Inner(0).Span.ToString)
    ' Check for the optional whitespace, there shouldn't be any.
    Assert.AreEqual(TokenKind.Whitespaces, Head.Inner(1).Kind)
    Assert.AreEqual(0, Head.Inner(1).Span.Size)
    Assert.AreEqual("(  1:  0)", Head.Inner(1).Span.ToString)
#End Region
#Region "Check the body part of the Align"
    Dim Body = TryCast(res.Inner(1), FormatString.ArgHole.Align.Body)
    Assert.IsNotNull(Body)
    Assert.AreEqual("(  1:  2)", Body.Span.ToString)
    Assert.AreEqual(2, Body.Inner.Count)
    Assert.AreEqual(TokenKind.Digits, Body.Inner(0).Kind)
    Assert.AreEqual(TokenKind.Whitespaces, Body.Inner(1).Kind)
    Assert.AreEqual("(  1:  1)", Body.Inner(0).Span.ToString)
    Assert.AreEqual("1", Body.Inner(0).Span.Text)
    Assert.AreEqual("(  2:  1)", Body.Inner(1).Span.ToString)

#End Region
  End Sub


  <TestMethod, TestCategory("Tokens.Arghole.ArgAlign")>
  Public Sub _06_()
    Dim Text = ", -1 "
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Align.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Align))
    Assert.AreEqual(TokenKind.ArgHole_Align, res.Kind)
    Assert.AreEqual("(  0:  5)", res.Span.ToString)
    Assert.AreEqual(2, res.Inner.Count)

    Assert.AreEqual(TokenKind.ArgHole_Align_Head, res.Inner(0).Kind)
    Assert.AreEqual(TokenKind.ArgHole_Align_Body, res.Inner(1).Kind)
    ' Check the head portion
#Region "Check the head part of the Align"
    ' Expecting: Comma Whitspace?
    Dim Head = TryCast(res.Inner(0), FormatString.ArgHole.Align.Head)
    Assert.IsNotNull(Head)
    Assert.AreEqual("(  0:  2)", Head.Span.ToString)
    Assert.AreEqual(2, Head.Inner.Count)
    Assert.AreEqual(TokenKind.Comma, Head.Inner(0).Kind)
    Assert.AreEqual("(  0:  1)", Head.Inner(0).Span.ToString)
    ' Check for the optional whitespace, there shouldn't be any.
    Assert.AreEqual(TokenKind.Whitespaces, Head.Inner(1).Kind)
    Assert.AreEqual(1, Head.Inner(1).Span.Size)
    Assert.AreEqual("(  1:  1)", Head.Inner(1).Span.ToString)
#End Region
#Region "Check the body part of the Align"
    Dim Body = TryCast(res.Inner(1), FormatString.ArgHole.Align.Body)
    Assert.IsNotNull(Body)
    ' Expecting: MinusSign Digits
    Assert.AreEqual("(  2:  3)", Body.Span.ToString)

    Assert.AreEqual(3, Body.Inner.Count)
    Assert.AreEqual(TokenKind.MinusSign, Body.Inner(0).Kind)
    Assert.AreEqual("(  2:  1)", Body.Inner(0).Span.ToString)

    Assert.AreEqual(TokenKind.Digits, Body.Inner(1).Kind)
    Assert.AreEqual("(  3:  1)", Body.Inner(1).Span.ToString)
    Assert.AreEqual("1", Body.Inner(1).Span.Text)

    Assert.AreEqual(TokenKind.Whitespaces, Body.Inner(2).Kind)
    Assert.AreEqual("(  4:  1)", Body.Inner(2).Span.ToString)
#End Region
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgAlign")>
  Public Sub _07_()
    Dim Text = ", -1"
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Align.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Align))
    Assert.AreEqual(TokenKind.ArgHole_Align, res.Kind)
    Assert.AreEqual("(  0:  4)", res.Span.ToString)
    Assert.AreEqual(2, res.Inner.Count)

    Assert.AreEqual(TokenKind.ArgHole_Align_Head, res.Inner(0).Kind)
    Assert.AreEqual(TokenKind.ArgHole_Align_Body, res.Inner(1).Kind)
    ' Check the head portion
#Region "Check the head part of the Align"
    ' Expecting: Comma Whitspace?
    Dim Head = TryCast(res.Inner(0), FormatString.ArgHole.Align.Head)
    Assert.IsNotNull(Head)
    Assert.AreEqual("(  0:  2)", Head.Span.ToString)
    Assert.AreEqual(2, Head.Inner.Count)
    Assert.AreEqual(TokenKind.Comma, Head.Inner(0).Kind)
    Assert.AreEqual("(  0:  1)", Head.Inner(0).Span.ToString)
    ' Check for the optional whitespace, there shouldn't be any.
    Assert.AreEqual(TokenKind.Whitespaces, Head.Inner(1).Kind)
    Assert.AreEqual(1, Head.Inner(1).Span.Size)
    Assert.AreEqual("(  1:  1)", Head.Inner(1).Span.ToString)
#End Region
#Region "Check the body part of the Align"
    Dim Body = TryCast(res.Inner(1), FormatString.ArgHole.Align.Body)
    Assert.IsNotNull(Body)
    ' Expecting: MinusSign Digits
    Assert.AreEqual("(  2:  2)", Body.Span.ToString)

    Assert.AreEqual(2, Body.Inner.Count)
    Assert.AreEqual(TokenKind.MinusSign, Body.Inner(0).Kind)
    Assert.AreEqual("(  2:  1)", Body.Inner(0).Span.ToString)

    Assert.AreEqual(TokenKind.Digits, Body.Inner(1).Kind)
    Assert.AreEqual("(  3:  1)", Body.Inner(1).Span.ToString)
    Assert.AreEqual("1", Body.Inner(1).Span.Text)

#End Region
  End Sub

  <TestMethod, TestCategory("Tokens.Arghole.ArgAlign")>
  Public Sub _08_()
    Dim Text = ", 1 "
    Dim TheSource = Source.Create(Text, Source.SourceKind.VB_Standard)
    Dim FirstPos = TheSource.First
    Dim res = FormatString.ArgHole.Align.TryParse(FirstPos)
    Assert.IsNotNull(res)
    Assert.IsInstanceOfType(res, GetType(FormatString.ArgHole.Align))
    Assert.AreEqual(TokenKind.ArgHole_Align, res.Kind)
    Assert.AreEqual("(  0:  4)", res.Span.ToString)
    Assert.AreEqual(2, res.Inner.Count)

    Assert.AreEqual(TokenKind.ArgHole_Align_Head, res.Inner(0).Kind)
    Assert.AreEqual(TokenKind.ArgHole_Align_Body, res.Inner(1).Kind)
    ' Check the head portion
#Region "Check the head part of the Align"
    ' Expecting: Comma Whitspace?
    Dim Head = TryCast(res.Inner(0), FormatString.ArgHole.Align.Head)
    Assert.IsNotNull(Head)
    Assert.AreEqual("(  0:  2)", Head.Span.ToString)
    Assert.AreEqual(2, Head.Inner.Count)
    Assert.AreEqual(TokenKind.Comma, Head.Inner(0).Kind)
    Assert.AreEqual("(  0:  1)", Head.Inner(0).Span.ToString)
    ' Check for the optional whitespace, there shouldn't be any.
    Assert.AreEqual(TokenKind.Whitespaces, Head.Inner(1).Kind)
    Assert.AreEqual(1, Head.Inner(1).Span.Size)
    Assert.AreEqual("(  1:  1)", Head.Inner(1).Span.ToString)
#End Region
#Region "Check the body part of the Align"
    Dim Body = TryCast(res.Inner(1), FormatString.ArgHole.Align.Body)
    Assert.IsNotNull(Body)
    ' Expecting: MinusSign Digits
    Assert.AreEqual("(  2:  2)", Body.Span.ToString)

    Assert.AreEqual(2, Body.Inner.Count)

    Assert.AreEqual(TokenKind.Digits, Body.Inner(0).Kind)
    Assert.AreEqual("(  2:  1)", Body.Inner(0).Span.ToString)
    Assert.AreEqual("1", Body.Inner(0).Span.Text)

    Assert.AreEqual(TokenKind.Whitespaces, Body.Inner(1).Kind)
    Assert.AreEqual("(  3:  1)", Body.Inner(1).Span.ToString)
#End Region
  End Sub
End Class