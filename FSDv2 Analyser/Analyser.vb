Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Public Class Analyser
  Public Class Issue
    Public ReadOnly Property Kind As Issue.Kinds
    Public ReadOnly Property Span As Source.Span
    Public ReadOnly Property Additional As String

    Public Sub New(Kind As Kinds, Span As Source.Span, Optional Additional As String = Nothing)
      Me.Kind = Kind : Me.Span = Span : Me.Additional = If(Additional, String.Empty)
    End Sub

    Public Enum Kinds As Integer
      Unexpected_End
      Unexpected_Characters
      Arg_Index_Framework_Upper_Limit_Exceeded
      Arg_Index_FrameWork_Lower_Limit_Exceeded
      Arg_Index_OutOfRange
      Arg_Index_Missing
      Unexpected_Token
      Arg_Align_Missing
      Arg_Align_Framework_Upper_Limit_Exceeded
      Arg_Align_Framework_Lower_Limit_Exceeded
      Invalid
    End Enum

    Public Shared Operator +(Issue0 As Issue, Issue1 As Issue) As Issues
      Return Issues.Empty + Issue0 + Issue1
    End Operator

  End Class

  Public Class Issues
    Public Shared ReadOnly Property Empty() As New Issues(Nothing)
    Public ReadOnly Property Issues As Issue()
    Private Sub New(Issues As IEnumerable(Of Issues))
      Me.Issues = If(Issues Is Nothing, Array.Empty(Of Issue), Issues.ToArray)
    End Sub

    Public Shared Operator +(Issue As Issue, Issues As Issues) As Issues
      Return New Issues(Enumerable.Repeat(Issue, 1).Concat(Issues))
    End Operator

    Public Shared Operator +(Issues As Issues, Issue As Issue) As Issues
      Return New Issues(Issues.Issues.Concat(Enumerable.Repeat(Issue, 1)))
    End Operator

    Public Shared Operator +(Issues0 As Issues, Issues1 As Issues) As Issues
      Return New Issues(Issues0.Issues.Concat(Issues1.Issues))
    End Operator

  End Class

  Public Class Result
    Public Property Issues As Issues = Issues.Empty
    Public Sub New()

    End Sub

  End Class

  Public Class Args
    Public ReadOnly Property Count As Integer
    Private _Used As New Generic.HashSet(Of BigInteger)
    Public Sub MarkAsUsed(ai As BigInteger?)
      If ai.HasValue = False Then Exit Sub
      _Used.Add(ai.Value)
    End Sub
  End Class

  Public Class Parameters
    Public Property Arg As Arg
    Public ReadOnly Property Args As Args
    Public Property Result As Result
    Public Sub New()
      Me.Result = New Result
    End Sub

  End Class

  Public Function Analyse(FS As FSDv2.FormatString, q As Parameters) As Parameters
    For Each t As Token In FS.InnerTokens.GetEnumerator
      Select Case t.Kind
        Case TokenKind.ArgHole
      End Select

    Next

  End Function

  Private Function Text(Txt As FormatString.Text, Q As Parameters) As Parameters
    If Txt.InnerTokens.Count = 0 Then Return Q
    Dim en = Txt.InnerTokens.GetEnumerator.GetEnumerator
    While en.MoveNext
      Select Case en.Current.Kind
        Case TokenKind.ParseError
        Case Else
      End Select
    End While
    Return Q
  End Function

  Private Function ArgHole(AH As FormatString.ArgHole, Q As Parameters) As Parameters
    Dim en = AH.InnerTokens.GetEnumerator.GetEnumerator

Expecting_Opening_Brace:
    If en.MoveNext = False Then GoTo
    Select Case en.Current.Kind
      Case TokenKind.Esc_Brace_Opening
      Case Else
    End Select
Expecting_Arghole_Index:
    If en.MoveNext = False Then GoTo
    Select Case en.Current.Kind
      Case TokenKind.ArgHole_Index : ArgIndex(DirectCast(en.Current, Index), Q)
      Case TokenKind.ParseError

      Case Else
        GoTo Expecting_Arghole_Index
    End Select
Expection_Arghole_Align:
Expecting_Arghole_Format:
Expecting_Closing_Brace:
    Return Q
  End Function

  Private Function ParseError(pe As ParseError, Q As Parameters) As Parameters
    Select Case pe.Why
      Case FSDv2.ParseError.Reason.UnexpectedCharacter : Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Characters, pe.Span)
      Case FSDv2.ParseError.Reason.Invalid : Q.Result.Issues += New Issue(Issue.Kinds.Invalid, pe.Span)
      Case Else
        Throw New NotImplementedException($"ParseError.Why:= {pe.Why}")
    End Select
    Return Q
  End Function

  Private Function ArgIndex(ai As Index, Q As Parameters) As Analyser.Parameters
    ' Arg.Index ::= Digits Whitespaces?
    Dim en = ai.InnerTokens.GetEnumerator.GetEnumerator

Expecting_Digits:
    If en.MoveNext = False Then results.Issues += New Issue(Issue.Kinds.Unexpected_End, Nothing) : GoTo state_end
    Select Case en.Current.Kind
      Case TokenKind.Digits
        Dim digits = DirectCast(en.Current, FSDv2.FormatString.Common.Digits).GetValue
        Q.Arg.Index = digits
        If Q.Arg.Index.HasValue = False Then
          Q.Result.Issues += New Issue(Issue.Kinds.Arg_Index_Missing, en.Current.Span)
        Else
          If Q.Arg.Index.Value >= Framework.UpperLimit Then Q.Result.Issues += New Issue(Issue.Kinds.Arg_Index_Framework_Upper_Limit_Exceeded, en.Current.Span)
          If Q.Arg.Index.Value >= Q.Args.Count Then Q.Result.Issues += New Issue(Issue.Kinds.Arg_Index_OutOfRange, en.Current.Span)
          Q.Args.MarkAsUsed(Q.Arg.Index.Value)
        End If
      Case TokenKind.Whitespaces
        ' An easy mistake to make is to have whitespaces after the opening brace.
        ' Eg: { 0}
        Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, en.Current.Span, "")
        GoTo Expecting_Digits
      Case Else
        Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, en.Current.Span, "")
        GoTo Expecting_Digits
    End Select
    GoTo Expecting_Possible_Whitespace

Expecting_Possible_Whitespace:
    If en.MoveNext = False Then GoTo state_end
    Select Case en.Current.Kind
      Case TokenKind.Whitespaces : GoTo state_check_no_more_tokens
      Case Else
        Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, en.Current.Span, "")
        GoTo state_check_no_more_tokens
    End Select

state_check_no_more_tokens:
    While en.MoveNext
      Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, en.Current.Span, "")
    End While
state_end:
    Return Q
  End Function

  Private Function ArgAlign(ai As Index, Q As Parameters) As Analyser.Parameters
    ' Arg.Align::= Comma Whitespace? MinusSign? Digits Whitespaces?

    Dim en = ai.InnerTokens.GetEnumerator.GetEnumerator

Expecting_Comma:
    If en.MoveNext = False Then Q.results += New Issue(Issue.Kinds.Unexpected_End, Nothing) : GoTo state_end
    Select Case en.Current.Kind
      Case TokenKind.Comma : GoTo Execting_Possible_Whitespace_0
      Case Else
        Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, en.Current.Span, "")
        GoTo Expecting_Comma
    End Select

Execting_Possible_Whitespace_0:
    If en.MoveNext = False Then Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_End, Nothing) : GoTo state_end
    Select Case en.Current.Kind
      Case TokenKind.Whitespaces : GoTo Expecting_Possible_MinusSign
      Case TokenKind.MinusSign : GoTo Expecting_Digits
      Case TokenKind.Digits : GoTo Expecting_Digits_1
      Case Else
        Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, en.Current.Span, "")
        GoTo Execting_Possible_Whitespace_0
    End Select

Expecting_Possible_MinusSign:
    If en.MoveNext = False Then Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_End, Nothing) : GoTo state_end
    Select Case en.Current.Kind
      Case TokenKind.MinusSign : GoTo Expecting_Digits
      Case TokenKind.Digit : GoTo Expecting_Digits_1
      Case Else
        Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, en.Current.Span, "")
        GoTo Expecting_Possible_MinusSign
    End Select

Expecting_Digits:
    If en.MoveNext = False Then Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_End, Nothing) : GoTo state_end
Expecting_Digits_1:
    Select Case en.Current.Kind
      Case TokenKind.Digits
        Dim digits = DirectCast(en.Current, FSDv2.FormatString.Common.Digits).GetValue
        Q.Arg.Align = digits
        If Q.Arg.Align.HasValue = False Then
          Q.Result.Issues += New Issue(Issue.Kinds.Arg_Align_Missing, en.Current.Span)
        Else
          If Q.Arg.Align.Value >= Framework.UpperLimit Then results.Issues += New Issue(Issue.Kinds.Arg_Align_Framework_Upper_Limit_Exceeded, en.Current.Span)
          If Q.Arg.Align.Value <= Framework.LowerLimit Then results.Issues += New Issue(Issue.Kinds.Arg_Align_Framework_Lower_Limit_Exceeded, en.Current.Span)
        End If
        GoTo Expecting_Possible_Whitespace_1
      Case Else
        Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, en.Current.Span, "")
        GoTo Expecting_Digits
    End Select

Expecting_Possible_Whitespace_1:
    If en.MoveNext = False Then GoTo state_end
state_check_for_more_tokens:
    Do
      Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, en.Current.Span)
    Loop While en.MoveNext
state_end:
    Return Q
  End Function

  Class Framework
    Public Shared ReadOnly Property UpperLimit As New BigInteger?(1000000)
    Public Shared ReadOnly Property LowerLimit As New BigInteger?(-1000000)
  End Class

End Class



Public Class Arg
  Public Property Index As BigInteger?
  Public Property Align As BigInteger?
  Public Property Format As String
End Class