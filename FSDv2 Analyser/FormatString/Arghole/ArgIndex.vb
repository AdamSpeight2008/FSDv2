Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser

  Private Function ArgIndex(ai As Index, Q As Parameters) As Analyser.Parameters
    ' Arg.Index ::= Digits Whitespaces?
    Dim en = ai.InnerTokens.GetEnumerator.GetEnumerator

Expecting_Digits:
    If en.MoveNext = False Then Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_End, Nothing) : GoTo state_end
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


End Class
