Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser

  Private Function ArgAlign(ai As Align, Q As Parameters) As Analyser.Parameters
    ' Arg.Align::= Comma Whitespace? MinusSign? Digits Whitespaces?

    Dim en = ai.InnerTokens.GetEnumerator.GetEnumerator

Expecting_Comma:
    If en.MoveNext = False Then Q.Result.Issues += Issue.Unexpected.EoT(Nothing) : GoTo state_end
    Select Case en.Current.Kind
      Case TokenKind.Comma : GoTo Execting_Possible_Whitespace_0
      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(en.Current.Span, en.Current)
        GoTo Expecting_Comma
    End Select

Execting_Possible_Whitespace_0:
    If en.MoveNext = False Then Q.Result.Issues += Issue.Unexpected.EoT(Nothing) : GoTo state_end
    Select Case en.Current.Kind
      Case TokenKind.Whitespaces : GoTo Expecting_Possible_MinusSign
      Case TokenKind.MinusSign : GoTo Expecting_Digits
      Case TokenKind.Digits : GoTo Expecting_Digits_1
      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(en.Current.Span, en.Current)
        GoTo Execting_Possible_Whitespace_0
    End Select

Expecting_Possible_MinusSign:
    If en.MoveNext = False Then Q.Result.Issues += Issue.Unexpected.EoT(Nothing) : GoTo state_end
    Select Case en.Current.Kind
      Case TokenKind.MinusSign : GoTo Expecting_Digits
      Case TokenKind.Digit : GoTo Expecting_Digits_1
      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(en.Current.Span, en.Current)
        GoTo Expecting_Possible_MinusSign
    End Select

Expecting_Digits:
    If en.MoveNext = False Then Q.Result.Issues += Issue.Unexpected.EoT(Nothing) : GoTo state_end
Expecting_Digits_1:
    Select Case en.Current.Kind
      Case TokenKind.Digits
        Dim digits = DirectCast(en.Current, FSDv2.FormatString.Common.Digits).GetValue
        Q.Arg.Align = digits
        If Q.Arg.Align.HasValue = False Then
          Q.Result.Issues += Issue.Arg.Align.Missing(en.Current.Span)
        Else
          If Q.Arg.Align.Value >= Framework.UpperLimit Then Q.Result.Issues += Issue.Arg.Align.Framework.Upper_Limit_Exceeded(en.Current.Span)
          If Q.Arg.Align.Value <= Framework.LowerLimit Then Q.Result.Issues += Issue.Arg.Align.Framework.Lower_Limit_Exceeded(en.Current.Span)
        End If
        GoTo Expecting_Possible_Whitespace_1
      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(en.Current.Span, en.Current)
        GoTo Expecting_Digits
    End Select

Expecting_Possible_Whitespace_1:
    If en.MoveNext = False Then GoTo state_end
state_check_for_more_tokens:
    Do
      Q.Result.Issues += Issue.Unexpected.Token(en.Current.Span, en.Current)
    Loop While en.MoveNext
state_end:
    Return Q
  End Function

End Class
