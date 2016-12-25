Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser

  Private Function ArgAlign_Head(ByRef idx As Integer, edx As Integer, src As Token, q As Parameters) As Parameters
    If idx >= edx Then q.Result.Issues += Issue.Unexpected.EoT(Nothing) : Return q
    '    Dim tkn = TryCast(src, FSDv2.FormatString.ArgHole.Align.Head)
    'Debug.Assert(tkn IsNot Nothing)
    Dim idx0 = 0, edx0 = src.InnerTokens.Count
Expecting_Comma:
    If idx0 >= edx0 Then
      'q.Result. Issues += Issue.Unexpected.EoT(Nothing)
      Return q
    End If
    Dim current = src(idx0)
    Select Case current.Kind
      Case TokenKind.Comma
        idx0 += 1 : GoTo Expecting_Possible_Whitespaces
      Case Else
        q.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        idx0 += 1
        GoTo Expecting_Comma
    End Select
Expecting_Possible_Whitespaces:
    If idx0 >= edx0 Then
      'q.Result.Issues += Issue.Unexpected.EoT(Nothing)
      Return q
    End If
    current = src(idx0)
    Select Case current.Kind
      Case TokenKind.Whitespaces
        idx0 += 1 : GoTo Completed_ArgAlign_Head
      Case Else
        q.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        idx0 += 1
        GoTo Expecting_Possible_Whitespaces
    End Select
Completed_ArgAlign_Head:
    While idx0 < edx0
      current = src(idx0)
      q.Result.Issues += Issue.Unexpected.Token(current.Span, current)
      idx0 += 1

    End While
    Return q

  End Function

  Private Function ArgAlign_Body_Completed(ByRef idx As Integer, edx As Integer, src As Token, q As Parameters) As Parameters
    While idx < edx
      Dim current = src(idx)
      q.Result.Issues += Issue.Unexpected.Token(current.Span, current)
      idx += 1
    End While
    Return q
  End Function

  Private Function ArgAlign_Expecting_PossibleMinusSign(ByRef idx As Integer, edx As Integer, src As Token, q As Parameters) As Parameters
    If idx >= edx Then q.Result.Issues += Issue.Unexpected.EoT(Nothing) : Return q
    Dim tkn = TryCast(src, FSDv2.FormatString.ArgHole.Align.Body)
    Debug.Assert(tkn IsNot Nothing)
    Dim idx0 = 0, edx0 = tkn.InnerTokens.Count
Expecting_Possible_MinusSign:
    If idx0 >= edx0 Then q.Result.Issues += Issue.Unexpected.EoT(Nothing) : Return ArgAlign_Body_Completed(idx, edx, src, q)
    Dim Current = tkn(idx0)
    Select Case Current.Kind
      Case TokenKind.MinusSign
        idx0 += 1
        Return ArgAlign_Expecting_Digits(idx0, edx0, Current, q)
      Case TokenKind.Digits
        Return ArgAlign_Expecting_Digits(idx0, edx0, Current, q)
      Case Else
        q.Result.Issues += Issue.Unexpected.Token(Current.Span, Current)
        idx0 += 1
        GoTo Expecting_Possible_MinusSign
    End Select


  End Function

  Private Function ArgAlign_Expecting_Possible_Whitespace_1(ByRef idx As Integer, edx As Integer, src As Token, q As Parameters) As Parameters
Expecting_Whitespace:
    If idx >= edx Then
      'q.Result.Issues += Issue.Unexpected.EoT(Nothing)
      Return q
    End If
    Dim Current = src(idx)
    Select Case Current.Kind
      Case TokenKind.Whitespaces
        idx += 1
        Return ArgAlign_Body_Completed(idx, edx, src, q)
      Case Else
        Debugger.Break()
        q.Result.Issues += Issue.Unexpected.Token(Current.Span, Current)
        idx += 1
        GoTo Expecting_Whitespace
    End Select

    Return q
  End Function

  Private Function ArgAlign_Expecting_Digits(ByRef idx As Integer, edx As Integer, src As Token, q As Parameters) As Parameters
    Dim tkn = TryCast(src, FSDv2.FormatString.Common.Digits)
    Debug.Assert(tkn IsNot Nothing)
Expecting_Digits:
    If idx >= edx Then
      'q.Result.Issues += Issue.Unexpected.EoT(Nothing)
      Return q
    End If
    Dim Current = tkn '(idx)
    Select Case Current.Kind
      Case TokenKind.Digits
        Dim digits = DirectCast(Current, FSDv2.FormatString.Common.Digits).GetValue
        If q.Arg Is Nothing Then q.Arg = New Arg
        q.Arg.Align = digits
        If q.Arg.Align.HasValue = False Then
          q.Result.Issues += Issue.Arg.Align.Missing(Current.Span)
        ElseIf q.Arg.Align.Value >= Framework.UpperLimit Then
          q.Result.Issues += Issue.Arg.Align.Framework.Upper_Limit_Exceeded(Current.Span)
        ElseIf q.Arg.Align.Value <= Framework.LowerLimit Then
          q.Result.Issues += Issue.Arg.Align.Framework.Lower_Limit_Exceeded(Current.Span)
        End If
        idx += 1
        Return ArgAlign_Expecting_Possible_Whitespace_1(idx, edx, src, q)
      Case Else
        ' q.Result.Issues += Issue.Unexpected.Token(Current.Span, Current)
        idx += 1
        GoTo Expecting_Digits
    End Select

  End Function

  Private Function ArgAlign_Body(ByRef idx As Integer, edx As Integer, src As Token, q As Parameters) As Parameters
    If idx >= edx Then Return q
    Dim tkn = TryCast(src, FSDv2.FormatString.ArgHole.Align.Body)
    Debug.Assert(tkn IsNot Nothing)
    Dim idx0 = 0, edx0 = tkn.InnerTokens.Count
    q = ArgAlign_Expecting_PossibleMinusSign(idx0, edx0, tkn, q)
    idx += 1
    If idx >= edx Then Return q
    Return ArgAlign_Body_Completed(idx, edx, src, q)
  End Function

  Private Function ArgAlign(ai As Align, Q As Parameters) As Analyser.Parameters
    ' Arg.Align::= Comma Whitespace? MinusSign? Digits Whitespaces?
    Dim idx = 0, edx = ai.InnerTokens.Count
Expecting_ArgIndex_Head:
    If idx >= edx Then Q.Result.Issues += Issue.Unexpected.EoT(Nothing) : Return Q

    Dim current = ai(idx)
    Select Case current.Kind
      Case TokenKind.ArgHole_Align_Head
        Q = ArgAlign_Head(idx, edx, current, Q)
        idx += 1
        If idx >= edx Then Return Q
        current = ai(idx)
        Q = ArgAlign_Body(idx, edx, current, Q)

      Case TokenKind.ArgHole_Align_Body
        ' missing argalign_head
        Return ArgAlign_Body(idx, edx, ai, Q)
      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        idx += 1

        GoTo Expecting_ArgIndex_Head
    End Select
    Return Q
  End Function

End Class
