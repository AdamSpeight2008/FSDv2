Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser
  Private Function Expecting_Possible_Whitespaces(
                                             ByRef idx As Integer, edx As Integer, src As Token,
                                                   Results As Parameters
                                                 ) As Parameters
Expecting_Possible_Whitespaces:
    If idx >= edx Then Return Results
    Dim current As Token = src(idx)
    If current.Kind = TokenKind.Whitespaces Then Return Results
    Results.Result.Issues += Issue.Unexpected.Token(current.Span, current)
    idx += 1
    GoTo Expecting_Possible_Whitespaces
  End Function

  Private Function Expecting_Comma(
                              ByRef idx As Integer, edx As Integer, src As Token,
                                    Results As Parameters
                                  ) As Parameters
Expecting_Comma:
    If EndOfText(idx, edx) Then Return Results
    Dim current = src(idx)
    If current.Kind <> TokenKind.Comma Then
      Results.Result.Issues += Issue.Unexpected.Token(current.Span, current)
      idx += 1
      GoTo Expecting_Comma
    End If
    idx += 1
    Return Results
  End Function

  Private Function ArgAlign_Head(
                            ByRef idx As Integer, edx As Integer, src As Token,
                                  Results As Parameters
                                  ) As Parameters
    If EndOfText(idx, edx) Then Return Results

    Dim idx0 = 0, edx0 = src.InnerTokens.Count
    Results = Expecting_Comma(idx0, edx0, src, Results)
    Results = Expecting_Possible_Whitespaces(idx0, edx0, src, Results)
    Results = AnyMoreTokensAreUnexpected(idx0, edx0, src, Results)
    Return Results
  End Function

  Private Function ArgAlign_Expecting_PossibleMinusSign(
                                                   ByRef idx As Integer,
                                                         edx As Integer,
                                                         src As Token,
                                                     Results As Parameters
                                                       ) As Parameters
    If EndOfText(idx, edx) Then Return Results
    Dim tkn = TryCast(src, FSDv2.FormatString.ArgHole.Align.Body)
    Debug.Assert(tkn IsNot Nothing)
    Dim idx0 = 0, edx0 = tkn.InnerTokens.Count

Expecting_Possible_MinusSign:
    If EndOfText(idx0, edx0) Then Return AnyMoreTokensAreUnexpected(idx, edx, src, Results)

    Dim Current = tkn(idx0)
    Select Case Current.Kind
      Case TokenKind.MinusSign
        idx0 += 1 : Return ArgAlign_Expecting_Digits(idx0, edx0, Current, Results)

      Case TokenKind.Digits
        Return ArgAlign_Expecting_Digits(idx0, edx0, Current, Results)

      Case Else
        Results.Result.Issues += Issue.Unexpected.Token(Current.Span, Current)
        idx0 += 1
        GoTo Expecting_Possible_MinusSign

    End Select
    Return Results
  End Function

  Private Function Validate_ArgAlign(ThisToken As Token, results As Parameters) As Parameters

    Dim Digits = TryCast(ThisToken, FSDv2.FormatString.Common.Digits)
    If Digits Is Nothing Then

    Else
      Dim ArgAlignValue = Digits.GetValue

      If results.Arg Is Nothing Then results.Arg = New Arg
      results.Arg.Align = ArgAlignValue
      If results.Arg.Align.HasValue = False Then
        results.Result.Issues += Issue.Arg.Align.Missing(ThisToken.Span)
      Else
        If results.Arg.Align.Value >= Framework.UpperLimit Then results.Result.Issues += Issue.Arg.Align.Framework.Upper_Limit_Exceeded(ThisToken.Span)
        If results.Arg.Align.Value <= Framework.LowerLimit Then results.Result.Issues += Issue.Arg.Align.Framework.Lower_Limit_Exceeded(ThisToken.Span)
      End If
    End If

    Return results
  End Function

  Private Function ArgAlign_Expecting_Digits(ByRef idx As Integer, edx As Integer, ThisToken As Token, Results As Parameters) As Parameters
    Dim Current = TryCast(ThisToken, FSDv2.FormatString.Common.Digits)
    Debug.Assert(Current IsNot Nothing)

Expecting_Digits:
    If EndOfText(idx, edx) Then Return Results
    If Current.Kind <> TokenKind.Digits Then
      Results.Result.Issues += Issue.Unexpected.Token(Current.Span, Current)
      idx += 1
      GoTo Expecting_Digits
    End If
    Results = Validate_ArgAlign(Current, Results)
    idx += 1
    Return Expecting_Possible_Whitespaces(idx, edx, ThisToken, Results)
  End Function

  Private Function ArgAlign_Body(
                            ByRef idx As Integer, edx As Integer,
                                  src As Token, Results As Parameters
                                ) As Parameters
    If EndOfText(idx, edx) Then Return Results
    Dim Current = TryCast(src, FSDv2.FormatString.ArgHole.Align.Body)
    Debug.Assert(Current IsNot Nothing)
    Dim idx0 = 0, edx0 = Current.InnerTokens.Count
    Results = ArgAlign_Expecting_PossibleMinusSign(idx0, edx0, Current, Results)
    idx += 1 ' : If idx >= edx Then Return Results
    Return AnyMoreTokensAreUnexpected(idx, edx, src, Results)
  End Function

  Private Function ArgAlign(
                             TheArgAlign As Align,
                             Results As Parameters
                           ) As Analyser.Parameters
    '
    ' Arg.Align::= Comma Whitespace? MinusSign? Digits Whitespaces?
    '
    Dim idx = 0, edx = TheArgAlign.InnerTokens.Count

Expecting_ArgAlign_Head:
    If EndOfText(idx, edx) Then
      'Results.Result.Issues += Issue.Unexpected.EoT(Nothing)
      Return Results
    End If

    Dim current = TheArgAlign(idx)
    Select Case current.Kind
      Case TokenKind.ArgHole_Align_Head
        Results = ArgAlign_Head(idx, edx, current, Results)
        idx += 1 : If EndOfText(idx, edx) Then Return Results
        current = TheArgAlign(idx)
        Results = ArgAlign_Body(idx, edx, TheArgAlign(idx), Results)

      Case TokenKind.ArgHole_Align_Body
        ' missing argalign_head
        Return ArgAlign_Body(idx, edx, TheArgAlign, Results)

      Case Else
        Results.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        idx += 1
        GoTo Expecting_ArgAlign_Head

    End Select
    Return Results
  End Function

End Class
