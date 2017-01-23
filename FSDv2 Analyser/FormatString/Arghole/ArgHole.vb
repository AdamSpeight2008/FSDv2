Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser

  Private Function ArgHole_Expecting_OpeningBrace(
                                             ByRef idx As Integer,
                                                   edx As Integer,
                                                   src As Token,
                                                   Results As Parameters
                                                 ) As Parameters
Expecting_Opening_Brace:
    If idx >= edx Then Return Results

    Dim curr = src(idx)
    Select Case curr.Kind
      Case TokenKind.Brace_Opening
        idx += 1
        Return ArgHole_Expecting_ArgIndex(idx, edx, src, Results)

      Case Else
        Results.Result.Issues += Issue.Unexpected.Token(curr.Span, curr)
        idx += 1
        GoTo Expecting_Opening_Brace
    End Select

    Return Results
  End Function

  Private Function ArgHole_Expecting_ArgIndex(
                                         ByRef idx As Integer,
                                               edx As Integer,
                                               src As Token,
                                               Results As Parameters
                                             ) As Parameters

Expecting_Arghole_Index:
    If idx >= edx Then Return Results
    Dim current = src(idx)
    Select Case current.Kind

      Case TokenKind.ArgHole_Index
        Results = ArgIndex(DirectCast(current, Index), Results)
        idx += 1
        Return ArgHole_Expecting_ArgAlign(idx, edx, src, Results)

      Case TokenKind.ArgHole_Align
        Results.Result.Issues += Issue.Arg.Index.Missing(src(idx).Span?.Start?.ToZeroSpan)
        Results = ArgHole_Expecting_ArgAlign(idx, edx, src, Results)
        idx += 1
        Return ArgHole_Expecting_ArgFormat(idx, edx, src, Results)

      Case TokenKind.ArgHole_Format
        Results.Result.Issues += Issue.Arg.Index.Missing(src(idx).Span?.Start?.ToZeroSpan)
        Return ArgHole_Expecting_ArgFormat(idx, edx, src, Results)

      Case TokenKind.Brace_Closing
        Results.Result.Issues += Issue.Arg.Index.Missing(src(idx).Span?.Start?.ToZeroSpan)
        Return ArgHole_Expecting_ClosingBrace(idx, edx, src, Results)

      Case TokenKind.ParseError
        Dim TheParseError = DirectCast(current, ParseError)
        Select Case TheParseError.Why

          Case FSDv2.ParseError.Reason.EoT
            Results.Result.Issues += Issue.Arg.Index.Missing(TheParseError.Span?.Start?.ToZeroSpan) +
                                     Issue.Missing.ClosingBrace(TheParseError.Span?.Start?.ToZeroSpan)
            idx += 1
            Return ArgHole_Completed(idx, edx, src, Results)

          Case FSDv2.ParseError.Reason.Partial
            Select Case TheParseError(0).Kind
              Case TokenKind.Brace_Closing
                Results.Result.Issues += Issue.Arg.Index.Missing(TheParseError(0).Span?.Start?.ToZeroSpan)
                idx += 1
                Return ArgHole_Expecting_ClosingBrace(idx, edx, src, Results)
              Case Else
                Results = ParseError(TheParseError, Results)
            End Select

          Case FSDv2.ParseError.Reason.NullParse
            If TheParseError.InnerTokens.Count > 0 Then
              Dim pe0 = TryCast(TheParseError(0), ParseError)
              If (pe0 IsNot Nothing) AndAlso (pe0.Why = FSDv2.ParseError.Reason.UnexpectedCharacter) Then
                Results.Result.Issues += Issue.Unexpected.Characters(pe0.Span)
              End If
            End If
            Results.Result.Issues += Issue.Arg.Index.Missing(TheParseError.Span?.Start?.ToZeroSpan)
            idx += 1
            Return ArgHole_Expecting_ArgAlign(idx, edx, src, Results)

          Case Else
            Results = ParseError(TheParseError, Results)
            idx += 1
            GoTo Expecting_Arghole_Index 
        End Select

      Case Else
        Results.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        GoTo Expecting_Arghole_Index
    End Select
    Return Results
  End Function

  Private Function ArgHole_Expecting_ArgAlign(
                                         ByRef idx As Integer,
                                               edx As Integer,
                                               src As Token,
                                               Results As Parameters
                                             ) As Parameters
Expection_Arghole_Align:
    If idx >= edx Then Return Results

    Dim current = src(idx)

    Select Case current.Kind
      Case TokenKind.ArgHole_Align
        Results = ArgAlign(DirectCast(current, FormatString.ArgHole.Align), Results)
        idx += 1
        Return ArgHole_Expecting_ArgFormat(idx, edx, src, Results)

      Case TokenKind.Brace_Closing
        Return ArgHole_Expecting_ClosingBrace(idx, edx, src, Results)

      Case Else
        Results.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        GoTo Expection_Arghole_Align

    End Select
    Return Results
  End Function

  Private Function ArgHole_Expecting_ArgFormat(
                                          ByRef idx As Integer,
                                                edx As Integer,
                                                src As Token,
                                                Results As Parameters
                                              ) As Parameters
Expecting_Arghole_Format:
    If idx >= edx Then Return Results
    Dim current = src(idx)
    Select Case current.Kind
      Case TokenKind.ArgHole_Format
        Results = ArgFormat(DirectCast(current, FSDv2.FormatString.ArgHole.Format), Results)
        idx += 1
        Return ArgHole_Expecting_ClosingBrace(idx, edx, src, Results)

      Case TokenKind.Brace_Closing
        Return ArgHole_Expecting_ClosingBrace(idx, edx, src, Results)

      Case Else
        Results.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        GoTo Expecting_Arghole_Format
    End Select
    Return Results
  End Function

  Private Function ArgHole_Expecting_ClosingBrace(
                                             ByRef idx As Integer,
                                                   edx As Integer,
                                                   src As Token,
                                                   Results As Parameters
                                                 ) As Parameters
Expecting_Closing_Brace:
    If idx >= edx Then
      'Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_End, Nothing)
      Return Results
    End If
    Dim current = src(idx)
    Select Case current.Kind

      Case TokenKind.Brace_Closing
        idx += 1
        Return ArgHole_Completed(idx, edx, src, Results)

      Case Else
        Dim TheParseError = TryCast(current, ParseError)
        If TheParseError?(0).Kind = TokenKind.Brace_Closing Then GoTo Expecting_Closing_Brace
        Results.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        GoTo Expecting_Closing_Brace
    End Select
    Return Results
  End Function

  Private Function ArgHole_Completed(
                                ByRef idx As Integer,
                                      edx As Integer,
                                      src As Token,
                                      Results As Parameters
                                    ) As Parameters
    While idx < edx
      Dim current = src(idx)
      Results.Result.Issues += Issue.Unexpected.Token(current.Span, current)
      idx += 1
    End While
    Return Results
  End Function

  Private Function ArgHole(
                            TheArgHole As FormatString.ArgHole,
                            Results As Parameters
                          ) As Parameters
    Dim idx = 0, edx = TheArgHole.InnerTokens.Count
    Results = ArgHole_Expecting_OpeningBrace(idx, edx, TheArgHole, Results)
    Results = ArgHole_Completed(idx, edx, TheArgHole, Results)
    Return Results
  End Function

End Class
