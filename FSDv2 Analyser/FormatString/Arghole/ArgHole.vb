Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser

  Private Function ArgHole_Expecting_OpeningBrace(
                                             ByRef idx As Integer,
                                                   edx As Integer,
                                                   src As Token,
                                                   Q As Parameters
                                                 ) As Parameters
Expecting_Opening_Brace:
    If idx >= edx Then
      'Q.Result.Issues += Issue.Unexpected.EoT(Nothing)
      Return Q
    End If
    Dim curr = src(idx)
    Select Case curr.Kind
      Case TokenKind.Brace_Opening
        idx += 1
        Return ArgHole_Expecting_ArgIndex(idx, edx, src, Q)

      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(curr.Span, curr)
        idx += 1
        GoTo Expecting_Opening_Brace
    End Select

    Return Q
  End Function

  Private Function ArgHole_Expecting_ArgIndex(
                                         ByRef idx As Integer,
                                               edx As Integer,
                                               src As Token,
                                               Q As Parameters
                                             ) As Parameters

Expecting_Arghole_Index:
    If idx >= edx Then
      'Q.Result.Issues += Issue.Unexpected.EoT(Nothing)
      Return Q
    End If
    Dim current = src(idx)
    Select Case current.Kind

      Case TokenKind.ArgHole_Index
        Q = ArgIndex(DirectCast(current, Index), Q)
        idx += 1
        Return ArgHole_Expecting_ArgAlign(idx, edx, src, Q)

      Case TokenKind.ArgHole_Align
        Q.Result.Issues += Issue.Arg.Index.Missing(src(idx).Span.Start.ToZeroSpan)
        Q = ArgHole_Expecting_ArgAlign(idx, edx, src, Q)
        idx += 1
        Return ArgHole_Expecting_ArgFormat(idx, edx, src, Q)

      Case TokenKind.ArgHole_Format
        Q.Result.Issues += Issue.Arg.Index.Missing(src(idx).Span.Start.ToZeroSpan)
        Return ArgHole_Expecting_ArgFormat(idx, edx, src, Q)

      Case TokenKind.Brace_Closing
        Q.Result.Issues += Issue.Arg.Index.Missing(src(idx).Span.Start.ToZeroSpan)
        Return ArgHole_Expecting_ClosingBrace(idx, edx, src, Q)

      Case TokenKind.ParseError
        Dim pe = DirectCast(current, ParseError)
        Select Case pe.Why

          Case FSDv2.ParseError.Reason.EoT
            Q.Result.Issues += Issue.Arg.Index.Missing(pe.Span.Start.ToZeroSpan) + Issue.Missing.ClosingBrace(pe.Span.Start.ToZeroSpan)
            idx += 1
            Return ArgHole_Completed(idx, edx, src, Q)

          Case FSDv2.ParseError.Reason.Partial
            Select Case pe(0).Kind
              Case TokenKind.Brace_Closing
                Q.Result.Issues += Issue.Arg.Index.Missing(pe(0).Span.Start.ToZeroSpan)
                idx += 1
                Return ArgHole_Expecting_ClosingBrace(idx, edx, src, Q)
              Case Else
                Q = ParseError(pe, Q)
            End Select

          Case FSDv2.ParseError.Reason.NullParse
            If pe.InnerTokens.Count > 0 Then
              Dim pe0 = TryCast(pe(0), ParseError)
              If (pe0 IsNot Nothing) AndAlso (pe0.Why = FSDv2.ParseError.Reason.UnexpectedCharacter) Then
                Q.Result.Issues += Issue.Unexpected.Characters(pe0.Span)
              End If
            End If
            Q.Result.Issues += Issue.Arg.Index.Missing(pe.Span.Start.ToZeroSpan)
            idx += 1
            Return ArgHole_Expecting_ArgAlign(idx, edx, src, Q)

          Case Else
            Q = ParseError(pe, Q)

        End Select

      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        GoTo Expecting_Arghole_Index
    End Select
    Return Q
  End Function

  Private Function ArgHole_Expecting_ArgAlign(ByRef idx As Integer, edx As Integer, src As Token, Q As Parameters) As Parameters
Expection_Arghole_Align:
    If idx >= edx Then
      '
      'Q.Result.Issues += Issue.Unexpected.EoT(Nothing)
      Return Q
    End If
    Dim current = src(idx)

    Select Case current.Kind
      Case TokenKind.ArgHole_Align
        Q = ArgAlign(DirectCast(current, FormatString.ArgHole.Align), Q)
        idx += 1
        Return ArgHole_Expecting_ArgFormat(idx, edx, src, Q)

      Case TokenKind.Brace_Closing
        Return ArgHole_Expecting_ClosingBrace(idx, edx, src, Q)

      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        GoTo Expection_Arghole_Align

    End Select
    Return Q
  End Function

  Private Function ArgHole_Expecting_ArgFormat(ByRef idx As Integer, edx As Integer, src As Token, Q As Parameters) As Parameters
Expecting_Arghole_Format:
    If idx >= edx Then
      'Q.Result.Issues += Issue.Unexpected.EoT(Nothing)
      Return Q
    End If
    Dim current = src(idx)
    Select Case current.Kind
      Case TokenKind.ArgHole_Format
        Q = ArgFormat(DirectCast(current, FSDv2.FormatString.ArgHole.Format), Q)
        idx += 1
        Return ArgHole_Expecting_ClosingBrace(idx, edx, src, Q)

      Case TokenKind.Brace_Closing
        Return ArgHole_Expecting_ClosingBrace(idx, edx, src, Q)

      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        GoTo Expecting_Arghole_Format
    End Select
    Return Q
  End Function

  Private Function ArgHole_Expecting_ClosingBrace(ByRef idx As Integer, edx As Integer, src As Token, Q As Parameters) As Parameters
Expecting_Closing_Brace:
    If idx >= edx Then
      'Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_End, Nothing)
      Return Q
    End If
    Dim current = src(idx)
    Select Case current.Kind
      Case TokenKind.Brace_Closing
        idx += 1
        Return ArgHole_Completed(idx, edx, src, Q)
      Case Else
        Dim pe = TryCast(Current, ParseError)
        If pe IsNot Nothing AndAlso pe(0).Kind = TokenKind.Brace_Closing Then
          GoTo Expecting_Closing_Brace
        End If
        Q.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        GoTo Expecting_Closing_Brace
    End Select
    Return Q
  End Function

  Private Function ArgHole_Completed(ByRef idx As Integer, edx As Integer, src As Token, Q As Parameters) As Parameters
    While idx < edx
      Dim current = src(idx)
      Q.Result.Issues += Issue.Unexpected.Token(current.Span, current)
      idx += 1
    End While
    Return Q
  End Function
  Private Function ArgHole(AH As FormatString.ArgHole, Q As Parameters) As Parameters
    Dim idx = 0, edx = AH.InnerTokens.Count
    Q = ArgHole_Expecting_OpeningBrace(idx, edx, AH, Q)
    Q = ArgHole_Completed(idx, edx, AH, Q)
state_end:
    Return Q
  End Function

End Class
