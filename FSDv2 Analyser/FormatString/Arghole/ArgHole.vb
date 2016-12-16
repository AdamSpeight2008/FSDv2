Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser

  Private Function ArgHole(AH As FormatString.ArgHole, Q As Parameters) As Parameters
    Dim en = AH.InnerTokens.GetEnumerator.GetEnumerator

Expecting_Opening_Brace:
    If en.MoveNext = False Then Q.Result.Issues += Issue.Unexpected.EoT(Nothing) : GoTo state_end
    Select Case en.Current.Kind
      Case TokenKind.Brace_Opening
      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(en.Current.Span)
        GoTo Expecting_Opening_Brace
    End Select

Expecting_Arghole_Index:
    If en.MoveNext = False Then Q.Result.Issues += Issue.Unexpected.EoT(Nothing) : GoTo state_end

    Select Case en.Current.Kind
      Case TokenKind.ArgHole_Index : Q = ArgIndex(DirectCast(en.Current, Index), Q) : GoTo Expection_Arghole_Align
      Case TokenKind.ArgHole_Align : GoTo Expection_Arghole_Align_1
      Case TokenKind.ArgHole_Format : GoTo Expecting_Arghole_Format_1
      Case TokenKind.Brace_Closing
        Q.Result.Issues += Issue.Arg.Index.Missing(Nothing)
        GoTo Expecting_Closing_Brace_1
      Case TokenKind.ParseError
        Dim pe = DirectCast(en.Current, ParseError)
        Select Case pe.Why

          Case FSDv2.ParseError.Reason.EoT
            Q.Result.Issues += Issue.Arg.Index.Missing(pe.Span.Start.ToZeroSpan) + New Issue(Issue.Kinds.Missing_Closing_Brace, pe.Span.Start.ToZeroSpan)
            GoTo Expecting_Done

          Case FSDv2.ParseError.Reason.Partial
            Select Case pe(0).Kind
              Case TokenKind.Brace_Closing
                Q.Result.Issues += Issue.Arg.Index.Missing(pe(0).Span.Start.ToZeroSpan)
                GoTo Expecting_Closing_Brace
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
            GoTo Expecting_Closing_Brace

          Case Else
            Q = ParseError(pe, Q)

        End Select

      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(en.Current.Span)
        GoTo Expecting_Arghole_Index
    End Select

Expection_Arghole_Align:
    If en.MoveNext = False Then Q.Result.Issues += Issue.Unexpected.EoT(Nothing) : GoTo state_end
Expection_Arghole_Align_1:
    Select Case en.Current.Kind
      Case TokenKind.ArgHole_Align : Q = ArgAlign(DirectCast(en.Current, FormatString.ArgHole.Align), Q) : GoTo Expecting_Closing_Brace
      Case TokenKind.Brace_Closing : GoTo Expecting_Closing_Brace_1
      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(en.Current.Span)
        GoTo Expecting_Closing_Brace
    End Select

Expecting_Arghole_Format:
    If en.MoveNext = False Then Q.Result.Issues += Issue.Unexpected.EoT(Nothing) : GoTo state_end
Expecting_Arghole_Format_1:
    Select Case en.Current.Kind
      Case TokenKind.ArgHole_Format : Q = ArgFormat(DirectCast(en.Current, FSDv2.FormatString.ArgHole.Format), Q) : GoTo Expecting_Closing_Brace
      Case TokenKind.Brace_Closing : GoTo Expecting_Closing_Brace_1
      Case Else
        Q.Result.Issues += Issue.Unexpected.Token(en.Current.Span)
        GoTo Expecting_Arghole_Format
    End Select

Expecting_Closing_Brace:
    If en.MoveNext = False Then Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_End, Nothing) : GoTo state_end
Expecting_Closing_Brace_1:
    Select Case en.Current.Kind
      Case TokenKind.Brace_Closing : GoTo Expecting_Done
      Case Else
        Dim pe = TryCast(en.Current, ParseError)
        If pe IsNot Nothing AndAlso pe(0).Kind = TokenKind.Brace_Closing Then
          GoTo Expecting_Closing_Brace
        End If
        Q.Result.Issues += Issue.Unexpected.Token(en.Current.Span)
        GoTo Expecting_Closing_Brace
    End Select

Expecting_Done:
    While en.MoveNext
      Q.Result.Issues += Issue.Unexpected.Token(en.Current.Span)
    End While
state_end:
    Return Q
  End Function

End Class
