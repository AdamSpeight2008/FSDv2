﻿Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser

  Private Function Expecting_Colon(
                              ByRef idx As Integer,
                                    edx As Integer,
                                    src As Token,
                                Results As Parameters
                                  ) As Parameters
Expecting_Colon:
    If idx >= edx Then
      Return Results
    End If
    Dim Current = src(idx)
    If Current.Kind <> TokenKind.Colon Then
      Results.Result.Issues += Issue.Unexpected.Token(Current.Span, Current)
      idx += 1
      GoTo Expecting_Colon
    End If
    Return Results
  End Function

  Private Function Validate_ArgFormatBody(
                                     ByRef idx As Integer,
                                           edx As Integer,
                                           src As Format,
                                       Results As Parameters
                                         ) As Parameters
    While idx < edx
      Dim Current = src(idx)
      Select Case Current.Kind
        Case TokenKind.Esc_Brace_Closing,
             TokenKind.Esc_Brace_Opening,
             TokenKind.Text
          ' These are valid within an ArgFormat
        Case TokenKind.Brace_Opening
          Results.Result.Issues += New Issue(Issue.Kinds.Invalid, Current.Span, "Opening Brace is not allowed with the ArgFormat.")
        Case Else
          Results.Result.Issues += Issue.Unexpected.Token(Current.Span, Current)
      End Select
      idx += 1
    End While
    Return Results
  End Function


  Private Function ArgFormat(TheArgFormat As FormatString.ArgHole.Format, Results As Parameters) As Parameters
    Dim idx = 0, edx = TheArgFormat.InnerTokens.Count
    Results = Expecting_Colon(idx, edx, TheArgFormat, Results)
    idx += 1
    Results = Validate_ArgFormatBody(idx, edx, TheArgFormat, Results)
    Return Results
  End Function

End Class
