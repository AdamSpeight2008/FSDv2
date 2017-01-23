Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser

  Private Function Validate_ArgIndex(tkn As Token, Results As Parameters) As Parameters

    Dim Digits = TryCast(tkn, FSDv2.FormatString.Common.Digits)
    If Digits Is Nothing Then

    Else
      Dim ValueOfArgIndex = Digits.GetValue
      Results.Arg = If(Results.Arg, New Arg())
      Results.Arg.Index = ValueOfArgIndex
      If Results.Arg.Index.HasValue = False Then
        Results.Result.Issues += Issue.Arg.Index.Missing(tkn.Span)
      Else
        If Results.Arg.Index.Value >= Framework.UpperLimit Then
          Results.Result.Issues += Issue.Arg.Index.Framework.Lower_Limit_Exceeded(tkn.Span)
        ElseIf Results.Arg.Index.Value >= Results.Args.Count Then
          Results.Result.Issues += Issue.Arg.Index.OutOfRange(tkn.Span)
        Else
          Results.Args.MarkAsUsed(Results.Arg.Index.Value)

        End If
      End If
    End If
    Return Results
  End Function

  Private Function Expecting_Digits(
                               ByRef idx As Integer,
                                     edx As Integer,
                                     src As Index,
                                 results As Parameters
                                   ) As Parameters
Expecting_Digits:
    If idx >= edx Then
      ''Results.Result.Issues += New Issue(Issue.Kinds.Unexpected_End, Nothing)
      'Return results
    Else
      If src(idx).Kind <> TokenKind.Digits Then
        ' An easy mistake to make is to have whitespaces after the opening brace.
        ' Eg: { 0}
        results.Result.Issues += Issue.Unexpected.Token(src(idx).Span, src(idx))
        idx += 1
        GoTo Expecting_Digits
      End If
      results = Validate_ArgIndex(src(idx), results)
    End If
    Return results
  End Function


  Private Function Expecting_Possible_Whitespaces(
                                                 ByRef idx As Integer,
                                                 edx As Integer,
                                                 src As Index,
                                                 Results As Parameters
                                                 ) As Parameters
    idx += 1
    If idx < edx Then idx += If(src(idx).Kind = TokenKind.Whitespaces, 1, 0)
    Return Results
  End Function

  Private Function ArgIndex(src As Index, Results As Parameters) As Analyser.Parameters
    '
    ' Arg.Index ::= Digits Whitespaces?
    '
    Dim idx = 0, edx = src.InnerTokens.Count
    Results = Expecting_Digits(idx, edx, src, Results)
    Results = Expecting_Possible_Whitespaces(idx, edx, src, Results)
    Results = AnyMoreTokensAreUnexpected(idx, edx, src, Results)
    Return Results
  End Function

  Private Function AnyMoreTokensAreUnexpected(
                                         ByRef idx As Integer, edx As Integer,
                                               src As Token, Results As Parameters
                                             ) As Parameters
    While idx < edx
      Results.Result.Issues += Issue.Unexpected.Token(src(idx).Span, src(idx))
      idx += 1
    End While
    Return Results
  End Function

End Class
