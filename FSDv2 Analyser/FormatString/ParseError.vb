Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics
Imports FSDv2.ParseError
Imports FSDv2_Analyser.Analyser.Issue

Partial Public Class Analyser

  Private Function ParseError(
                               ThisParseError As ParseError,
                         ByRef Q As Parameters) As Parameters
    Select Case ThisParseError.Why
      Case Reason.UnexpectedCharacter,
           Reason.ResyncSkipped
        Q.Result.Issues += Issue.Unexpected.Characters(ThisParseError.Span)

      Case Reason.Invalid
        Q.Result.Issues += Issue.Invalid(ThisParseError.Span)

      Case Reason.EoT
        Q.Result.Issues += Issue.Unexpected.EoT(ThisParseError.Span)

      'Case Reason.ResyncSkipped
      '  Q.Result.Issues += Issue.Resynced.Skipped(pe.Span)

      Case FSDv2.ParseError.Reason.Partial

      Case Else
        Throw New NotImplementedException($"ParseError.Why:= {ThisParseError.Why}")
    End Select
    Return Q
  End Function

End Class
