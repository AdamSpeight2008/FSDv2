Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics
Imports FSDv2.ParseError
Imports FSDv2_Analyser.Analyser.Issue

Partial Public Class Analyser

  Private Function ParseError(pe As ParseError, ByRef Q As Parameters) As Parameters
    Select Case pe.Why
      Case Reason.UnexpectedCharacter, Reason.ResyncSkipped
        Q.Result.Issues += Issue.Unexpected.Characters(pe.Span)

      Case Reason.Invalid
        Q.Result.Issues += Issue.Invalid(pe.Span)

      Case Reason.EoT
        Q.Result.Issues += Issue.Unexpected.EoT(pe.Span)

      'Case Reason.ResyncSkipped
      '  Q.Result.Issues += Issue.Resynced.Skipped(pe.Span)

      Case FSDv2.ParseError.Reason.Partial

      Case Else
        Throw New NotImplementedException($"ParseError.Why:= {pe.Why}")
    End Select
    Return Q
  End Function

End Class
