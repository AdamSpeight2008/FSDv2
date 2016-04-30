Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser

  Private Function ParseError(pe As ParseError, ByRef Q As Parameters) As Parameters
    Select Case pe.Why
      Case FSDv2.ParseError.Reason.UnexpectedCharacter : Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Characters, pe.Span)
      Case FSDv2.ParseError.Reason.Invalid : Q.Result.Issues += New Issue(Issue.Kinds.Invalid, pe.Span)
      Case FSDv2.ParseError.Reason.EoT : Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_End, pe.Span)

      Case FSDv2.ParseError.Reason.Partial

      Case Else
        Throw New NotImplementedException($"ParseError.Why:= {pe.Why}")
    End Select
    Return Q
  End Function

End Class
