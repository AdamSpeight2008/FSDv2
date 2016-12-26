Imports System.Runtime.CompilerServices

Public Module Exts

  <DebuggerStepperBoundary>
  <Extension>
  Public Function IsNotNullParse(T As Token) As Boolean
    Return TryCast(T, ParseError)?.Why <> ParseError.Reason.Invalid
  End Function

  <DebuggerStepperBoundary>
  <Extension>
  Public Function IsKindOrIsNotNullParse(T As Token, k As TokenKind) As Boolean
    Return (T.Kind = k) OrElse T.IsNotNullParse
  End Function

  <Extension>
    Public Function AsString(Tk As Token) As String
      If Tk Is Nothing Then Return "{Nothing}"
      Dim sb As New Text.StringBuilder
      _AsString(Tk, sb, 0)
      Return sb.ToString
    End Function

  Private Sub _AsString(Tk As Token, sb As Text.StringBuilder, level As Integer)
    If TypeOf Tk Is ParseError Then
      Dim pe = DirectCast(Tk, ParseError)
      sb.AppendLine($" {Tk.Span.ToString} {Tk.Kind.ToString()}({pe.Why})")
    Else
      sb.AppendLine($" {Tk.Span.ToString} {Tk.Kind.ToString()}")

    End If

    For i = 0 To Tk.InnerTokens.Count - 1
      sb.Append(Space(level * 3))
      sb.Append($"[{i,2}]")
      _AsString(Tk(i), sb, level + 1)
    Next
  End Sub


End Module
