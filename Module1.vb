Imports FSDv2
Imports System.Runtime.CompilerServices


Module Module1

  Sub Main()
    '           0         1         2         3         4         5         6         7         8
    '           012345678901234567890123456789012345678901234567890123456789012345678901234567890
    Dim Text = "{}"
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard)
    Dim Ix = TheSource.First
    'Dim sw = Diagnostics.Stopwatch.StartNew
    Dim ParseResult = FormatString.TryParse(Ix)
    Console.WriteLine($"Input:=[{Text}]")
    Dim Actual = ParseResult.AsString
    Console.WriteLine($"Output:= {Actual}")
    'sw.Stop()
    '    Console.WriteLine(sw.Elapsed.TotalMilliseconds.ToString)

  End Sub

End Module


Public Module Exts
  <Extension>
  Public Function AsString(Tk As Token) As String
    If Tk Is Nothing Then Return "{Nothing}"
    Dim sb As New Text.StringBuilder
    _AsString(Tk, sb, 0)
    Return sb.ToString
  End Function

  Private Sub _AsString(Tk As Token, sb As Text.StringBuilder, level As Integer)
    sb.AppendLine($"{Space(level * 2)}{Tk.Span.ToString} {Tk.GetType.FullName}")
    For i = 0 To Tk.InnerTokens.Count - 1
      sb.Append(Space(level * 2))
      sb.Append($"[{i,2}]")
      _AsString(Tk(i), sb, level + 1)
    Next
  End Sub

End Module
