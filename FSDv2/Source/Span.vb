Partial Public Structure Source
  <DebuggerDisplay("({Start.Index},{Size}) =[{Me.Text}]")>
  Public Structure Span
    Public ReadOnly Property Start As Position
    Public ReadOnly Property Size As Integer

    Private Sub New(Start As Position, Size As Integer)
      Me.Start = Start : Me.Size = Size
    End Sub

    Public Shared Function Create(P As Position, Size As Integer) As Span
      Return New Span(P, Size)
    End Function
    Public Shared Function Create_ZeroSpan(p As Position) As Span
      Return New Span(p, 0)
    End Function
    Public Shared Function Create_UnitSpan(p As Position) As Span
      Return New Span(p, 1)
    End Function

    Public Function [Next]() As Source.Position
      Return Source.Position.Create(Me.Start.Source, Me.Start.Index + Me.Size)
    End Function

    Public Shared Function [From](p0 As Source.Position, p1 As Source.Position) As Span?
      If p0.Source <> p1.Source Then Return New Span?
      Return New Span(p0, p1.Index - p0.Index)
    End Function

    Public Overrides Function ToString() As String
      Return $"({Start,3}:{Size,3})"
    End Function

    Iterator Function GetChars() As IEnumerable(Of Char?)
      Dim cx = Start
      Dim fs = Me.Next
      While cx < fs
        Yield cx.Value
        cx = cx.Next
      End While
    End Function

    Public Function Text() As String
      Dim r = New String(GetChars.Where(Function(c) c.HasValue).Select(Function(c) c.Value).ToArray)
      Return r
    End Function

  End Structure

End Structure
