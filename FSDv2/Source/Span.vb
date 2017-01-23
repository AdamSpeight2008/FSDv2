Partial Public Class Source

  <DebuggerDisplay("({Start.Index},{Size}) =[{Me.Text}]")>
  Public Structure Span

#Region "ReadOnly Properties"
    Public ReadOnly Property Start As Position?
    Public ReadOnly Property Size As Integer
#End Region


    <DebuggerStepperBoundary>
    Friend Sub New(Start As Position?, Size As Integer)
      Me.Start = Start : Me.Size = Size
    End Sub

#Region "Creators"
    <DebuggerStepperBoundary>
    Public Shared Function Create(P As Position, Size As Integer) As Span
      Return New Span(P, Size)
    End Function
    <DebuggerStepperBoundary>
    Public Shared Function Create_ZeroSpan(p As Position?) As Span
      Return New Span(p, 0)
    End Function
    <DebuggerStepperBoundary>
    Public Shared Function Create_UnitSpan(p As Position?) As Span
      Return New Span(p, 1)
    End Function
#End Region

    Public Function [Next]() As Source.Position
      Dim idx = Me.Start?.Index + Me.Size
      Return Source.Position.Create(Me.Start?.Src, If(idx.HasValue, idx.Value, 0))
    End Function

    <DebuggerStepperBoundary>
    Public Overrides Function ToString() As String
      Return $"({Start,3}:{Size,3})"
    End Function

    <DebuggerStepperBoundary>
    Iterator Function GetChars() As IEnumerable(Of Char?)
      Dim cx = Start
      Dim fs = Me.Next
      While cx < fs
        Yield cx?.Value
        cx = cx?.Next
      End While
    End Function

    <DebuggerStepperBoundary>
    Public Function Text() As String
      Return New String(GetChars.Where(Function(c) c.HasValue).Select(Function(c) c.Value).ToArray)
    End Function

  End Structure

End Class
