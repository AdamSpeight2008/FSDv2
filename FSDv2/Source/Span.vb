﻿Partial Public Structure Source

  <DebuggerDisplay("({Start.Index},{Size}) =[{Me.Text}]")>
  Public Structure Span

#Region "ReadOnly Properties"
    Public ReadOnly Property Start As Position
    Public ReadOnly Property Size As Integer
#End Region


    Friend Sub New(Start As Position, Size As Integer)
      Me.Start = Start : Me.Size = Size
    End Sub

#Region "Creators"
    Public Shared Function Create(P As Position, Size As Integer) As Span
      Return New Span(P, Size)
    End Function
    Public Shared Function Create_ZeroSpan(p As Position) As Span
      Return New Span(p, 0)
    End Function
    Public Shared Function Create_UnitSpan(p As Position) As Span
      Return New Span(p, 1)
    End Function
#End Region

    Public Function [Next]() As Source.Position
      Return Source.Position.Create(Me.Start.Source, Me.Start.Index + Me.Size)
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
      Return New String(GetChars.Where(Function(c) c.HasValue).Select(Function(c) c.Value).ToArray)
    End Function

  End Structure

End Structure
