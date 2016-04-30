Partial Class Analyser

  Public Class Issues
    Public Shared ReadOnly Property Empty() As New Issues(Nothing)
    Public ReadOnly Property Issues As Issue()
    Private Sub New(Issues As IEnumerable(Of Issue))
      Me.Issues = If(Issues Is Nothing, Array.Empty(Of Issue), Issues.ToArray)
    End Sub

    Public Shared Operator +(I As Issue, Ix As Issues) As Issues
      Return New Issues(Enumerable.Repeat(I, 1).Concat(Ix.Issues.AsEnumerable))
    End Operator

    Public Shared Operator +(Ix As Issues, I As Issue) As Issues
      Return New Issues(Ix.Issues.AsEnumerable.Concat(Enumerable.Repeat(I, 1)))
    End Operator

    Public Shared Operator +(Ix0 As Issues, Ix1 As Issues) As Issues
      Return New Issues(Ix0.Issues.AsEnumerable.Concat(Ix1.Issues.AsEnumerable))
    End Operator

  End Class

End Class