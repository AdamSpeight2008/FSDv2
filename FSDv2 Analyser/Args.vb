Imports System.Numerics

Partial Public Class Analyser

  Public Class Args
    Public ReadOnly Property Count As Integer
    Private _Used As New Generic.HashSet(Of BigInteger)

    Public Sub MarkAsUsed(ai As BigInteger?)
      If ai.HasValue = False Then Exit Sub
      _Used.Add(ai.Value)
    End Sub

  End Class

End Class
