Public Class Tokens
  Public Shared ReadOnly Property Empty As Tokens = New Tokens()
  Private ReadOnly Property _Tokens As Token()
  Public ReadOnly Property Count As Integer

  Private Sub New(Optional Tokens As IEnumerable(Of Token) = Nothing)
    If Tokens Is Nothing Then Me._Tokens = Array.Empty(Of Token) Else Me._Tokens = Tokens.ToArray
    Me.Count = _Tokens.Count
  End Sub

  Public Shared Function Create(T0 As Token, T1 As Token) As Tokens
    If (T0 Is Nothing) OrElse (T1 Is Nothing) Then Throw New Exception
    Return New Tokens(Enumerable.Repeat(T0, 1).Concat(Enumerable.Repeat(T1, 1)))
  End Function


  Public Function GetEnumerator() As IEnumerable(Of Token)
    Return _Tokens.AsEnumerable
  End Function

  Public Function Add(Token As Token) As Tokens
    If Token Is Nothing Then Return New Tokens(_Tokens) Else Return New Tokens(_Tokens.Concat(Enumerable.Repeat(Token, 1)))
  End Function

  Public Function First() As Token
    Return _Tokens.FirstOrDefault
  End Function
  Public Function Last() As Token
    Return _Tokens.LastOrDefault
  End Function

  Public Shared Operator +(Tx As Tokens, T As Token) As Tokens
    Return New Tokens(Tx._Tokens.Concat(Enumerable.Repeat(T, 1)))
  End Operator

  Public Shared Operator +(T As Token, Tx As Tokens) As Tokens
    Return New Tokens(Enumerable.Repeat(T, 1).Concat(Tx._Tokens))
  End Operator
  Public Shared Operator +(Tx0 As Tokens, Tx1 As Tokens) As Tokens
    Return New Tokens(Tx0._Tokens.Concat(Tx1._Tokens))
  End Operator

  Default Public ReadOnly Property Tokens(ByVal Index As Integer) As Token
    Get
      If Index < 0 OrElse Index >= Count Then Return Nothing
      Return _Tokens(Index)
    End Get
  End Property
End Class
