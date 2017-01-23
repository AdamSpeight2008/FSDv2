Public Class Tokens
  Public Shared ReadOnly Property Empty As Tokens = New Tokens()
  Private ReadOnly Property _Tokens As Token()
  Public ReadOnly Property Count As Integer

  <DebuggerStepperBoundary>
  Private Sub New(Optional Tokens As IEnumerable(Of Token) = Nothing)
    Me._Tokens = If(Tokens Is Nothing, Array.Empty(Of Token), Tokens.ToArray)
    Me.Count = _Tokens.Count
  End Sub

  <DebuggerStepperBoundary>
  Public Shared Function Create(T0 As Token, T1 As Token) As Tokens
    If (T0 Is Nothing) Then Throw New ArgumentNullException(NameOf(T0))
    If (T1 Is Nothing) Then Throw New ArgumentNullException(NameOf(T1))
    Return New Tokens(Enumerable.Repeat(T0, 1).Concat(Enumerable.Repeat(T1, 1)))
  End Function

  <DebuggerStepperBoundary>
  Public Shared Function Create(T0 As Token) As Tokens
    If (T0 Is Nothing) Then Throw New ArgumentNullException(NameOf(T0))
    Return New Tokens(Enumerable.Repeat(T0, 1))
  End Function

  <DebuggerStepperBoundary>
  Public Function GetEnumerator() As IEnumerable(Of Token)
    Return _Tokens.AsEnumerable
  End Function

  <DebuggerStepperBoundary>
  Public Function Add(Token As Token) As Tokens
    Return New Tokens(If(Token Is Nothing, _Tokens, _Tokens.Concat(Enumerable.Repeat(Token, 1))))
  End Function

  <DebuggerStepperBoundary>
  Public Function First() As Token
    Return _Tokens.FirstOrDefault
  End Function

  <DebuggerStepperBoundary>
  Public Function Last() As Token
    Return _Tokens.LastOrDefault
  End Function

#Region "Operator to help with creating Tokens"

  <DebuggerStepperBoundary>
  Public Shared Operator +(Tx As Tokens, T As Token) As Tokens
    Return New Tokens(Tx._Tokens.Concat(Enumerable.Repeat(T, 1)))
  End Operator

  <DebuggerStepperBoundary>
  Public Shared Operator +(T As Token, Tx As Tokens) As Tokens
    Return New Tokens(Enumerable.Repeat(T, 1).Concat(Tx._Tokens))
  End Operator

  <DebuggerStepperBoundary>
  Public Shared Operator +(Tx0 As Tokens, Tx1 As Tokens) As Tokens
    Return New Tokens(Tx0._Tokens.Concat(Tx1._Tokens))
  End Operator
#End Region

  Default Public ReadOnly Property Tokens(ByVal Index As Integer) As Token
    Get
      Return If( Index < 0 OrElse Index >= Count, Nothing, _Tokens(Index))
    End Get
  End Property

End Class
