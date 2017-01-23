Public Structure ResyncPoints
  Private _ResyncPoints As Func(Of Source.Position?, Boolean, Token)()
  Public ReadOnly Property _Count As Integer

  <DebuggerStepperBoundary>
  Private Sub New(ResyncPoints As Func(Of Source.Position?, Boolean, Token)())
    _ResyncPoints = ResyncPoints
    _Count = _ResyncPoints.Count
  End Sub

  <DebuggerStepperBoundary>
  Public Shared Function CreateNew(ParamArray Rpx As Func(Of Source.Position?, Boolean, Token)()) As ResyncPoints
    Debug.Assert(Rpx IsNot Nothing)
    Debug.Assert(Rpx.Count > 0)
    Return New ResyncPoints(Rpx)
  End Function

  '  <DebuggerStepperBoundary>
  Public Function TryToResync(ix As Source.Position?, DoingResync As Boolean) As Token
    Dim sx = ix
    'If Not DoingResync Then
    Dim c = _Count - 1
    If c < 0 Then Return ParseError.Make.NullParse(ix)
    While ix?.IsValid
      Dim s = If(ix = sx, 1, 0)
      For i = s To c
        Dim rp = _ResyncPoints(i)
        Dim q = rp(ix, DoingResync)
        If q Is Nothing Then Continue For
        If q.Kind <> TokenKind.ParseError Then Return New ParseError.Resync(sx?.To(ix), q)
      Next
      ix = ix?.Next
    End While
    'End If
    Return ParseError.Make.NullParse(sx)
  End Function

End Structure
