Public Structure ResyncPoints
  Private _ResyncPoints As ResyncPoint()
  Public ReadOnly Property Count As Integer

  <DebuggerStepperBoundary>
  Private Sub New(Optional ResyncPoints As IEnumerable(Of ResyncPoint) = Nothing)
    Me._ResyncPoints = ResyncPoints.ToArray
    Me.Count = Me._ResyncPoints.Count
  End Sub

  <DebuggerStepperBoundary>
  Public Shared Operator +(Rpx As ResyncPoints, Rp As ResyncPoint) As ResyncPoints
    Return New ResyncPoints(Rpx._ResyncPoints.AsEnumerable.Concat(Enumerable.Repeat(Rp, 1)))
  End Operator

  <DebuggerStepperBoundary>
  Public Shared Function CreateNew(Rp0 As ResyncPoint, Rp1 As ResyncPoint) As ResyncPoints
    Return New ResyncPoints(Enumerable.Repeat(Rp0, 1).Concat(Enumerable.Repeat(Rp1, 1)))
  End Function

  '  <DebuggerStepperBoundary>
  Public Function TryToResync(ix As Source.Position, DoingResync As Boolean) As Token
    Dim sx = ix
    '   If Not DoingResync Then
    Dim c = Count - 1
    If c < 0 Then Return ParseError.Make.NullParse(ix)
    While ix.IsValid
      Dim s = If(ix = sx, 1, 0)
      For i = s To c
        Dim rp = _ResyncPoints(i)
        Dim q = rp.TryParse(ix, DoingResync)
        If q Is Nothing Then Continue For
        If q.Kind <> TokenKind.ParseError Then Return New ParseError.Resync(sx.To(ix), q)
      Next
      ix = ix.Next
      End While
    ' End If
    Return ParseError.Make.NullParse(sx)
  End Function

End Structure
