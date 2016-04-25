Public Structure ResyncPoints
  'Public Shared ReadOnly Property Empty As New ResyncPoints()
  Private _ResyncPoints As ResyncPoint() '= Array.Empty(Of ResyncPoint)

  Private Sub New(Optional ResyncPoints As IEnumerable(Of ResyncPoint) = Nothing)
    'If ResyncPoints IsNot Nothing Then
    Me._ResyncPoints = ResyncPoints.ToArray
    'End If
  End Sub

  Public Shared Operator +(Rpx As ResyncPoints, Rp As ResyncPoint) As ResyncPoints
    Return New ResyncPoints(Rpx._ResyncPoints.AsEnumerable.Concat(Enumerable.Repeat(Rp, 1)))
  End Operator

  Public Shared Function CreateNew(Rp0 As ResyncPoint, Rp1 As ResyncPoint) As ResyncPoints
    Return New ResyncPoints(Enumerable.Repeat(Rp0, 1).Concat(Enumerable.Repeat(Rp1, 1)))
  End Function

  Public Function TryToResync(ix As Source.Position) As Token
    Dim c = _ResyncPoints.Count - 1
    If c < 0 Then Return Nothing
    Dim sx = ix
    While ix.IsValid
      For i = 0 To c
        Dim q = _ResyncPoints(i).TryParse(ix)
        If q Is Nothing Then Continue For
        If q.Kind <> TokenKind.ParseError Then Return New ParseError.Resync(sx.To(ix), q)
      Next
      ix = ix.Next
    End While
    Return ParseError.Make.NullParse(ix)
  End Function

  'Public Function TryToResync(ix As Source.Position) As Token
  '  If Me._ResyncPoints.Count = 0 Then Return Nothing
  '  Dim tt = _ResyncPoints.AsParallel.Select(
  '    Function(rp As ResyncPoint)
  '      Dim _rp = rp
  '      Return Task.Run(Of Token)(Function()
  '                                  Dim lx = ix
  '                                  Dim res As Token
  '                                  While lx.IsValid
  '                                    res = _rp.TryParse(lx)
  '                                    If res IsNot Nothing Then Return res
  '                                    lx = lx.Next
  '                                  End While
  '                                  Return Nothing
  '                                End Function)
  '    End Function).ToArray
  '  Dim q = Task.WhenAll(Of Token)(tt)
  '  Dim output = tt.AsParallel.AsOrdered.FirstOrDefault(Function(rp) rp.Result IsNot Nothing)?.Result
  '  Return output
  'End Function
End Structure
