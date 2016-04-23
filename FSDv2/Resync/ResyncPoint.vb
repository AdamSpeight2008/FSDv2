﻿
Public Structure ResyncPoint
  Public ReadOnly Property TryParse As Func(Of Source.Position, Token)

  Public Sub New(TryParse As Func(Of Source.Position, Token))
    Me.TryParse = TryParse
  End Sub

  Public Shared Operator +(Rp0 As ResyncPoint, Rp1 As ResyncPoint) As ResyncPoints
    Return ResyncPoints.CreateNew(Rp0, Rp1)
  End Operator
End Structure