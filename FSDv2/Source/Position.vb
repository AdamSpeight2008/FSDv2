Partial Public Class Source

  <DebuggerDisplay("{Index}=[{Value}]")>
  Public Structure Position
    Public ReadOnly Property Src As Source
    Public ReadOnly Property Index As Integer
    Public ReadOnly Property IsValid As Boolean
    Public ReadOnly Property IsInvalid As Boolean

    <DebuggerStepperBoundary>
    Private Sub New(Source As Source, Index As Integer)
      Me.Src = Source : Me.Index = Bound(Index, 0, Source.Length) : Me.IsValid = IsWithin(Index, 0, Source.Length) : Me.IsInvalid = Not IsValid
    End Sub

    <DebuggerStepperBoundary>
    Public Function Value() As Char?
      Return Src(Index)
    End Function

    <DebuggerStepperBoundary>
    Private Function IsWithin(Value As Integer, Limit0 As Integer, Limit1 As Integer) As Boolean
      Return (Limit0 <= Value) AndAlso (Value < Limit1)
    End Function

    <DebuggerStepperBoundary>
    Private Shared Function Bound(value As Integer, Limit0 As Integer, Limit1 As Integer) As Integer
      If value < Limit0 Then
        If Limit0 = Integer.MinValue Then Return Integer.MinValue Else Return Limit0 - 1
      ElseIf value > Limit1 Then
        If Limit1 = Integer.MaxValue Then Return Integer.MaxValue Else Return Limit1
      Else
        Return value
      End If
    End Function

    <DebuggerStepperBoundary>
    Public Function [Next]() As Position
      Return New Position(Me.Src, Index + 1)
    End Function

    <DebuggerStepperBoundary>
    Public Function AddOffset(Offset As Integer) As Position
      Return New Position(Me.Src, Index + Offset)
    End Function

    <DebuggerStepperBoundary>
    Public Function [To](p As Position?) As Span?
      Return If(Src = p?.Src, New Span(Me, CInt(p?.Index - Index)), Nothing)
    End Function

#Region "(Position, Char) : Boolean Operators"
    <DebuggerStepperBoundary>
    Shared Operator =(ByVal P As Position?, Ch As Char) As Boolean
      Return CBool(Not String.IsNullOrEmpty(P?.Src.Text) AndAlso P?.Value.HasValue AndAlso (P?.Value.Value = Ch))
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator >(ByVal P As Position?, Ch As Char) As Boolean
      Return CBool(Not String.IsNullOrEmpty(P?.Src.Text) AndAlso P?.Value.HasValue AndAlso (P?.Value.Value > Ch))
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator <(ByVal P As Position?, Ch As Char) As Boolean
      Return CBool(Not String.IsNullOrEmpty(P?.Src.Text) AndAlso P?.Value.HasValue AndAlso (P?.Value.Value < Ch))
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator <=(ByVal P As Position?, Ch As Char) As Boolean
      Return CBool(Not String.IsNullOrEmpty(P?.Src.Text) AndAlso P?.Value.HasValue AndAlso (P?.Value.Value <= Ch))
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator >=(ByVal P As Position?, Ch As Char) As Boolean
      Return CBool(Not String.IsNullOrEmpty(P?.Src.Text) AndAlso P?.Value.HasValue AndAlso (P?.Value.Value >= Ch))
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator <>(ByVal P As Position?, Ch As Char) As Boolean
      Return CBool(Not String.IsNullOrEmpty(P?.Src.Text) AndAlso P?.Value.HasValue AndAlso (P?.Value.Value <> Ch))
    End Operator
#End Region

#Region "(Char, Position) : Boolean Operators"
    <DebuggerStepperBoundary>
    Public Shared Operator =(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Src.Text) AndAlso P.Value.HasValue AndAlso Ch = P.Value.Value
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator >(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Src.Text) AndAlso P.Value.HasValue AndAlso Ch > P.Value.Value
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator <(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Src.Text) AndAlso P.Value.HasValue AndAlso Ch < P.Value.Value
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator <=(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Src.Text) AndAlso P.Value.HasValue AndAlso Ch <= P.Value.Value
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator >=(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Src.Text) AndAlso P.Value.HasValue AndAlso Ch >= P.Value.Value
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator <>(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Src.Text) AndAlso P.Value.HasValue AndAlso Ch <> P.Value.Value
    End Operator
#End Region

#Region "(Position, Position) : Boolean Operators"
    <DebuggerStepperBoundary>
    Public Shared Operator =(P0 As Position, P1 As Position) As Boolean
      Return (P0.Src = P1.Src) AndAlso P0.Index = P1.Index
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator <>(P0 As Position, P1 As Position) As Boolean
      Return (P0.Src = P1.Src) AndAlso P0.Index <> P1.Index
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator >=(P0 As Position, P1 As Position) As Boolean
      Return (P0.Src = P1.Src) AndAlso P0.Index >= P1.Index
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator <=(P0 As Position, P1 As Position) As Boolean
      Return (P0.Src = P1.Src) AndAlso P0.Index <= P1.Index
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator <(P0 As Position, P1 As Position) As Boolean
      Return (P0.Src = P1.Src) AndAlso P0.Index < P1.Index
    End Operator
    <DebuggerStepperBoundary>
    Public Shared Operator >(P0 As Position, P1 As Position) As Boolean
      Return (P0.Src = P1.Src) AndAlso P0.Index > P1.Index
    End Operator
#End Region

#Region "Span Makers"
    <DebuggerStepperBoundary>
    Public Function ToZeroSpan() As Span
      Return Source.Span.Create_ZeroSpan(Me)
    End Function
    <DebuggerStepperBoundary>
    Public Function ToUnitSpan() As Span
      Return Source.Span.Create_UnitSpan(Me)
    End Function
#End Region

    <DebuggerStepperBoundary>
    Public Shared Function Create(Source As Source, Index As Integer) As Position
      Return New Position(Source, Index)
    End Function

    <DebuggerStepperBoundary>
    Public Overrides Function ToString() As String
      Return Me.Index.ToString
    End Function

  End Structure

End Class