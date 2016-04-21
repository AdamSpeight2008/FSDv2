Partial Public Structure Source

  <DebuggerDisplay("{Index}=[{Value}]")>
  Public Structure Position
    Public ReadOnly Property Source As Source
    Public ReadOnly Property Index As Integer
    Public ReadOnly Property IsValid As Boolean
    Public ReadOnly Property IsInvalid As Boolean

    Private Sub New(Source As Source, Index As Integer)
      Me.Source = Source : Me.Index = Bound(Index, 0, Source.Length) : Me.IsValid = IsWithin(Index, 0, Source.Length) : Me.IsInvalid = Not IsValid
    End Sub

    Public Function Value() As Char?
      Return Source(Index)
    End Function

    Private Function IsWithin(Value As Integer, Limit0 As Integer, Limit1 As Integer) As Boolean
      Return (Limit0 <= Value) AndAlso (Value < Limit1)
    End Function

    Private Function Bound(value As Integer, Limit0 As Integer, Limit1 As Integer) As Integer
      If value < Limit0 Then
        If Limit0 = Integer.MinValue Then Return Integer.MinValue Else Return Limit0 - 1
      ElseIf value > Limit1 Then
        If Limit1 = Integer.MaxValue Then Return Integer.MaxValue Else Return Limit1
      Else
        Return value
      End If
    End Function

    Public Function [Next]() As Position
      Return New Position(Me.Source, Index + 1)
    End Function
    Public Function AddOffset(Offset As Integer) As Position
      Return New Position(Me.Source, Index + Offset)
    End Function
    Public Function [To](p As Position) As Span?
      If Me.Source <> p.Source Then Return Nothing
      Return Source.Span.From(Me, p)
    End Function

    Public Shared Operator =(ByVal P As Position, Ch As Char) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso (P.Value.Value = Ch)
    End Operator
    Public Shared Operator >(ByVal P As Position, Ch As Char) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso (P.Value.Value > Ch)
    End Operator
    Public Shared Operator <(ByVal P As Position, Ch As Char) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso (P.Value.Value < Ch)
    End Operator
    Public Shared Operator <=(ByVal P As Position, Ch As Char) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso (P.Value.Value <= Ch)
    End Operator
    Public Shared Operator >=(ByVal P As Position, Ch As Char) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso (P.Value.Value >= Ch)
    End Operator
    Public Shared Operator <>(ByVal P As Position, Ch As Char) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso (P.Value.Value <> Ch)
    End Operator


    Public Shared Operator =(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso Ch = P.Value.Value
    End Operator
    Public Shared Operator >(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso Ch > P.Value.Value
    End Operator
    Public Shared Operator <(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso Ch < P.Value.Value
    End Operator
    Public Shared Operator <=(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso Ch <= P.Value.Value
    End Operator
    Public Shared Operator >=(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso Ch >= P.Value.Value
    End Operator
    Public Shared Operator <>(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso Ch <> P.Value.Value
    End Operator

    Public Shared Operator =(P0 As Position, P1 As Position) As Boolean
      Return (P0.Source = P1.Source) AndAlso P0.Index = P1.Index
    End Operator
    Public Shared Operator <>(P0 As Position, P1 As Position) As Boolean
      Return (P0.Source = P1.Source) AndAlso P0.Index <> P1.Index
    End Operator
    Public Shared Operator >=(P0 As Position, P1 As Position) As Boolean
      Return (P0.Source = P1.Source) AndAlso P0.Index >= P1.Index
    End Operator
    Public Shared Operator <=(P0 As Position, P1 As Position) As Boolean
      Return (P0.Source = P1.Source) AndAlso P0.Index <= P1.Index
    End Operator
    Public Shared Operator <(P0 As Position, P1 As Position) As Boolean
      Return (P0.Source = P1.Source) AndAlso P0.Index < P1.Index
    End Operator
    Public Shared Operator >(P0 As Position, P1 As Position) As Boolean
      Return (P0.Source = P1.Source) AndAlso P0.Index > P1.Index
    End Operator

    Public Function ToZeroSpan() As Span
      Return Source.Span.Create_ZeroSpan(Me)
    End Function
    Public Function ToUnitSpan() As Span
      Return Source.Span.Create_UnitSpan(Me)
    End Function

    Public Shared Function Create(Source As Source, Index As Integer) As Position
      Return New Position(Source, Index)
    End Function

    Public Overrides Function ToString() As String
      Return Me.Index.ToString
    End Function
  End Structure
End Structure