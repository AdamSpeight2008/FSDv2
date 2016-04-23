Partial Public Structure Source
  Friend ReadOnly Property ID As Guid
  Public ReadOnly Property Text As String
  Public ReadOnly Property Length As Integer
  Public ReadOnly Property Kind As SourceKind

  Private Sub New(Text As String, Kind As SourceKind)
    Me.ID = Guid.NewGuid
    Me.Text = If(Text, String.Empty)
    Me.Length = Me.Text.Length
    Me.Kind = Kind
  End Sub

  Public Function First() As Position?
    Return Position.Create(Me, If(Length <= 0, -1, 0))
  End Function

  Default Public ReadOnly Property Chars(Index As Integer) As Char?
    Get
      Return If((0 <= Index) AndAlso (Index < Me.Length), New Char?(Text(Index)), Nothing)
    End Get
  End Property

  Public Shared Function Create(Text As String, SourceKind As SourceKind) As Source
    Return New Source(Text, SourceKind)
  End Function

  Public Shared Operator =(S0 As Source, S1 As Source) As Boolean
    Return (S0.ID = S1.ID)
  End Operator
  Public Shared Operator <>(S0 As Source, S1 As Source) As Boolean
    Return (S0.ID <> S1.ID)
  End Operator

  Public Enum SourceKind As Integer
    VB_Standard
    CS_Standard
    CS_Verbatum
  End Enum





End Structure
