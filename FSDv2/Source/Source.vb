Partial Public Structure Source

#Region "ReadOnly Properties"
  Friend ReadOnly Property ID As Guid
  Public ReadOnly Property Text As String
  Public ReadOnly Property Length As Integer
    Public ReadOnly Property Kind As SourceKind
    Public ReadOnly Property KindOfString As StringKind
#End Region

    Private Sub New(Text As String, Kind As SourceKind, KindOfString As StringKind)
        Me.ID = Guid.NewGuid : Me.Text = If(Text, String.Empty) : Me.Length = Me.Text.Length : Me.Kind = Kind
        Me.KindOfString = KindOfString
    End Sub

    Public Function First() As Position?
    Return Position.Create(Me, If(Length <= 0, -1, 0))
  End Function

  Default Public ReadOnly Property Chars(Index As Integer) As Char?
    Get
      Return If((0 <= Index) AndAlso (Index < Me.Length), New Char?(Text(Index)), Nothing)
    End Get
  End Property

    Public Shared Function Create(Text As String, SourceKind As SourceKind, KindOfString As StringKind) As Source
        Return New Source(Text, SourceKind, KindOfString)
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

    Public Enum StringKind As Integer
        StringFormat = 0
        StringInterpolation = 1
    End Enum

End Structure
