Partial Public Class FormatString : Inherits Token

  Friend Sub New(Span As Source.Span, Inner As Tokens)
    MyBase.New(Span, Inner)
  End Sub

  Public Shared Function TryParse(Ix As Source.Position) As Token
    If Ix.IsInvalid Then Return Nothing
    Dim sx = Ix
    Dim Txn = Tokens.Empty
    Dim T As Token
    Dim TextStart As Source.Position? = Nothing
    While Ix.IsValid
      T = Common.Brace.TryParse(Ix)
      Select Case True
        Case TypeOf T Is Common.Brace.Esc.Opening,
             TypeOf T Is Common.Brace.Esc.Closing
          Txn = Common.AddThenNext(T, Txn, Ix, TextStart)
        Case TypeOf T Is Common.Brace.Closing
          Txn = Common.AddThenNext(New ParseError(T.Span, ParseError.Reason.Invalid), Txn, Ix, TextStart)
        Case TypeOf T Is Common.Brace.Opening
          Dim res = ArgHole.TryParse(Ix)
          If res IsNot Nothing Then
            Txn = Common.AddThenNext(res, Txn, Ix, TextStart)
          Else
            Txn = Common.AddThenNext(New ParseError(Ix.ToUnitSpan, ParseError.Reason.UnexpectedCharacter), Txn, Ix, TextStart)
          End If
        Case Else
          If TextStart Is Nothing Then TextStart = New Source.Position?(Ix)
          Ix = Ix.Next
      End Select
    End While
    Txn = Common.AddThenNext(Nothing, Txn, Ix, TextStart)
    Return New FormatString(sx.To(Ix), Txn)
  End Function







End Class
