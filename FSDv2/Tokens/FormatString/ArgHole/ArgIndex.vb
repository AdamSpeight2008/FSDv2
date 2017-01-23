Imports System.Globalization
Partial Public Class FormatString
  Partial Public Class ArgHole : Inherits Token


    Public Class Index : Inherits Token
#Region "ResyncPoints"
      Private Shared RPX As ResyncPoints = ResyncPoints.CreateNew(AddressOf Common.Digits.TryParse,
                                                                  AddressOf Align.Comma.TryParse,
                                                                  AddressOf Format.Colon.TryParse,
                                                                  AddressOf Common.Brace.Closing.TryParse)
#End Region

      <DebuggerStepperBoundary>
      Private Sub New(Span As Source.Span?, Inner As Tokens)
        MyBase.New(TokenKind.ArgHole_Index, Span, Inner)
      End Sub

      <DebuggerStepperBoundary>
      Public Shared Function TryParse(Index As Source.Position?, DoingResync As Boolean) As Token
        If Index?.IsInvalid Then Return ParseError.Make.EoT(Index)
        Dim Tokens = FSDv2.Tokens.Empty, Token As Token, Start = Index

#Region "AreThereDigits"
AreThereDigits:
        Token = Common.Digits.TryParse(Index, DoingResync)
        If Token.Kind <> TokenKind.Digits Then GoTo TryToResync
        Tokens = Common.AddThenNext(Token, Tokens, Index)
#End Region

#Region "AreTheWhitespace"
AreThereWhitespace:
        Token = Common.Whitespaces.TryParse(Index, DoingResync)
        If Token.Kind = TokenKind.Whitespaces Then Tokens = Common.AddThenNext(Token, Tokens, Index)
#End Region
Done:
        Return New Index(Start?.To(Index), Tokens)

#Region "TryToResync"
TryToResync:
        If Not DoingResync Then

          Dim ThisParseError = TryCast(Token, FSDv2.ParseError)
          If ThisParseError?.Why=ParseError.Reason.UnexpectedCharacter Then Tokens += ThisParseError.InnerTokens(0)

          Dim ResultOfResyncing = RPX.TryToResync(Index, True)
          Dim TheParseError     = TryCast(ResultOfResyncing, ParseError)
          If TheParseError Is Nothing Then GoTo Null
          Select Case TheParseError.Why
            Case ParseError.Reason.ResyncSkipped
              CheckSpanSizeOfResync(Index, Tokens, Start, ResultOfResyncing)

              Select Case TheParseError(0).Kind
                Case TokenKind.Digits        : GoTo AreThereDigits
                Case TokenKind.Whitespaces   : GoTo AreThereWhitespace
                Case TokenKind.Brace_Closing : GoTo Null
              End Select
            Case Else
              Debug.Assert(False, "Unsupported Kind: " & ResultOfResyncing.Kind)
          End Select
#End Region
Null:
        End If
        Return ParseError.Make.NullParse(Index, Tokens) ' New FormatString.ArgHole.Index(qx.ToZeroSpan, Tokens.Empty)) ' Txn)
      End Function

      Private Shared Sub CheckSpanSizeOfResync(ByRef Index As Source.Position?, ByRef Tokens As Tokens, sx As Source.Position?, ResultOfResyncing As Token)
        If ResultOfResyncing.Span?.Size = 0 Then Exit Sub
        Dim tmp = ParseError.Make.UnexpectedChars(sx?.To(ResultOfResyncing.Span?.Start?.Next), Tokens.Empty, String.Empty)
        Tokens = Common.AddThenNext(tmp, Tokens, Index)
       End Sub
    End Class
  End Class
End Class
