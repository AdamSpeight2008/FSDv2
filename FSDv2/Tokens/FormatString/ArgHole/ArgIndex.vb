Imports System.Globalization
Partial Public Class FormatString
  Partial Public Class ArgHole : Inherits Token


    Public Class Index : Inherits Token
#Region "ResyncPoints"
      Private Shared RPX As ResyncPoints = New ResyncPoint(AddressOf Common.Digits.TryParse) + New ResyncPoint(AddressOf Align.Comma.TryParse) +
                                       New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)
#End Region

      <DebuggerStepperBoundary>
      Private Sub New(Span As Source.Span?, Inner As Tokens)
        MyBase.New(TokenKind.ArgHole_Index, Span, Inner)
      End Sub

      <DebuggerStepperBoundary>
      Public Shared Function TryParse(Ix As Source.Position?, DoingResync As Boolean) As Token
        If Ix?.IsInvalid Then Return ParseError.Make.EoT(Ix)
        Dim Txn = Tokens.Empty, T As Token, sx = Ix
#Region "AreThereDigits"
AreThereDigits:
        T = Common.Digits.TryParse(Ix, DoingResync)
        If T.Kind <> TokenKind.Digits Then GoTo TryToResync
        Txn = Common.AddThenNext(T, Txn, Ix)
#End Region
#Region "AreTheWhitespace"
AreThereWhitespace:
        T = Common.Whitespaces.TryParse(Ix, DoingResync)
        If T.Kind = TokenKind.Whitespaces Then Txn = Common.AddThenNext(T, Txn, Ix)
#End Region
Done:
        Return New Index(sx?.To(Ix), Txn)

#Region "TryToResync"
TryToResync:
        If Not DoingResync Then
          Dim er = TryCast(T, FSDv2.ParseError)
          If er IsNot Nothing Then
            Select Case er.Why
              Case ParseError.Reason.UnexpectedCharacter
                Txn += er.InnerTokens(0)
            End Select
          End If


          Dim qx = Ix
          Dim ResultOfResyncing = RPX.TryToResync(Ix, True)
          Dim TheParseError = TryCast(ResultOfResyncing, ParseError)
          If TheParseError IsNot Nothing Then
            Select Case TheParseError.Why
              Case ParseError.Reason.ResyncSkipped
                If ResultOfResyncing.Span?.Size > 0 Then
                  Dim tmp As ParseError = ParseError.Make.UnexpectedChars(sx?.To(ResultOfResyncing.Span?.Start?.Next), Tokens.Empty, "")
                  Txn = Common.AddThenNext(tmp, Txn, Ix)
                End If
                Select Case TheParseError(0).Kind
                  Case TokenKind.Digits : GoTo AreThereDigits
                  Case TokenKind.Whitespaces : GoTo AreThereWhitespace
                  Case TokenKind.Brace_Closing : GoTo Null
                End Select
              Case Else
                Debug.Assert(False, "Unsupported Kind: " & ResultOfResyncing.Kind)


            End Select
          End If
#End Region
Null:
        End If
        Return ParseError.Make.NullParse(Ix, Txn) ' New FormatString.ArgHole.Index(qx.ToZeroSpan, Tokens.Empty)) ' Txn)
      End Function

    End Class
  End Class
End Class
