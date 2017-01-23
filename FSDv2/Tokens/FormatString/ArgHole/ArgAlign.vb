Imports System.Globalization
Partial Public Class FormatString
  Partial Public Class ArgHole : Inherits Token

    Public Class Align : Inherits Token
#Region "ResyncePoints"
      Private Shared ReadOnly RPX0 As ResyncPoints = ResyncPoints.CreateNew(AddressOf Head.TryParse,
                                                                   AddressOf Format.Colon.TryParse,
                                                                   AddressOf Common.Brace.Closing.TryParse)
      Private Shared ReadOnly RPX1 As ResyncPoints = ResyncPoints.CreateNew(AddressOf Body.TryParse,
                                                                            AddressOf Format.Colon.TryParse,
                                                                            AddressOf Common.Brace.Closing.TryParse)
#End Region

      <DebuggerStepperBoundary>
      Private Sub New(Span As Source.Span?, Inner As Tokens)
        MyBase.New(TokenKind.ArgHole_Align, Span, Inner)
      End Sub

      <DebuggerStepperBoundary>
      Public Shared Function TryParse(Idx As Source.Position?, DoingResync As Boolean) As Token
        '
        '  ArgHole.Index ::= ArgHole.Align.Head ArgHole.Align.Body
        '
        If Idx?.IsInvalid Then Return ParseError.Make.NullParse(Idx)
#Region "IsThereAHead"
IsThereAHead:
        Dim Txn = Tokens.Empty, sx = Idx, _Head = Head.TryParse(Idx, DoingResync)
        If _Head.Kind <> TokenKind.ArgHole_Align_Head Then GoTo TryToResyncHead
        Txn = Common.AddThenNext(_Head, Txn, Idx)

#End Region
#Region "IsThereABody"
IsThereABody:
        Dim _Body = Body.TryParse(Idx, DoingResync)
        If _Body.Kind <> TokenKind.ArgHole_Align_Body Then GoTo TryToResyncBody
        Txn = Common.AddThenNext(_Body, Txn, Idx)
#End Region
Done:
        Return New Align(Txn.First.Span?.Start?.To(Txn.Last.Span?.Next), Txn)

#Region "TryToResyncHead"
TryToResyncHead:
        If Not DoingResync Then
          Dim ResultOfResyncing = RPX0.TryToResync(Idx, True)
          Dim Size = ResultOfResyncing.Span?.Size
          If Size > 0 Then Txn += New ParseError.Resync(sx?.To(ResultOfResyncing.Span?.Start), ResultOfResyncing)
          Select Case ResultOfResyncing.Kind
            Case TokenKind.ArgHole_Align_Head : GoTo IsThereAHead
            Case TokenKind.Colon              : GoTo IsThereABody
            Case TokenKind.ArgHole_Align_Body : GoTo IsThereABody
            Case TokenKind.Brace_Closing      : GoTo Done
            Case TokenKind.ParseError
            Case Else
              Debug.Assert(False, "Unsupported Kind: " & ResultOfResyncing.Kind)
          End Select
        End If
        'Return ParseError.Make.Invalid(Ix, Txn)

        Return ParseError.Make.NullParse(Idx)
#End Region
#Region "TryToResyncBody"
TryToResyncBody:
        If Not DoingResync Then
          Dim ResultOfResyncing = RPX1.TryToResync(Idx, True)
          Dim size = ResultOfResyncing.Span?.Size
          If size > 0 Then
            Txn += New ParseError.Resync(sx?.To(ResultOfResyncing.Span?.Start), ResultOfResyncing)
            GoTo Done
          End If
          Select Case ResultOfResyncing.Kind
            Case TokenKind.ParseError
            Case TokenKind.ArgHole_Align_Head, TokenKind.Colon,
                 TokenKind.ArgHole_Align_Body, TokenKind.Brace_Closing  :  GoTo Done
            Case Else
              Debug.Assert(False, "Unsupported Kind: " & ResultOfResyncing.Kind)

          End Select
        End If
        GoTo Done
#End Region
      End Function

      Public Class Comma : Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span?)
          MyBase.New(TokenKind.Comma, Span)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Function TryParse(Ix As Source.Position?, DoingResync As Boolean) As Token
          If Ix?.IsInvalid Then Return ParseError.Make.EoT(Ix)
          If (Ix?.Value <> ","c) Then Return ParseError.Make.NullParse(Ix)
          Return New Comma(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class MinusSign : Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span?)
          MyBase.New(TokenKind.MinusSign, Span)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Function TryParse(Ix As Source.Position?, DoingResync As Boolean) As Token
          If Ix?.IsInvalid Then Return ParseError.Make.EoT(Ix)
          If (Ix?.Value <> "-"c) Then Return ParseError.Make.NullParse(Ix)
          Return New MinusSign(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class Head : Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span?, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Align_Head, Span, Inner)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Function TryParse(Index As Source.Position?, DoingResync As Boolean) As Token
          '
          ' ArgHole.Align.Head ::= Comma Whitespaces?
          '
          If Index?.IsInvalid Then Return ParseError.Make.EoT(Index)
          Dim Tokens = FSDv2.Tokens.Empty, Token = Comma.TryParse(Index, DoingResync)
          If Token.Kind <> TokenKind.Comma Then Return ParseError.Make.NullParse(Index)
          Dim Start = Index : Tokens += Token : Index = Token.Span?.Next
          Token = Common.Whitespaces.TryParse(Index, DoingResync)
          If Token.Kind = TokenKind.Whitespaces Then Tokens += Token : Index = Token.Span?.Next
          Return New Head(Start?.To(Index), Tokens)
        End Function

      End Class

      Public Class Body : Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span?, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Align_Body, Span, Inner)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Function TryParse(Index As Source.Position?, DoingResync As Boolean) As Token
          '
          '  ArgHole.Align.Body ::= MinusSign? Digits Whitespaces?
          '
          If Index?.IsInvalid Then Return ParseError.Make.EoT(Index)
          Dim Start = Index, Tokens = FSDv2.Tokens.Empty, Token As Token
          Token = MinusSign.TryParse(Index, DoingResync)
          If Token.Kind = TokenKind.MinusSign Then Tokens = Common.AddThenNext(Token, Tokens, Index)
          Token = Common.Digits.TryParse(Index, DoingResync)
          If Token.Kind = TokenKind.ParseError Then Return ParseError.Make.NullParse(Index)
          Tokens = Common.AddThenNext(Token, Tokens, Index)
          Token = Common.Whitespaces.TryParse(Index, DoingResync)
          If Token.Kind = TokenKind.Whitespaces Then Tokens = Common.AddThenNext(Token, Tokens, Index)
          Return New Body(Start?.To(Index), Tokens)
        End Function

      End Class

    End Class

  End Class
End Class