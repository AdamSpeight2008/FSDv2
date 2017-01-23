Imports System.Globalization
Partial Public Class FormatString
  Private Const Trace As Boolean = True

  Partial Public Class ArgHole : Inherits Token
#Region "Resync Points"
    Private Shared RPX0 As ResyncPoints = ResyncPoints.CreateNew(AddressOf Align.Comma.TryParse,
                                                                 AddressOf Format.Colon.TryParse,
                                                                 AddressOf Common.Brace.Closing.TryParse)
#End Region

    <DebuggerStepperBoundary>
    Private Sub New(Span As Source.Span?, Inner As Tokens)
      MyBase.New(TokenKind.ArgHole, Span, Inner)
    End Sub

    Private Shared Function TryFind_Brace_Opening(ByRef Index As Source.Position?, ByRef Tokens As Tokens, DoingResync As Boolean) As Boolean
      Dim Token= Common.Brace.Opening.TryParse(Index, DoingResync)
      If Token.Kind <> TokenKind.Brace_Opening Then Return False 
      Tokens = Common.AddThenNext(Token, Tokens, Index)
      Return True
    End Function

    Private Shared Function TryFind_ArgIndex(ByRef Index As Source.Position?, ByRef Tokens As Tokens, DoingResync As Boolean) As Boolean
      Dim Token = ArgHole.Index.TryParse(Index, DoingResync)
      If TypeOf Token Is ParseError.EoT Then Tokens += Token : Return False
      If Token.Kind <> TokenKind.ArgHole_Index Then Return False
      Tokens = Common.AddThenNext(Token, Tokens, Index)
      Return True
    End Function

    Private Shared Function TryFind_ArgAlign(ByRef Index As Source.Position?, ByRef Tokens As Tokens, DoingResync As Boolean) As Boolean
      Dim Token = ArgHole.Align.TryParse(Index, DoingResync)
      If TypeOf Token Is ParseError.EoT Then Tokens += Token : Return False
      If Token.Kind <> TokenKind.ArgHole_Align Then Return False
      Tokens = Common.AddThenNext(Token, Tokens, Index)
      Return True
    End Function

    Private Shared Function TryFind_ArgFormat(ByRef Index As Source.Position?, ByRef Tokens As Tokens, DoingResync As Boolean) As Boolean
      Dim T As Token = ArgHole.Format.TryParse(Index, DoingResync)
      If TypeOf T Is ParseError.EoT Then Tokens += T : Return False
      If T.Kind <> TokenKind.ArgHole_Format Then Return False
      Tokens = Common.AddThenNext(T, Tokens, Index)
      Return True
    End Function

    Private Shared Function TryFind_Brace_Closing(ByRef Index As Source.Position?, ByRef Tokens As Tokens, DoingResync As Boolean) As Boolean
      Dim Token = Common.Brace.Closing.TryParse(Index, DoingResync)
      If TypeOf Token Is ParseError.EoT Then Tokens += Token : Return False
      If Token.Kind <> TokenKind.Brace_Closing Then Return False
      Tokens = Common.AddThenNext(Token, Tokens, Index)
      Return True
    End Function

    '<DebuggerStepperBoundary>
    Public Shared Function TryParse(Index As Source.Position?, DoingResync As Boolean) As Token
      '
      '  ArgHole ::= Brace.Opening ArgHole.Index?
      '
      If Index?.IsInvalid Then Return ParseError.Make.EoT(Index)

      Dim Start = Index, Tokens = FSDv2.Tokens.Empty

Find_Brace_Opening:
      If Not TryFind_Brace_Opening(Index, Tokens, DoingResync) Then GoTo TryToResync

Find_Index:
      If Not TryFind_ArgIndex(Index, Tokens, DoingResync) Then GoTo TryToResync

Find_Align:
      If Not TryFind_ArgAlign(Index, Tokens, DoingResync) Then GoTo TryToResync

Find_Format:
      If Not TryFind_ArgFormat(Index, Tokens, DoingResync) Then GoTo TryToResync

Find_Brace_Closing:
      If Not TryFind_Brace_Closing(Index, Tokens, DoingResync) Then GoTo TryToResync

Done:
      Return New ArgHole(Start?.To(Index), Tokens)
      ' Checking for the valid tokens is left the Analysis (TODO)
#Region "TryToResync"
TryToResync:
      If Index?.IsInvalid Then GoTo Done
      If Not DoingResync Then
        Dim ResultOfResyncing = RPX0.TryToResync(Index, True)
        Dim ThisParseError = TryCast(ResultOfResyncing, ParseError)
        If ThisParseError Is Nothing Then GoTo Find_Brace_Closing
        Dim size = ResultOfResyncing.Span?.Size
        If size.HasValue AndAlso size.Value > 0 Then Tokens = Common.AddThenNext(ResultOfResyncing, Tokens, Index)
        Select Case ResultOfResyncing(0).Kind
          Case TokenKind.Comma         : GoTo Find_Align
          Case TokenKind.Colon         : GoTo Find_Format
          Case TokenKind.Brace_Closing : GoTo Find_Brace_Closing
          Case Else
            Debug.Assert(False, "Unsupported Kind: " & ResultOfResyncing(0).Kind)
        End Select
      End If
      GoTo Find_Brace_Closing
#End Region
    End Function

  End Class

End Class