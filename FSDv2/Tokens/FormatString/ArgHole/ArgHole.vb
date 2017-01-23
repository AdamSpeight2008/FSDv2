Imports System.Globalization
Partial Public Class FormatString

  Public Class ArgHole : Inherits Token
#Region "Resync Points"
    Private Shared RPX0 As ResyncPoints = New ResyncPoint(AddressOf Align.Comma.TryParse) + New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)
#End Region

    Private Sub New(Span As Source.Span, Inner As Tokens)
      MyBase.New(TokenKind.ArgHole, Span, Inner)
    End Sub

    Public Shared Function TryParse(Ix As Source.Position) As Token
      '
      '  ArgHole ::= Brace.Opening ArgHole.Index?
      '
      If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
#Region "Find Brace Opening"
Find_Brace_Opening:
      Dim sx = Ix, txn = Tokens.Empty, T As Token = Common.Brace.Opening.TryParse(Ix)
      If T.Kind = TokenKind.Brace_Opening Then txn = Common.AddThenNext(T, txn, Ix)
#End Region
#Region "Find Index"
Find_Index:
      T = ArgHole.Index.TryParse(Ix)
      If TypeOf T Is ParseError.EoT Then txn += T : GoTo Done
      txn = Common.AddThenNext(T, txn, Ix)
      If T.Kind <> TokenKind.ArgHole_Index Then

        GoTo TryToResync
      End If
#End Region
#Region "Find Align"
Find_Align:
      T = ArgHole.Align.TryParse(Ix)
      If TypeOf T Is ParseError.EoT Then txn += T : GoTo Done
      If T.Kind <> TokenKind.ArgHole_Align Then GoTo TryToResync
      txn = Common.AddThenNext(T, txn, Ix)
#End Region
#Region "Find Format"
Find_Format:
      T = ArgHole.Format.TryParse(Ix)
      If TypeOf T Is ParseError.EoT Then txn += T : GoTo Done
      If T.Kind <> TokenKind.ArgHole_Format Then GoTo TryToResync
      txn = Common.AddThenNext(T, txn, Ix)
#End Region
#Region "Find Brace Closing"
Find_Brace_Closing:
      T = Common.Brace.Closing.TryParse(Ix)
      If TypeOf T Is ParseError.EoT Then txn += T : GoTo Done
      If T.Kind = TokenKind.Brace_Closing Then txn = Common.AddThenNext(T, txn, Ix)
#End Region
Done:
      Return New ArgHole(sx.To(Ix), txn)
      ' Checking for the valid tokens is left the Analysis (TODO)
#Region "TryToResync"
TryToResync:
      Dim rp1 = RPX0.TryToResync(Ix), pe = TryCast(rp1, ParseError)
      If pe Is Nothing OrElse pe.Why = ParseError.Reason.NullParse Then GoTo Find_Brace_Closing
      Select Case rp1(0).Kind
        Case TokenKind.Comma : txn = Common.AddThenNext(rp1, txn, Ix) : GoTo Find_Align
        Case TokenKind.Colon : txn = Common.AddThenNext(rp1, txn, Ix) : GoTo Find_Format
        Case TokenKind.Brace_Closing : txn = Common.AddThenNext(rp1, txn, Ix) : GoTo Find_Brace_Closing
      End Select
      GoTo Find_Brace_Closing
#End Region
    End Function

    Public Class Index : Inherits Token
#Region "ResyncPoints"
      Private Shared RPX As ResyncPoints = New ResyncPoint(AddressOf Common.Digits.TryParse) + New ResyncPoint(AddressOf Align.Comma.TryParse) +
                                     New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)
#End Region

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.ArgHole_Index, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
        Dim Txn = Tokens.Empty, T As Token, sx = Ix
#Region "AreThereDigits"
AreThereDigits:
        T = Common.Digits.TryParse(Ix)
        If T.Kind <> TokenKind.Digits Then GoTo TryToResync
        Txn = Common.AddThenNext(T, Txn, Ix)
#End Region
#Region "AreTheWhitespace"
AreThereWhitespace:
        T = Common.Whitespaces.TryParse(Ix)
        If T.Kind = TokenKind.Whitespaces Then Txn = Common.AddThenNext(T, Txn, Ix)
#End Region
Done:
        Return New Index(sx.To(Ix), Txn)

#Region "TryToResync"
TryToResync:
        Dim qx = Ix
        Dim r = RPX.TryToResync(Ix)
        Dim pe = TryCast(r, ParseError)
        If pe IsNot Nothing Then
          Select Case pe.Why
            Case ParseError.Reason.Partial
              If r.Span.Size > 0 Then
                Dim tmp As ParseError = ParseError.Make.UnexpectedChars(sx.To(r.Span.Start.Next), Tokens.Empty, "")
                Txn = Common.AddThenNext(tmp, Txn, Ix)
              End If
              Select Case pe(0).Kind
                Case TokenKind.Digits : GoTo AreThereDigits
                Case TokenKind.Whitespaces : GoTo AreThereWhitespace
                Case TokenKind.Brace_Closing : GoTo Null
              End Select
          End Select
        End If
#End Region
Null:
        Return ParseError.Make.NullParse(Ix, Txn)
      End Function

    End Class

    Public Class Identifier : Inherits Token
#Region "ResyncPoints"
      'Private Shared RPX As ResyncPoints = New ResyncPoint(AddressOf Common.Digits.TryParse) + New ResyncPoint(AddressOf Align.Comma.TryParse) +
      '                               New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)
#End Region
      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.Identifier, Span, Inner)
      End Sub
#Region "Grammar Definitions"
#Region "C# Identifer grammar"
      '                  identifier  ::=  available-identifier  |  "@" identifier-Or-keyword
      '        available-identifier  ::=  An identifier-Or-keyword that Is Not a keyword
      '       identifier-Or-keyword  ::=  identifier-start-character identifier-part-character*
      '  identifier-start-character  ::=  letter-character  |  "_"
      '   identifier-part-character  ::=  letter-character  |  Decimal-digit-character  |  connecting-character  |  combining-character  |  FormattingCharacter
      '            letter-character  ::=  AlphaCharacter       |  < A unicode-character-escape-sequence representing a character Of classes Lu, Ll, Lt, Lm, Lo, Or Nl >
      '         combining-character  ::=  CombiningCharacter   |  < A unicode-character-escape-sequence representing a character Of classes Mn Or Mc
      '     Decimal-digit-character  ::=  NumericCharacter     |  < A unicode-character-escape-sequence representing a character Of the Class Nd >
      '        connecting-character  ::=  UnderscoreCharacter  |  < A unicode-character-escape-sequence representing a character Of the Class Pc >
      '        formatting-character  ::=  FormattingCharacter  |  < A unicode-character-escape-sequence representing a character Of the Class Cf >
#End Region
#Region "VB Identifier Grammar"
      '            Identifier  ::=  NonEscapedIdentifier [ TypeCharacter ]  |  EscapedIdentifier
      '  NonEscapedIdentifier  ::=  < IdentifierName but Not Keyword >
      '     EscapedIdentifier  ::=  [ IdentifierName ] 
      '        IdentifierName  ::=  IdentifierStart [ IdentifierCharacter+ ]
      '       IdentifierStart  ::=  AlphaCharacter |  UnderscoreCharacter IdentifierCharacter
      '   IdentifierCharacter  ::=  UnderscoreCharacter  |  AlphaCharacter  |  NumericCharacter  |  CombiningCharacter   |  FormattingCharacter
#End Region
#Region "Common Grammar"
      '        AlphaCharacter  ::=  < Unicode alphabetic character (classes Lu, Ll, Lt, Lm, Lo, Nl) >
      '      NumericCharacter  ::=  < Unicode decimal digit character (class Nd) >
      '    CombiningCharacter  ::=  < Unicode combining character (classes Mn, Mc) >
      '   FormattingCharacter  ::=  < Unicode formatting character (class Cf) >
      '   UnderscoreCharacter  ::=  < Unicode connection character (class Pc) >
      '   IdentifierOrKeyword  ::=  Identifier | Keyword
#End Region
#End Region

      Private Shared Function AlphaCharacter(ch As Char) As Boolean
        '  AlphaCharacter::=  <Unicode alphabetic character (classes Lu, Ll, Lt, Lm, Lo, Nl) >
        Dim category = Char.GetUnicodeCategory(ch)
        Select Case category
          Case UnicodeCategory.UppercaseLetter, UnicodeCategory.LowercaseLetter, UnicodeCategory.TitlecaseLetter,
               UnicodeCategory.ModifierLetter, UnicodeCategory.OtherLetter, UnicodeCategory.LetterNumber
            Return True
        End Select
        Return False
      End Function
      Private Shared Function NumericCharacter(ch As Char) As Boolean
        '  NumericCharacter::= < Unicode decimal digit character (class Nd) >
        Dim category = Char.GetUnicodeCategory(ch)
        Select Case category
          Case UnicodeCategory.DecimalDigitNumber
            Return True
        End Select
        Return False
      End Function
      Private Shared Function CombiningCharacter(ch As Char) As Boolean
        '  CombiningCharacter::= < Unicode combining character (classes Mn, Mc) >
        Dim category = Char.GetUnicodeCategory(ch)
        Select Case category
          Case UnicodeCategory.NonSpacingMark, UnicodeCategory.SpacingCombiningMark
            Return True
        End Select
        Return False
      End Function
      Private Shared Function FormattingCharacter(ch As Char) As Boolean
        '  FormattingCharacter::= < Unicode formatting character (class Cf) >
        Dim category = Char.GetUnicodeCategory(ch)
        Select Case category
          Case UnicodeCategory.Format
            Return True
        End Select
        Return False
      End Function
      Private Shared Function UnderscoreCharacter(ch As Char) As Boolean
        '  UnderscoreCharacter::= < Unicode connection character (class Pc) >
        Dim category = Char.GetUnicodeCategory(ch)
        Select Case category
          Case UnicodeCategory.ConnectorPunctuation
            Return True
        End Select
        Return False
      End Function

      Private Shared Function IdentifierCharacter(ch As Char) As Boolean
        '  IdentifierCharacter::=  UnderscoreCharacter |  AlphaCharacter |  NumericCharacter  |  CombiningCharacter  |  FormattingCharacter
        Return UnderscoreCharacter(ch) OrElse AlphaCharacter(ch) OrElse NumericCharacter(ch) OrElse CombiningCharacter(ch) OrElse FormattingCharacter(ch)
      End Function
      Private Shared Function FirstIdentifierCharacter(ch As Char) As Boolean
        Return (ch = "_"c) OrElse IdentifierCharacter(ch)
      End Function

      Private Shared Function TryParse_VBIdentifer(Ix As Source.Position) As Token
        Debug.Assert(Ix.Source.KindOfString = Source.StringKind.StringInterpolation)
        Debug.Assert(Ix.Source.Kind = Source.SourceKind.VB_Standard)
        If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
        Dim Txn = Tokens.Empty, T As Token, sx = Ix
FirstCharacter:
        If FirstIdentifierCharacter(Ix.Value.Value) Then
RestCharacters:
          Ix = Ix.Next
          While Ix.IsValid AndAlso IdentifierCharacter(Ix.Value.Value)
            Ix = Ix.Next
          End While
        End If
Done:
        Return New Identifier(sx.To(Ix), Txn)
#Region "TryToResync"
TryToResync:
        'Dim qx = Ix
        'Dim r = RPX.TryToResync(Ix)
        'Dim pe = TryCast(r, ParseError)
        'If pe IsNot Nothing Then
        '    Select Case pe.Why
        '        Case ParseError.Reason.Partial
        '            If r.Span.Size > 0 Then
        '                Dim tmp As ParseError = ParseError.Make.UnexpectedChars(sx.To(r.Span.Start.Next), Tokens.Empty, "")
        '                Txn = Common.AddThenNext(tmp, Txn, Ix)
        '            End If
        '            Select Case pe(0).Kind
        '                Case TokenKind.Digits : GoTo AreThereDigits
        '                Case TokenKind.Whitespaces : GoTo AreThereWhitespace
        '                Case TokenKind.Brace_Closing : GoTo Null
        '            End Select
        '    End Select
        'End If
#End Region
Null:
        Return ParseError.Make.NullParse(Ix, Tokens.Empty)
      End Function
      Private Shared Function TryParse_CSIdentifer(Ix As Source.Position) As Token
        Debug.Assert(Ix.Source.KindOfString = Source.StringKind.StringInterpolation)
        Debug.Assert(Ix.Source.Kind = Source.SourceKind.CS_Standard)
        If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
        Dim Txn = Tokens.Empty, T As Token, sx = Ix
FirstCharacter:
        If FirstIdentifierCharacter(Ix.Value.Value) Then
RestCharacters:
          Ix = Ix.Next
          While Ix.IsValid AndAlso IdentifierCharacter(Ix.Value.Value)
            Ix = Ix.Next
          End While
        End If
Done:
        Return New Identifier(sx.To(Ix), Txn)
#Region "TryToResync"
TryToResync:
        'Dim qx = Ix
        'Dim r = RPX.TryToResync(Ix)
        'Dim pe = TryCast(r, ParseError)
        'If pe IsNot Nothing Then
        '    Select Case pe.Why
        '        Case ParseError.Reason.Partial
        '            If r.Span.Size > 0 Then
        '                Dim tmp As ParseError = ParseError.Make.UnexpectedChars(sx.To(r.Span.Start.Next), Tokens.Empty, "")
        '                Txn = Common.AddThenNext(tmp, Txn, Ix)
        '            End If
        '            Select Case pe(0).Kind
        '                Case TokenKind.Digits : GoTo AreThereDigits
        '                Case TokenKind.Whitespaces : GoTo AreThereWhitespace
        '                Case TokenKind.Brace_Closing : GoTo Null
        '            End Select
        '    End Select
        'End If
#End Region
Null:
        Return ParseError.Make.NullParse(Ix, Tokens.Empty)
      End Function
      Public Shared Function TryParse(Ix As Source.Position) As Token
        Debug.Assert(Ix.Source.KindOfString = Source.StringKind.StringInterpolation)
        If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
        Select Case Ix.Source.Kind
          Case Source.SourceKind.VB_Standard : Return TryParse_VBIdentifer(Ix)
          Case Source.SourceKind.CS_Standard : Return TryParse_CSIdentifer(Ix)
          Case Source.SourceKind.CS_Verbatum
        End Select
        Return ParseError.Make.NullParse(Ix, Tokens.Empty)
      End Function
    End Class

    Public Class Align : Inherits Token
#Region "ResyncePoints"
      Private Shared RPX0 As ResyncPoints = New ResyncPoint(AddressOf Head.TryParse) + New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)
      Private Shared RPX1 As ResyncPoints = New ResyncPoint(AddressOf Body.TryParse) + New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)
#End Region
      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.ArgHole_Align, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        '
        '  ArgHole.Index ::= ArgHole.Align.Head ArgHole.Align.Body
        '
        If Ix.IsInvalid Then Return ParseError.Make.NullParse(Ix)
#Region "IsThereAHead"
IsThereAHead:
        Dim Txn = Tokens.Empty, sx = Ix, _Head = Head.TryParse(Ix)
        If _Head.Kind <> TokenKind.ArgHole_Align_Head Then GoTo TryToResyncHead
        Txn = Common.AddThenNext(_Head, Txn, Ix)
#End Region
#Region "IsThereABody"
IsThereABody:
        Dim _Body = Body.TryParse(Ix)
        If _Body.Kind <> TokenKind.ArgHole_Align_Body Then GoTo TryToResyncBody
        Txn = Common.AddThenNext(_Body, Txn, Ix)
#End Region
Done:
        Return New Align(Txn.First.Span.Start.To(Txn.Last.Span.Next), Txn)

#Region "TryToResyncHead"
TryToResyncHead:
        Dim rp0 = RPX0.TryToResync(Ix)
        Select Case rp0.Kind
          Case TokenKind.ArgHole_Align_Head : Txn += New ParseError.Resync(sx.To(rp0.Span.Start), rp0) : GoTo IsThereAHead
          Case TokenKind.Colon : Txn += New ParseError.Resync(sx.To(rp0.Span.Start), rp0) : GoTo IsThereABody
          Case TokenKind.ArgHole_Align_Body : Txn += New ParseError.Resync(sx.To(rp0.Span.Start), rp0) : GoTo IsThereABody
          Case TokenKind.Brace_Closing : Txn += New ParseError.Resync(sx.To(rp0.Span.Start), rp0) : GoTo Done
          Case TokenKind.ParseError
            If TypeOf rp0 Is ParseError.Partial Then Txn += New ParseError.Resync(sx.To(rp0.Span.Start), rp0)
        End Select
        Return ParseError.Make.Invalid(Ix, Txn)

        Return ParseError.Make.NullParse(Ix)
#End Region
#Region "TryToResyncBody"
TryToResyncBody:
        Dim rp1 = RPX1.TryToResync(Ix)
        Select Case rp1.Kind
          Case TokenKind.ParseError
          Case TokenKind.ArgHole_Align_Head,
               TokenKind.Colon,
               TokenKind.ArgHole_Align_Body,
               TokenKind.Brace_Closing
            Txn += New ParseError.Resync(sx.To(rp1.Span.Start), rp1)
            GoTo Done
        End Select
        GoTo Done
#End Region
      End Function

      Public Class Comma : Inherits Token

        Private Sub New(Span As Source.Span)
          MyBase.New(TokenKind.Comma, Span)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          If (Ix.Value <> ","c) Then Return ParseError.Make.NullParse(Ix)
          Return New Comma(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class MinusSign : Inherits Token

        Private Sub New(Span As Source.Span)
          MyBase.New(TokenKind.MinusSign, Span)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          If (Ix.Value <> "-"c) Then Return ParseError.Make.NullParse(Ix)
          Return New MinusSign(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class Head : Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Align_Head, Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          '
          ' ArgHole.Align.Head ::= Comma Whitespaces?
          '
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          Dim Txn = Tokens.Empty, T = Comma.TryParse(Ix)
          If T.Kind <> TokenKind.Comma Then Return ParseError.Make.NullParse(Ix)
          Dim sx = Ix : Txn += T : Ix = T.Span.Next
          T = Common.Whitespaces.TryParse(Ix)
          If T.Kind = TokenKind.Whitespaces Then Txn += T : Ix = T.Span.Next
          Return New Head(sx.To(Ix), Txn)
        End Function

      End Class

      Public Class Body : Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Align_Body, Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          '
          '  ArgHole.Align.Body ::= MinusSign? Digits Whitespaces?
          '
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          Dim sx = Ix, Txn = Tokens.Empty
          Dim T As Token = MinusSign.TryParse(Ix)
          If T.Kind = TokenKind.MinusSign Then Txn = Common.AddThenNext(T, Txn, Ix)
          T = Common.Digits.TryParse(Ix)
          If T.Kind = TokenKind.ParseError Then Return ParseError.Make.NullParse(Ix)
          Txn = Common.AddThenNext(T, Txn, Ix)
          T = Common.Whitespaces.TryParse(Ix)
          If T.Kind = TokenKind.Whitespaces Then Txn = Common.AddThenNext(T, Txn, Ix)
          Return New Body(sx.To(Ix), Txn)
        End Function

      End Class

    End Class

    Public Class Format : Inherits Token

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.ArgHole_Format, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
        Dim T As Token = Format.Head.TryParse(Ix)
        If TypeOf T Is ParseError Then Return T
        Dim sx = Ix
        Dim Txn = Tokens.Empty : Txn = Common.AddThenNext(T, Txn, Ix)
        T = ArgHole.Format.Body.TryParse(Ix)
        If T.Kind = TokenKind.ArgHole_Format_Body Then Txn = Common.AddThenNext(T, Txn, Ix)
        Return New Format(sx.To(Ix), Txn)
      End Function

      Public Class Head
        Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Format_Head, Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          Dim T As Token = Colon.TryParse(Ix)
          If T.Kind = TokenKind.ParseError Then Return ParseError.Make.NullParse(Ix)
          Return New Head(T.Span, Tokens.Empty + T)
        End Function

      End Class

      Public Class Body
        Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Format_Body, Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          Dim Txn = Tokens.Empty
          Dim sx = Ix
          Dim T As Token
          While Ix.IsValid

            T = Common.Brace.Opening.TryParse(Ix)
            Select Case T.Kind
              Case TokenKind.Brace_Opening : T = ParseError.Make.Invalid(T.Span, T) : GoTo OnToNext
              Case TokenKind.Esc_Brace_Opening : GoTo OnToNext
            End Select

            T = Common.Brace.Closing.TryParse(Ix)
            Select Case T.Kind
              Case TokenKind.Brace_Closing : Exit While
              Case TokenKind.Esc_Brace_Closing : GoTo OnToNext
            End Select

            T = Text._TryParse(Ix, True)
            Select Case T.Kind
              Case TokenKind.ParseError : Exit While
            End Select

OnToNext:
            Txn = Common.AddThenNext(T, Txn, Ix)

          End While
          Return New Format.Body(sx.To(Ix), Txn)
        End Function

      End Class

      Public Class Colon : Inherits Token

        Private Sub New(Span As Source.Span)
          MyBase.New(TokenKind.Colon, Span)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          If (Ix <> ":"c) Then Return ParseError.Make.NullParse(Ix)
          Return New Colon(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

    End Class

  End Class

End Class