Imports System.Runtime.CompilerServices

Public Module Exts

  <Extension>
  Public Function IsNotNullParse(T As Token) As Boolean
    Return TryCast(T, ParseError)?.Why <> ParseError.Reason.Invalid
  End Function

  <Extension>
  Public Function IsKindOrIsNotNullParse(T As Token, k As TokenKind) As Boolean
    Return (T.Kind = k) OrElse T.IsNotNullParse
  End Function

End Module
