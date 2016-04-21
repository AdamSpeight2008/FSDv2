Imports FSDv2


Module Module1

  Sub Main()
    '           0         1         2         3         4         5         6         7         8
    '           012345678901234567890123456789012345678901234567890123456789012345678901234567890
    Dim Text = "{}"
    'Text = "}  {1 , -123 : {{ABC}} }  {1 , -123 : {{ABC}} {} "
    'Text = " {0001234567   } {0001234567, 0007654321} {0001234567, -0007654321}{0001234567, 0007654321 : XX44} {0001234567, -7654321 : XX44} {0001234567 : XX44} "
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard)
    Dim Ix = TheSource.First
    'Dim T0 = FormatString.ArgHole.Index.TryParse(Ix)
    'Dim T1 = FormatString.ArgHole.Align.TryParse(T0.Span.Next)
    'Dim T2 = FormatString.ArgHole.Format.TryParse(T1.Span.Next)
    ' Dim T3 = ArgHole.TryParse(Ix)
    'Dim Txt = TryCast(T3.Inner.Tokens(3).Inner.Tokens(1).Inner.Tokens(0), ArgHole.Text)


    'Dim rp0 = New ResyncPoint(AddressOf FormatString.Common.Digits.TryParse, Nothing) + New ResyncPoint(AddressOf FormatString.ArgHole.Align.Comma.TryParse, Nothing) +
    '          New ResyncPoint(AddressOf FormatString.ArgHole.Format.Colon.TryParse, Nothing) + New ResyncPoint(AddressOf FormatString.Common.Brace.Closing.TryParse, Nothing)
    '  Dim res = rp0.TryToResync(Ix)
    Dim sw = Diagnostics.Stopwatch.StartNew
    Dim T4 = FormatString.TryParse(Ix)
    sw.Stop()
    Console.WriteLine(sw.Elapsed.TotalMilliseconds.ToString)

  End Sub

End Module

