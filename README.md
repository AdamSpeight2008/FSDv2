# FSDv2

FSDv2 is the core parser for the text in format string, eg `String.Format("X:= {0}, Y:= {1}", x, y)`

The result is a simple object model of the contents of the string, so that further analyse can be performed.

For example: An Argument Index is out of range;= `String.Format("X:= {1}", x)`
