#Include "fbstd/map.bi"


Using fbstd
function PairToString (p as any ptr) as string
	with *cast(Pair ptr, p)
		return str(*cast(integer ptr, .first)) & ":" & str(*cast(integer ptr, .first))
	end with
end Function

Dim val1 As Integer
Dim val2 As String


val1 = 10
val2 = "test"


Dim mypair1 As pair =  fbstd.Pair(@val1,TypeTraitsInteger,@val2,TypeTraitsString)



 Print  PairToString(@mypair1)


Sleep





 