#include "containers/MAP.bi"

MMapTemplate(string , string)

Sub PostOrder(p As MAPNODESTRINGSTRING Ptr)

     If (p<>0) Then
     	
     	

        p = p->pLeft

       'p =  p->pRight
       
   
         Print *(p->nKey) , *(p->nData)

     Endif

End Sub

Dim p As TMAPSTRINGSTRING

For i As Long = 1 To 10

   p.Insert("Node" & str(i) , str(i*1000))

Next


PostOrder(p.pRoot)


sleep


