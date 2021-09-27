#Include "containers/map.bi"




MMapTemplate(UInteger , String)


Dim temp1 As TMAPUINTEGERSTRING
Dim tempptr As MAPNODEUINTEGERSTRING




temp1.insert(0,"test1")
temp1.insert(1,"test2")
temp1.insert(2,"test3")
temp1.insert(3,"test4")
temp1.insert(4,"test5")
temp1.insert(5,"test6")
temp1.insert(6,"test7")
temp1.insert(7,"test8")
temp1.insert(8,"test9")

tempptr = temp1.find(5)



Print *(tempptr.ndata)


