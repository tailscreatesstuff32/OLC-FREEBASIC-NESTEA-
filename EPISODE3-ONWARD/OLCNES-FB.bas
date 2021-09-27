#Include "nes/olc6502 .bi"


Print lookup(0).Name;":";
lookup(0).operate()
lookup(0).addrmode()
Print ":";lookup(0).cycles

Print lookup(1).Name;":";
lookup(1).operate()
lookup(1).addrmode()
Print ":";lookup(1).cycles

Print lookup(2).Name;":";
lookup(2).operate()
lookup(2).addrmode()
Print ":";lookup(2).cycles

Print lookup(3).Name;":";
lookup(3).operate()
lookup(3).addrmode()
Print ":";lookup(3).cycles

Sleep

