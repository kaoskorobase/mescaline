//Precord : FilterPattern
//{
//	embedInStream { | inval |
//		var stream = pattern.asStream;
//		var outval, list = Array.new, value;
//		while { (outval = stream.next(inval)).notNil } {
//			value = this.filterValue(outval.copy);
//			value !? { list = list.add(value) };
//			inval = outval.yield;
//		};
//		^this.embedList(list, inval);
//	}
//	filterValue { | inval | ^inval }
//	embedList { | inval | ^inval }
//}
//
//Pmirror : Precord
//{
//	embedList { | list, inval |
//		forBy (list.size - 2, 0, -1) { |i|
//			inval = list[i].yield;
//		};
//		^inval
//	}
//}
//
//Pmirror1 : Pmirror
//{
//	embedList { | list, inval |
//		forBy (list.size - 2, 1, -1) { |i|
//			inval = list[i].yield;
//		};
//		^inval
//	}
//}
//
//Pmirror2 : Pmirror
//{
//	embedList { | list, inval |
//		forBy (list.size - 1, 0, -1) { |i|
//			inval = list[i].yield;
//		};
//		^inval
//	}
//}
//
//+Pattern
//{
//	mirror  { ^Pmirror(this) }
//	mirror1 { ^Pmirror1(this) }
//	mirror2 { ^Pmirror2(this) }
//}

