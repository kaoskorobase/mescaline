Precord : FilterPattern
{
	var <>func, <>count;
	*new { | pattern, func, count=inf |
		^super.new(pattern).func_(func).count_(count)
	}
	embedInStream { | inval |
		var stream = pattern.asStream;
		var countStream = Pn(true, count).asStream;
		var outval, list = Array.new, value;
		while { countStream.next.notNil
				and: { (outval = stream.next(inval)).notNil } }
		{
			list = list.add(outval.copy);
			inval = outval.yield;
		};
		^if (list.notEmpty, { func.value(list).embedInStream(inval) })
	}
}

+Pattern
{
	mirror {
		^Precord(this, { | list |
			p { | inval |
				forBy (list.size - 2, 0, -1) { |i|
					inval = list[i].yield;
				};
				inval
			}
		})
	}
	mirror1 {
		^Precord(this, { | list |
			p { | inval |
				forBy (list.size - 2, 1, -1) { |i|
					inval = list[i].yield;
				};
				inval
			}
		})
	}
	mirror2 {
		^Precord(this, { | list |
			p { | inval |
				forBy (list.size - 1, 0, -1) { |i|
					inval = list[i].yield;
				};
				inval
			}
		})
	}
	scramble {
		^Precord(this, { | list | Pshuf(list) })
	}
	rotate { | n=1 |
		^Precord(this, { | list | Pseq(list.rotate(n)) })
	}
	pyramid { | n=1 |
		^Precord(this, { | list | Pseq(list.pyramid(n)) })
	}
	permute { | n=0, count=1, step=1 |
		^Precord(this, { | list |
			p { | inval |
				Pseries(n, step, count).asStream.do { | i |
					list.permute(i).do { |x|
						inval = x.yield;
					};
				}
			}
		})
	}
}
