Pdelay : FilterPattern
{
	var <>delay;

	*new { | delay, pattern |
		^super.new(pattern).delay_(delay)
	}
	*humanize { | pattern, deviation(1/32) |
		deviation = deviation.abs.asFloat;
		^this.new(Pfunc { deviation + bilinrand(deviation) }, pattern)
	}
	*swing { | pattern, ratio=0.25, grid=0.125, epsilon=1e-6 |
		^this.new(Pfunc {
			var beatInBar = thisThread.clock.beatInBar;
			var gridPos = beatInBar.round(grid);
			var dif = absdif(beatInBar, gridPos);
			if (dif < epsilon and: { (gridPos / grid).asInteger.odd })
			{ ratio * grid }
			{ 0.0 }
		}, pattern)
	}
	storeArgs { ^[delay, pattern] }
	embedInStream { | inEvent |
		var now = 0.0, rest = (type: \rest);
		var pq, evtStream, delayStream;
		var processFunc = {
			var outEvent, nextTime, delayTime;
			inEvent ?? { evtStream.next(nil); ^nil.yield };
			(outEvent = evtStream.next(inEvent)) !? {
				// schedule next stream event
				pq.put(now + outEvent.delta, processFunc);
				(delayTime = delayStream.next) !? {
					// schedule delayed event
					pq.put(now + delayTime, {
						nextTime = pq.topPriority;
						if (nextTime.notNil) {
							// fix delta to next queue event
							outEvent.put(\delta, nextTime - now);
						};
						inEvent = outEvent.yield;
						inEvent ?? { evtStream.next(nil); ^nil.yield };
						now = nextTime;
					});
				};
			};
			(nextTime = pq.topPriority) !? {
				// wait for next queue event
				inEvent = rest.copy.put(\delta, nextTime - now).yield
			};
			now = nextTime;
		};

		evtStream = pattern.asStream;
		delayStream = delay.asStream;

		pq = PriorityQueue.new;
		pq.put(now, processFunc);

		while { pq.notEmpty } { pq.pop.value };
		^inEvent
	}
}

+ Pattern
{
	delay { | delay |
		^Pdelay(delay, this)
	}
}