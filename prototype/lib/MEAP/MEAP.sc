MEAPUnit
{
	var <fileName, <startTime, <duration, <featureSpecs, <features;
	
	*new { | fileName, startTime, duration, featureSpecs, features |
		^super.new.init(
			fileName,
			startTime, duration,
			featureSpecs, features)
	}
	*fromStream { | fileName, featureSpecs, stream |
		var startTime, endTime;
		startTime = stream.next;
		endTime = stream.next;
		^this.new(
			fileName, startTime, endTime,
			featureSpecs,
			featureSpecs.collectAs(
				{ |x| x.name -> x.valueFrom(stream) },
				MEAPFeature.dictionaryClass
			)
		)
	}
	init { | ... args |
		# fileName, startTime, duration, featureSpecs, features = args;
		features.put(\startTime, startTime);
		features.put(\duration, duration);
	}
	keys {
		^featureSpecs.collect(_.name)
	}
	values {
		var result = [];
		this.do { | key, value |
			result = result.add(value);
		};
		^result
	}
	scalarKeys {
		^featureSpecs.select(_.isScalar).collect(_.name)
	}
	scalarValues {
		var result = [];
		this.scalarKeys.do { | key |
			result = result.add(features[key]);
		};
		^result
	}
	at { | key |
		^features.at(key)
	}
	do { | function |
		this.keys.do { | key, i |
			function.value(key, features[key], i);
		}
	}
}

MEAPFeature
{
	var <name, <size, <scale;
	
	*dictionaryClass {
		^IdentityDictionary
	}
	*new { | name, size, scale=1.0 |
		^super.newCopyArgs(name, size, scale)
	}
	*fromString { | spec |
		var error = { Error("couldn't parse feature spec").throw };
		var scale, name, size;
		# scale, spec = spec.split($*);
		if (scale.isNil, error);
		# name, spec = spec.split($();
		if (name.isNil, error);
		size = spec.split($)).first;
		if (size.isNil, error);
		^this.new(name.split($.).last.asSymbol, size.asInteger, scale.asFloat)
	}
	isScalar {
		^this.size == 1
	}
	printOn { | aStream |
		aStream
			<< this.class.name
//			<< ".fromString" << $(
//			<< "\"%*%(%)\"".format(scale, name, size)
//			<< $);
			<< $(;
			[name, size, scale].printItemsOn(aStream);
			aStream << $);
	}
	valueFrom { | stream |
		var result = stream.nextN(this.size);
		if (result.size != this.size) {
			Error("end of stream while parsing feature values").throw;
		};
		^if (this.isScalar)
			{ result.first }
			{ result }
	}
}

MEAPSoundFile
{
	var <fileName, <sampleRate, <numChannels, <numFrames;
	*new { | fileName |
		^super.newCopyArgs(fileName).prInit
	}
	printOn { | aStream |
		aStream << this.class.name << $(;
		[fileName, this.sampleRate, this.numChannels, this.numFrames].printItemsOn(aStream);
		aStream << $);
	}
	== { | other |
		^fileName == other.fileName
	}
	hash {
		^fileName.hash
	}
	prInit {
		var sf = SoundFile.openRead(fileName);
		sampleRate = sf.sampleRate;
		numChannels = sf.numChannels;
		numFrames = sf.numFrames;
		sf.close;
	}
}

MEAP
{
	var <baseDir, <files, <units;
	
	*new { | baseDir=nil |
		^super.newCopyArgs(baseDir, Set.new, Array.new)
	}
	*readFile { | path, baseDir=nil |
		^this.new(baseDir).readFile(path)
	}
	expandFileName { | path |
		^if (baseDir.notNil)
			{ baseDir +/+ PathName(path).fileName }
			{ path }.replace("%20", " ");
	}
	readFile { | path |
		var contents = FileReader.read(path);
		var fspecs = contents.detect({ |x| x.find(["#", "Features:"]) == 0 }) ?? {
			Error("couldn't find features header").throw;
		};
		var features = fspecs[2..].reject(_.isEmpty).collect(MEAPFeature.fromString(_));
		Pseq(contents).reject({ |line|
			line.isEmpty or: { line.first.beginsWith("#") }
		}).asStream.do { | line |
			var stream = Pseq(line).asStream;
			var fileName = this.expandFileName(stream.next);
			files.add(MEAPSoundFile(fileName));
			units = units.add(MEAPUnit.fromStream(fileName, features, stream.collect(_.asFloat)));
		};
	}
	unitsSortedBy { | feature, function |
		var sortFunc = if (function.isNil)
						{ { |a,b| a.features[feature] < b.features[feature] } }
						{ { |a,b| function.(a.features[feature], b.features[feature]) } };
		^SortedList(units.size, sortFunc).addAll(units);
	}
	segments {
		^this.unitsSortedBy(\startTime)
	}
	fileNames {
		^files.collect(_.fileName)
	}
	allocBuffers { | server |
		^this.fileNames.collectAs({ |x| x.postln -> Buffer.read(server, x) }, Dictionary)
	}
	asKDTree {
		^KDTree(units.collect { |x| x.scalarValues ++ [x] }, lastIsLabel: true)
	}
}

/*
	x = MEAPUnit(1.0, 2.0, Dictionary["foo" -> 1.0, "gee" -> 3.4]);
	f = [MEAPFeature("foo", 1), MEAPFeature("gee", 1)];
	y = MEAPUnit.fromStream(f, Pseq([1.0, 2.0, 1.0, 3.4]).asStream);
	x == y;	// false
	x === y;	// false
	
	x = MEAPFeature.fromString("1.0*com.meapsoft.featextractors.AvgMelSpec(40)");
	[x.name, x.size, x.scale] == ["com.meapsoft.featextractors.AvgMelSpec", 40, 1.0]; // true
replace

	x = MEAPPhrase.readFile("/Users/sk/src/MEAPsoft-1.1.1/data/test.feat");
	x.fileName;
	x.units.collect(_.features);
	
	x.fileInfo;
	x.buffer;
	x.asKDTree.dumpTree;
*/
