Ratio {
	var <num, <den, <description, <fav, <ratioAsString;
	var <value, <limit;
	classvar gannRatios;

	*new {|num = 1, den = 1, description = "", fav = 0, ratioAsString|
		^super.newCopyArgs(num, den, description, fav, ratioAsString).initRatio
	}

	// ***** Instance method: initRatio
	initRatio {
		if(ratioAsString.notNil, {
			if(ratioAsString.contains("/"), {
				#num, den = ratioAsString.split($/).collect{|i| i.asInteger};
			});
		});
			
		if(den != 0, {
			value = num / den;
			limit = this.getLimit(num, den);
		}, {
			"Illegal denominator!".postln;
			value = 1
		});

		if(gannRatios.isNil, {
			this.loadGannRatios()
		});
	}

	// ***** Instance method: copy
	copy {
		^Ratio(num, den);
	}

	// ***** Instance method: setDescription
	setDescription {|aString|
		description = aString
	}

	// ***** setRatioWithString
	setRatioWithString {|aString|
		if(aString.contains("/"), {
			#num, den = aString.split($/).collect{|i| i.asInteger};

			if(den != 0, {
				value = num / den;
				limit = this.getLimit(num, den);
			}, {
				"Illegal denominator!".error;
				value = 1
			});
		}, {
			"Illegal ratio name!".error
		});
	}

	// ***** Instance method: setFav
	setFav {|val|
		fav = val.clip(0,1).asInteger;
	}

	// ***** Instance method: num_
	num_{|val|
		num = val.asInteger;
		limit = this.limit(num, den);
	}

	// ***** Instance method: den_
	den_{|val|
		den = val.asInteger;
		limit = this.limit(num, den);
	}

	// ***** Instance method: print
	print {
		var str = num.asString ++ "/" ++ den.asString;

		^str
	}

	// ***** Instance method: asNum
	asNum {
		^(num / den)
	}

	// ***** Instance method: asString
	asString {
		^this.print()
	}

	// ***** Instance method: asMIDI
	asMIDI {
		var nn, dev;

		var asmidi = this.asNum().ratiomidi;

		nn = asmidi.round(1).asInteger;
		dev = asmidi - nn;

		^[nn, dev]
	}

	// ***** Instance method: getLimit
	getLimit { | num, den |
		var numLimit, denLimit;
		var numFactors = num.factors.reverse;
		var denFactors = den.factors.reverse;

		if(numFactors.size == 0, {
			numLimit = 1
		}, {
			numLimit = numFactors[0]
		});

		if(denFactors.size == 0, {
			denLimit = 1
		}, {
			denLimit = denFactors[0]
		});

		if(numLimit > denLimit, {
			^numLimit
		}, {
			^denLimit
		});
	}

	// ***** Instance method: getLimit
	multiply {|am|
		if(am.class == Ratio, {
			^Ratio(num * am.num, den * am.den).simplify
		}, {
			if(am.class == Integer, {
				^Ratio((num * am).asInteger, den).simplify
			})
		});
	}

	// ***** Instance method: divide
	divide {|am|
		if(am.class == Ratio, {
			^Ratio(num * am.den, den * am.num).simplify
		}, {
			if(am.class == Integer, {
				^Ratio(this.num, this.den * am).simplify
			})
		})
	}

	// ***** Instance method: add
	add {|am|
		if(am.class == Ratio, {
			^Ratio(
				(this.num * am.den) + (am.num * this.den),
				this.den * am.den
			).simplify
		}, {
			if(am.class == Integer, {
				^Ratio(
					this.num + (am * this.den),
					this.den
				).simplify
			})
		})
	}

	+ {|am| ^this.add(am) }
	/ {|am| ^this.divide(am) }
	* {|am| ^this.multiply(am) }

	// ***** Instance method: simplify
	simplify {
		var div = gcd(num, den);
		num = num.div(div);
		den = den.div(div);
		^this
	}

	// ***** Instance method: pow
	pow {|aInteger|
		^Ratio(num.pow(aInteger.asInteger).asInteger, den.pow(aInteger.asInteger).asInteger)
	}

	// ***** Instance method: forceOctave
	forceOctave {
		while({den > num}, {
			num = num * 2
		});
		while({num > (den * 2)}, {
			den = den * 2
		});
		^this.simplify()
	}

	// ***** Instance method: invert
	invert { ^Ratio(den, num) }

	// ***** Instance method: am
	am {
		// arithmetic mean
		^this.add(1).divide(2).simplify()
	}
	// ***** Instance method: hm
	hm {
		// harmonic mean
		^this.multiply(2).divide(this.add(1)).simplify()
	}
	// ***** Instance method: dt
	dt {
		// difference tone
		^Ratio(num - den, den)
	}
	// ***** Instance method: st
	st {
		// sum tone
		^Ratio(num + den, den).simplify
	}
	// ***** Instance method: collectionWithCommonDen
	collectionWithCommonDen {|aList|
		var mult = [];
		var new = [];
		aList = aList.asArray.insert(0, this);
		aList.do{|r, i|
			var thisMult = [];
			aList.do{|r, j|
				if(i != j, {
					thisMult = thisMult.add(r.den)
				})
			};
			mult = mult.add(thisMult)
		};

		mult.do{|l, i|
			var num = aList[i].num.copy;
			var den = aList[i].den.copy;
			l.do{|m| num = num * m; den = den * m};
			new = new.add(Ratio(num, den))
		};

		^this.simplifyCollectionCommonDenominator(new);
	}

	// ***** Instance method: simplifyCollectionCommonDenominator	
	simplifyCollectionCommonDenominator {|aList|
		var numbers = [aList[0].den] ++ aList.collect{|r| r.num};
		var den = numbers[0];

		numbers.do{|item|
			den = gcd(den, item)
		};

		^aList.collect{|item| Ratio(item.num.div(den), item.den.div(den))}
	}

	// ***** Instance method: collectionAsHarmonics
	collectionAsHarmonics {|aList|
		var mult = [];
		var harm = [];
		aList = aList.asArray.insert(0, this);
		aList.do{|r, i|
			var thisMult = [];
			aList.do{|r, j|
				if(i != j, {
					thisMult = thisMult.add(r.den)
				})
			};
			mult = mult.add(thisMult)
		};

		mult.do{|l, i|
			var num = aList[i].num;
			l.do{|m| num = num * m};
			harm = harm.add(num)
		};

		^this.simplifyHarmonics(harm)
	}

	// ***** Instance method: simplifyHarmonics	
	simplifyHarmonics {|aList|
		var den = aList[0];

		aList.do{|item|
			den = gcd(den, item)
		};

		^aList.collect{|item| item.div(den)}
	}

	// * Instance method: loadGannRatios
	loadGannRatios {
		gannRatios = File(Platform.userExtensionDir ++ "/ratio/gann.json", "r").readAllString.parseJSON;
	}

	// * Instance method: getGannDesc
	getGannDesc {
		var forced = Ratio(num, den).forceOctave();
		if(gannRatios[forced.asString].notNil, {
			^gannRatios[forced.asString]
		}, {
			^""
		})
	}
}

// Local variables:
// eval: (outshine-mode t)
// End: