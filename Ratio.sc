Ratio {
	var <num, <den, <description, <fav, <ratioAsString;
	var <value, <limit;

	*new {|num = 1, den = 1, description = "", fav = 0, ratioAsString|
		^super.newCopyArgs(num, den, description, fav, ratioAsString).initRatio
	}

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
	}

	copy {
		^Ratio(num, den);
	}

	num_{|val|
		num = val.asInteger
	}

	den_{|val|
		den = val.asInteger
	}

	print {
		var str = num.asString ++ "/" ++ den.asString;

		^str
	}

	asNum {
		^(num / den)
	}

	asString {
		^this.print()
	}

	asMIDI {
		var nn, dev;

		var asmidi = this.asNum().ratiomidi;

		nn = asmidi.round(1).asInteger;
		dev = asmidi - nn;

		^[nn, dev]
	}

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

	multiply {|am|
		if(am.class == Ratio, {
			^Ratio(num * am.num, den * am.den).simplify
		}, {
			if(am.class == Integer, {
				^Ratio((num * am).asInteger, den).simplify
			})
		});
	}

	divide {|am|
		if(am.class == Ratio, {
			^Ratio(num * am.den, den * am.num).simplify
		}, {
			if(am.class == Integer, {
				^Ratio(this.num, this.den * am).simplify
			})
		})
	}

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

	simplify {
		var div = gcd(num, den);
		num = num.div(div);
		den = den.div(div);
		^this
	}

	pow {|aInteger|
		^Ratio(num.pow(aInteger.asInteger).asInteger, den.pow(aInteger.asInteger).asInteger)
	}

	forceOctave {
		while({den > num}, {
			num = num * 2
		});
		while({num > (den * 2)}, {
			den = den * 2
		});
		^this.simplify()
	}

	invert { ^Ratio(den, num) }

	am {
		^this.add(1).divide(2).simplify()
	}

	hm {
		^this.multiply(2).divide(this.add(1)).simplify()
	}

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

	simplifyCollectionCommonDenominator {|aList|
		var numbers = [aList[0].den] ++ aList.collect{|r| r.num};
		var den = numbers[0];

		numbers.do{|item|
			den = gcd(den, item)
		};

		^aList.collect{|item| Ratio(item.num.div(den), item.den.div(den))}
	}

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

	simplifyHarmonics {|aList|
		var den = aList[0];

		aList.do{|item|
			den = gcd(den, item)
		};

		^aList.collect{|item| item.div(den)}
	}
}