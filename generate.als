abstract sig Class {}

abstract sig Limit {}

one sig Zero extends Limit {}
one sig One extends Limit {}
one sig Two extends Limit {}
one sig Star extends Limit {}

pred smallerOrSame (l, l' : Limit) {
  not (l = Star)
  and (l = Two implies (l' = Two or l' = Star))
  and (l = One implies not l' = Zero)
}

abstract sig Connection {
  from : one Class,
  to : one Class,
  next : set Connection
}

fact validLimitsConnection {
  all c : Connection | smallerOrSame [c.fromLower, c.fromUpper]
  all c : Connection | smallerOrSame [c.toLower, c.toUpper]
}

sig Inheritance extends Connection {
  nextInheritance : set Inheritance
}
sig Assoc extends Connection {
  fromLower : one Limit,
  fromUpper : one Limit,
  toLower : one Limit,
  toUpper : one Limit
}

sig Aggregation extends Assoc {}
sig Association extends Assoc {}
sig Composition extends Assoc {
  nextComposition : set (Composition + Inheritance)
}

fact noSelfConnection {
  all c: Connection | c.from != c.to
}

fact noDoubleConnection {
  all c, c' : Connection | c != c' implies c.from = c'.from implies c.to != c'.to
}

fact noReverseConnection {
  all c, c' : Connection | c != c' implies c.to = c'.from implies c.from != c'.to
}

fact nextConnection {
  all c, c' : Connection | not (c in c.next) and (c' in c.next iff c.to = c'.from)
}

fact nextInheritanceValues {
  all i : Inheritance | i.nextInheritance = i.next & Inheritance
}

fact nextCompositionValues {
  all c : Composition | c.nextComposition = c.next & (Composition + Inheritance)
}

fact noDoubleInheritance {
  all i, i' :  Inheritance | i != i' implies i.to != i'.to
}

fact noInheritanceCycles {
  all i : Inheritance | not i.from in (i.*nextInheritance).to
}

fact noCopositionCycles {
  all c : Composition | not c.from in (c.*nextComposition).to
}

fact nonEmptyInstancesOnly {
  some Inheritance
}

pred show {}

run show for 4
