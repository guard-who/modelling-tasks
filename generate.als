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
  to : one Class
}

fact validLimitsAssoc {
  all a : Assoc | smallerOrSame [a.fromLower, a.fromUpper]
  all a : Assoc | smallerOrSame [a.toLower, a.toUpper]
}

fact validLimitsComposition {
  all c : Composition | (c.toLower = Zero or c.toLower = One) and c.toUpper = One
}

sig Inheritance extends Connection {}

abstract sig Assoc extends Connection {
  fromLower : one Limit,
  fromUpper : one Limit,
  toLower : one Limit,
  toUpper : one Limit
}

sig Aggregation extends Assoc {}
sig Association extends Assoc {}
sig Composition extends Assoc {}

fact noSelfConnection {
  all c: Connection | c.from != c.to
}

fact noDoubleConnection {
  all c, c' : Connection | c != c' implies c.from = c'.from implies c.to != c'.to
}

fact noReverseConnection {
  all c, c' : Connection | c != c' implies c.to = c'.from implies c.from != c'.to
}

fact noDoubleInheritance {
  all i, i' :  Inheritance | i != i' implies i.to != i'.to
}

fun connection (restriction : set Connection) : Class -> Class {
  ((~from :> restriction) . (restriction <: to))
}

fact noInheritanceCycles {
  let inheritance = connection [Inheritance] |
  all c : Class | not c in c.^inheritance
}

fact noCompositionCycles {
  let inheritance = connection [Inheritance],
      composition = connection [Composition] |
  all c : Class | not c in c.^(*inheritance.composition).*~inheritance
}

fact nonEmptyInstancesOnly {
  some Connection
}

pred cd {
  0 <= #{ Association } and #{ Association } <= 2
  0 <= #{ Aggregation } and #{ Aggregation } <= 2
  0 <= #{ Composition } and #{ Composition } <= 2
  1 <= #{ Inheritance } and #{ Inheritance } <= 2
  4 <= #{ Class }
  3 <= #{ Connection }
}

run cd for 5 Connection, 4 Class
