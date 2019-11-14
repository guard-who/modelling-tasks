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

abstract sig Relationship {
  from : one Class,
  to : one Class
}

pred validLimitsAssoc [a : Assoc] {
  smallerOrSame [a.fromLower, a.fromUpper]
  smallerOrSame [a.toLower, a.toUpper]
}

pred validLimitsComposition [c : Composition] {
  (c.toLower = Zero or c.toLower = One) and c.toUpper = One
}

sig Inheritance extends Relationship {}

abstract sig Assoc extends Relationship {
  fromLower : one Limit,
  fromUpper : one Limit,
  toLower : one Limit,
  toUpper : one Limit
}

sig Aggregation extends Assoc {}
sig Association extends Assoc {}
sig Composition extends Assoc {}

pred noSelfRelationship [c : Relationship] {
  c.from != c.to
}

pred noDoubleRelationship [c, c' : Relationship] {
  c != c' implies c.from = c'.from implies c.to != c'.to
}

pred noReverseRelationship [c, c' : Relationship] {
  c != c' implies c.to = c'.from implies c.from != c'.to
}

pred noDoubleInheritance [i, i' : Inheritance] {
  i != i' implies i.to != i'.to
}

fun relationship (restriction : set Relationship) : Class -> Class {
  ((~from :> restriction) . (restriction <: to))
}

pred noInheritanceCycles {
  let inheritance = relationship [Inheritance] |
  all c : Class | not c in c.^inheritance
}

pred noCompositionCycles {
  let inheritance = relationship [Inheritance],
      composition = relationship [Composition] |
  all c : Class | not c in c.^(*inheritance.composition).*~inheritance
}

fact nonEmptyInstancesOnly {
  some Relationship
}

pred cd {
  all c : Assoc | validLimitsAssoc [c]
  all c : Composition | validLimitsComposition [c]
  all c : Relationship | noSelfRelationship [c]
  all c, c' : Relationship | noDoubleRelationship [c, c']
  all c, c' : Relationship | noReverseRelationship [c, c']
  all i, i' : Inheritance | noDoubleInheritance [i, i']
  noInheritanceCycles
  noCompositionCycles
  0 <= #{ Association } and #{ Association } <= 2
  0 <= #{ Aggregation } and #{ Aggregation } <= 2
  0 <= #{ Composition } and #{ Composition } <= 2
  1 <= #{ Inheritance } and #{ Inheritance } <= 2
  4 <= #{ Class }
  3 <= #{ Relationship }
}

run cd for 6 Relationship, 4 Class
