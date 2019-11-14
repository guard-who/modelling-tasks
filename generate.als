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

pred validLimitsComposition [a : Assoc] {
  (a.toLower = Zero or a.toLower = One) and a.toUpper = One
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

pred noInheritanceCycles [is : set Inheritance] {
  let inheritance = relationship [is] |
  all c : Class | not c in c.^inheritance
}

pred noCompositionCycles [is : set Inheritance, cs : set Composition] {
  let inheritance = relationship [is],
      composition = relationship [cs] |
  all c : Class | not c in c.^(*inheritance.composition).*~inheritance
}

fact nonEmptyInstancesOnly {
  some Relationship
}

one sig Change {
  add : lone Relationship,
  remove : lone Relationship
}

pred sameDirection [r, r' : Relationship] {
  r.from = r'.from and r.to = r'.to
}

pred sameKind [r, r' : Relationship] {
  r in Association iff r' in Association
  r in Aggregation iff r' in Aggregation
  r in Composition iff r' in Composition
  r in Inheritance iff r' in Inheritance
}

pred sameFromLimits [a, a' : Assoc] {
  a.fromLower = a'.fromLower
  a.fromUpper = a'.fromUpper
}

pred sameLimits [a, a' : Assoc] {
  sameFromLimits [a, a']
  a.toLower = a'.toLower
  a.toUpper = a'.toUpper
}

pred flip [c : Change] {
  c.add.from = c.remove.to and c.add.to = c.remove.from
  sameKind [c.add, c.remove]
  c.add in Assoc implies sameLimits [c.add, c.remove]
}

pred changeKind [c : Change] {
  sameDirection [c.add, c.remove]
  not sameKind [c.add, c.remove]
  c.add in Assoc and c.remove in Assoc implies sameFromLimits [c.add, c.remove]
  c.add in Assoc and c.remove in Assoc
    and (validLimitsComposition [c.remove] or not c.add in Composition)
    implies sameLimits [c.add, c.remove]
}

pred change [c : Change] {
  some (c.add + c.remove)
  c.add != c.remove
  c.remove in Relationship - Change.add
  one c.add and one c.remove iff changeKind [c] or flip [c] // Limit change missing
}

one sig A extends Class {}
one sig B extends Class {}
one sig C extends Class {}
one sig D extends Class {}

one sig x extends Association {}
one sig y extends Association {}
one sig z extends Composition {}
one sig i0 extends Inheritance {}

pred cd {
  x.from = C
  x.to = D
  x.fromLower = Two
  x.fromUpper = Star
  x.toLower = Two
  x.toUpper = Star
  y.from = D
  y.to = A
  y.fromLower = Zero
  y.fromUpper = One
  y.toLower = Zero
  y.toUpper = Zero
  z.from = C
  z.to = B
  z.fromLower = Two
  z.fromUpper = Two
  z.toLower = One
  z.toUpper = One
  i0.from = D
  i0.to = B
  Relationship = x + y + z + i0 + Change.add
  change [Change]
  let Assoc' = Assoc - Change.remove,
      Composition' = Composition - Change.remove,
      Relationship' = Relationship - Change.remove,
      Inheritance' = Inheritance - Change.remove {
    all c : Assoc' | validLimitsAssoc [c]
    all c : Composition' | validLimitsComposition [c]
    all c : Relationship' | noSelfRelationship [c]
    all c, c' : Relationship' | noDoubleRelationship [c, c']
    all c, c' : Relationship' | noReverseRelationship [c, c']
    all i, i' : Inheritance' | noDoubleInheritance [i, i']
    noInheritanceCycles [Inheritance']
    noCompositionCycles [Inheritance', Composition']
  }
}

run cd for 6 Relationship, 4 Class
