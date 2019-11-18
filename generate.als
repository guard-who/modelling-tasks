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

pred smaller (l, l' : Limit) {
  not l = l' and smallerOrSame [l, l']
}

abstract sig Relationship {
  from : one Class,
  to : one Class
}

pred validLimitsAssoc [a : Assoc] {
  smallerOrSame [a.fromLower, a.fromUpper]
  smallerOrSame [a.toLower, a.toUpper]
  a.fromLower = Zero implies a.fromUpper != Zero
  a.toLower = Zero implies a.toUpper != Zero
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

pred isRedEdge [r : Relationship, rs : set Relationship, is : set Inheritance] {
  let subs = ~(relationship [is]) |
  some r' : rs - Inheritance |
    (r.from != r'.from or r.to != r'.to)
      and (r'.from in r.from.*subs and (r'.to in r.to.*subs or r.to in r'.to.*subs)
        or r'.to in r.to.*subs and (r'.from in r.from.*subs or r.from in r'.from.*subs))
    or (r.from != r'.to or r.to != r'.from)
      and (r'.to in r.from.*subs and (r'.from in r.to.*subs or r.from in r'.from.*subs)
        or r'.from in r.to.*subs and (r'.to in r.from.*subs or r.from in r'.to.*subs))
}

fact nonEmptyInstancesOnly {
  some Relationship
}

lone sig Change {
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

pred sameToLimits [a, a' : Assoc] {
  a.toLower = a'.toLower
  a.toUpper = a'.toUpper
}

pred sameLimits [a, a' : Assoc] {
  sameFromLimits [a, a']
  sameToLimits [a, a']
}

pred flip [c : Change] {
  c.add.from = c.remove.to and c.add.to = c.remove.from
  sameKind [c.add, c.remove]
  c.add in Assoc implies sameLimits [c.add, c.remove]
}

pred changedKind [c : Change] {
  sameDirection [c.add, c.remove]
  not sameKind [c.add, c.remove]
  c.add in Assoc and c.remove in Assoc implies sameFromLimits [c.add, c.remove]
  c.add in Assoc and c.remove in Assoc
    and (validLimitsComposition [c.remove] or not c.add in Composition)
    implies sameLimits [c.add, c.remove]
}

pred increasedFromRange [a, a' : Assoc] {
  smaller [a'.fromLower, a.fromLower] or smaller [a.fromUpper, a'.fromUpper]
  sameToLimits [a, a']
}

pred increasedToRange [a, a' : Assoc] {
  smaller [a'.toLower, a.toLower] or smaller [a.toUpper, a'.toUpper]
  sameFromLimits [a, a']
}

pred increasedRange [a, a' : Assoc] {
  increasedFromRange [a, a'] iff not increasedToRange [a, a']
}

pred changedRange [a, a' : Assoc] {
  increasedRange [a, a'] iff not increasedRange [a', a]
}

fun shiftBy [l, shift : Limit] : lone Limit {
  shift = Zero implies l
  else shift = One implies (l = Zero implies One else l = One implies Two else l = Two implies Star else none)
  else shift = Two implies (l = Zero implies Two else l = One implies Star else none)
  else shift = Star implies (l = Zero implies Star else none)
  else none
}

pred shiftLimits [l1, l1', l2, l2' : Limit] {
  one shift : Limit {
    shift != Zero
    l1 = shiftBy [l2, shift]
    l1' = shiftBy [l2', shift]
  }
}

pred shiftedRangeUp [a, a' : Assoc] {
  shiftLimits [a.fromLower, a.fromUpper, a'.fromLower, a'.fromUpper] and sameToLimits [a, a']
  iff not (shiftLimits [a.toLower, a.toUpper, a'.toLower, a'.toUpper] and sameFromLimits [a, a'])
}

pred shiftedRange [a, a' : Assoc] {
  shiftedRangeUp [a, a'] iff not shiftedRangeUp [a', a]
}

pred changedLimit [c : Change] {
  sameDirection [c.add, c.remove]
  sameKind [c.add, c.remove]
  validLimitsAssoc [c.add]
  c.add in Composition implies validLimitsComposition [c.add]
  shiftedRange [c.add, c.remove] iff not changedRange [c.add, c.remove]
}

pred change [c : Change] {
  some c.add + c.remove
  no c.add or not c.add in Relationship - Change.add
  c.remove in Relationship - Change.add
  let c1 = changedLimit [c],
      c2 = changedKind [c],
      c3 = flip [c] |
    one c.add and one c.remove iff c1 implies not c2 and not c3 else c2 iff not c3
}

one sig A extends Class {}
one sig B extends Class {}
one sig C extends Class {}
one sig D extends Class {}
/*
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
  y.toUpper = One
  z.from = C
  z.to = B
  z.fromLower = Two
  z.fromUpper = Two
  z.toLower = One
  z.toUpper = One
  i0.from = D
  i0.to = B
  Class = A + B + C + D
  Relationship - Change.add = x + y + z + i0
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
*/

pred cd {
  all c : Assoc | validLimitsAssoc [c]
  all c : Composition | validLimitsComposition [c]
  all c : Relationship | noSelfRelationship [c]
  all c, c' : Relationship | noDoubleRelationship [c, c']
  all c, c' : Relationship | noReverseRelationship [c, c']
  all i, i' : Inheritance | noDoubleInheritance [i, i']
  noInheritanceCycles [Inheritance]
  noCompositionCycles [Inheritance, Composition]
  some r : Relationship - Inheritance | isRedEdge [r, Relationship, Inheritance]
  0 <= #Association and #Association <= 2
  0 <= #Aggregation and #Aggregation <= 2
  0 <= #Composition and #Composition <= 2
  1 <= #Inheritance and #Inheritance <= 2
  4 <= #Class
  3 <= #Relationship
  no Change
}

run cd for 6 Relationship, 4 Class
