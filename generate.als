abstract sig Class {}

abstract sig Limit {}

one sig Zero extends Limit {}
one sig One extends Limit {}
one sig Two extends Limit {}
one sig Star extends Limit {}

pred smallerOrSame [l, l' : Limit] {
  not (l = Star)
  and (l = Two implies (l' = Two or l' = Star))
  and (l = One implies not l' = Zero)
}

pred smaller [l, l' : Limit] {
  not l = l' and smallerOrSame [l, l']
}

abstract sig Relationship {
  from : one Class,
  to : one Class
}

pred validFromLimitsAssoc [a : Assoc] {
  smallerOrSame [a.fromLower, a.fromUpper]
  a.fromLower = Zero implies a.fromUpper != Zero
}

pred validToLimitsAssoc [a : Assoc] {
  smallerOrSame [a.toLower, a.toUpper]
  a.toLower = Zero implies a.toUpper != Zero
}

pred validLimitsAssoc [a : Assoc] {
  validFromLimitsAssoc [a]
  validToLimitsAssoc [a]
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

pred selfRelationship [r : Relationship] {
  r.from = r.to
}

pred doubleRelationship [r, r' : Relationship] {
  r != r' and r.from = r'.from and r.to = r'.to
}

pred reverseRelationship [r, r' : Relationship] {
  r != r' and r.to = r'.from and r.from = r'.to
}

pred multipleInheritance [i, i' : Inheritance] {
  i != i' and i.from = i'.from
}

fun relationship [restriction : set Relationship] : Class -> Class {
  ((~from :> restriction) . (restriction <: to))
}

pred noInheritanceCycles [is : set Inheritance] {
  let inheritance = relationship [is] |
  no c : Class | c in c.^inheritance
}

pred noCompositionCycles [is : set Inheritance, cs : set Composition] {
  let inheritance = relationship [is],
      composition = relationship [cs] |
  no c : Class | c in c.^(*inheritance.composition).*~inheritance
}

pred markedEdgeCriterion [xFrom, xTo, yFrom, yTo : Class, is : set Inheritance] {
  let subs = ~(relationship [is]) |
    (xFrom != yFrom or xTo != yTo)
      and (yFrom in xFrom.*subs and (yTo in xTo.*subs or xTo in yTo.*subs)
        or yTo in xTo.*subs and (yFrom in xFrom.*subs or xFrom in yFrom.*subs))
}

pred markedEdge [r : Relationship, rs : set Relationship, is : set Inheritance] {
  some r' : rs - Inheritance |
    markedEdgeCriterion [r.from, r.to, r'.from, r'.to, is]
    or markedEdgeCriterion [r.to, r.from, r'.from, r'.to, is]
}

pred noMarkedEdges [rs : set Relationship, is : set Inheritance] {
  no r : rs - is | markedEdge [r, rs, is]
}

pred noDoubleRelationships [rs : set Relationship] {
  no r, r' : rs | doubleRelationship [r, r']
}

pred noReverseRelationships [rs : set Relationship] {
  no r, r' : rs | reverseRelationship [r, r']
}

pred noMultipleInheritances [is : set Inheritance] {
  no i, i' : is | multipleInheritance [i, i']
}

fact nonEmptyInstancesOnly {
  some Relationship
}

sig Change {
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

pred change [c : Change, rs : set Relationship] {
  some c.add + c.remove
  no c.add or not c.add in rs
  c.remove in rs
  let c1 = changedLimit [c],
      c2 = changedKind [c],
      c3 = flip [c] |
    one c.add and one c.remove iff c1 implies not c2 and not c3 else c2 iff not c3
}

fact changesAreUnique {
  all c, c' : Change | c != c' implies (c.add = c'.add implies c.remove != c'.remove)
    and (c.remove = c'.remove implies c.add != c'.add)
}

fact preventSaneErrors {
  all a : Assoc | (a.fromLower = Star implies a.toLower != Star)
    and (a.toLower = Star implies a.toUpper != Star)
}

abstract sig Boolean {}
one sig True, False extends Boolean {}

pred classDiagram [
  assocs : set Assoc,
  compositions : set Composition,
  inheritances : set Inheritance,
  relationships : set Relationship,
  wrongAssocs : one Int,
  wrongCompositions : one Int,
  selfRelationships : one Int,
  hasDoubleRelationships : one Boolean,
  hasReverseRelationships : one Boolean,
  hasMultipleInheritances : one Boolean,
  hasInheritanceCycles : one Boolean,
  hasCompositionCycles : one Boolean,
  hasMarkedEdges : lone Boolean] {
  #{ a : assocs | not validLimitsAssoc [a]} = wrongAssocs
  #{ a : assocs | not validFromLimitsAssoc [a]} + #{ a : assocs | not validToLimitsAssoc [a]} = wrongAssocs
  #{ c : compositions | not validLimitsComposition [c]} = wrongCompositions
  #{ r : relationships | selfRelationship [r]} = selfRelationships
  hasDoubleRelationships = True
    implies not noDoubleRelationships [relationships]
    else noDoubleRelationships [relationships]
  hasReverseRelationships = True
    implies not noReverseRelationships [relationships]
    else noReverseRelationships [relationships]
  hasMultipleInheritances = True
     implies not noMultipleInheritances [inheritances]
     else noMultipleInheritances [inheritances]
  hasInheritanceCycles = True
    implies not noInheritanceCycles [inheritances]
    else noInheritanceCycles [inheritances]
  hasCompositionCycles = True
    implies not noCompositionCycles [inheritances, compositions]
    else noCompositionCycles [inheritances, compositions]
  hasMarkedEdges = True
    implies not noMarkedEdges[relationships, inheritances]
    else hasMarkedEdges = False implies noMarkedEdges[relationships, inheritances]
}

pred changeOfFirstCD [
  c : one Change,
  wrongAssocs : one Int,
  wrongCompositions : one Int,
  selfRelationships : one Int,
  hasDoubleRelationships : one Boolean,
  hasReverseRelationships : one Boolean,
  hasMultipleInheritances : one Boolean,
  hasInheritanceCycles : one Boolean,
  hasCompositionCycles : one Boolean,
  hasMarkedEdges : lone Boolean] {
    let Assoc' = Assoc - (Change.add - c.add) - c.remove,
        Composition' = Composition - (Change.add - c.add) - c.remove,
        Relationship' = Relationship - (Change.add - c.add) - c.remove,
        Inheritance' = Inheritance - (Change.add - c.add) - c.remove {
      change[c, Relationship - Change.add]
      classDiagram [Assoc', Composition', Inheritance', Relationship',
        wrongAssocs, wrongCompositions, selfRelationships,
        hasDoubleRelationships, hasReverseRelationships,
        hasMultipleInheritances, hasInheritanceCycles, hasCompositionCycles,
        hasMarkedEdges]
  }
}

pred cd {
  let Assoc' = Assoc - Change.add,
      Association' = Association - Change.add,
      Aggregation' = Aggregation - Change.add,
      Composition' = Composition - Change.add,
      Relationship' = Relationship - Change.add,
      Inheritance' = Inheritance - Change.add {
    classDiagram [Assoc', Composition', Inheritance', Relationship', 0, 0, 0,
      False, False, False, False, False, none]
    0 <= #Association' and #Association' <= 2
    0 <= #Aggregation' and #Aggregation' <= 2
    0 <= #Composition' and #Composition' <= 2
    1 <= #Inheritance' and #Inheritance' <= 2
    4 <= #Class
    3 <= #Relationship'
  }
}

sig c1, c2, c3 extends Change {}

pred changes {
  one m1, m2 : Boolean {
    m1 = False or m2 = False
    let c1Assocs = Assoc - (Change.add - Assoc <: c1.add) - c1.remove,
        c2Assocs = Assoc - (Change.add - Assoc <: c2.add) - c2.remove |
      some c1Assocs or some c2Assocs
    changeOfFirstCD [c1, 0, 0, 0, False, False, False, False, False, m1]
    changeOfFirstCD [c2, 0, 0, 0, False, False, False, False, False, m2]
    changeOfFirstCD [c3, 0, 0, 0, False, False, False, False, False, False]
  }
}

run { cd and changes } for 12 Relationship, 4 Class, 3 Change
