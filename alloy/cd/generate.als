module uml/cd/CDand3Changes

open uml/cd/assoclimits

sig Inheritance extends Relationship {}

sig Aggregation extends Assoc {}
sig Association extends Assoc {}
sig Composition extends Assoc {}

pred selfRelationship [r : Relationship] {
  r.from = r.to
}

pred sameDirection [r, r2 : Relationship] {
  r.from = r2.from and r.to = r2.to
}

pred doubleRelationship [r, r2 : Relationship] {
  r != r2 and sameDirection [r, r2]
}

pred reverseRelationship [r, r2 : Relationship] {
  r != r2 and r.to = r2.from and r.from = r2.to
}

pred multipleInheritance [i, i2 : Inheritance] {
  i != i2 and i.from = i2.from
}

fun relationship [restriction : set Relationship] : Class -> Class {
  ((~from :> restriction) . (restriction <: to))
}

pred noNonTrivialInheritanceCycles [is : set Inheritance] {
  let inheritance = relationship [is] |
  no c : Class |
     not c in c.inheritance + c.inheritance.inheritance
     and c in c.^inheritance
}

pred noCompositionCycles [is : set Inheritance, cs : set Composition] {
  let inheritance = relationship [is],
      composition = relationship [cs] |
  no c : Class | c in c.^(*inheritance.composition).*~inheritance
}

pred markedEdgeCriterion [xFrom, xTo, yFrom, yTo : Class, is : set Inheritance] {
  let subs = ~(relationship [is]) |
    (xFrom != yFrom or xTo != yTo)
      and let first = yFrom in xFrom.subs, second = yTo in xTo.subs |
        (first and (second or xTo in yTo.subs)
          or second and xFrom in yFrom.subs)
}

pred markedEdge [a : Assoc, assocs : set Assoc, is : set Inheritance] {
  some a2 : assocs |
    markedEdgeCriterion [a.from, a.to, a2.from, a2.to, is]
    or markedEdgeCriterion [a.to, a.from, a2.from, a2.to, is]
}

pred noMarkedEdges [assocs : set Assoc, is : set Inheritance] {
  no a : assocs | markedEdge [a, assocs, is]
}

pred noDoubleRelationships [rs : set Relationship] {
  no r, r2 : rs | doubleRelationship [r, r2]
}

pred noReverseRelationships [rs : set Relationship] {
  no r, r2 : rs | reverseRelationship [r, r2]
}

pred noMultipleInheritances [is : set Inheritance] {
  no i, i2 : is | multipleInheritance [i, i2]
}

fact nonEmptyInstancesOnly {
  some Relationship
}

sig Change {
  add : lone Relationship,
  remove : lone Relationship
}

pred sameKind [r, r2 : Relationship] {
  r in Association iff r2 in Association
  r in Aggregation iff r2 in Aggregation
  r in Composition iff r2 in Composition
  r in Inheritance iff r2 in Inheritance
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

pred changedLimit [c : Change] {
  c.add in Assoc
  sameDirection [c.add, c.remove]
  sameKind [c.add, c.remove]
  validLimitsAssoc [c.add]
  c.add in Composition implies validLimitsComposition [c.add]
  shiftedRange [c.add, c.remove] iff not changedRange [c.add, c.remove]
}

pred sameRelationship [r, r2 : Relationship] {
  r.from = r2.from
  sameKind [r, r2]
  sameDirection [r, r2]
  r in Assoc implies sameLimits [r, r2]
}

pred change [c : Change, rs : set Relationship] {
  some c.add + c.remove
  no c.add or not c.add in rs
  c.remove in rs
  let c1 = changedLimit [c],
      c2 = changedKind [c],
      c3 = flip [c] |
    one c.add and one c.remove iff c1 or c2 or c3
}

fact changesAreUnique {
  all c, c2 : Change | c = c2
    or c.add != c2.add and not sameRelationship [c.add, c2.add]
    or c.remove != c2.remove and not sameRelationship [c.remove, c2.remove]
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
  selfInheritances : one Int,
  hasDoubleRelationships : one Boolean,
  hasReverseRelationships : one Boolean,
  hasReverseInheritances : one Boolean,
  hasMultipleInheritances : one Boolean,
  hasNonTrivialInheritanceCycles : one Boolean,
  hasCompositionCycles : one Boolean,
  hasMarkedEdges : lone Boolean] {
  #{ a : assocs | not validLimitsAssoc [a]} = wrongAssocs
  #{ a : assocs | not validFromLimitsAssoc [a] iff validToLimitsAssoc [a]} = wrongAssocs
  #{ c : compositions | not validLimitsComposition [c]} = wrongCompositions
  #{ r : assocs | selfRelationship [r]} = selfRelationships
  #{ i : inheritances | selfRelationship [i]} = selfInheritances
  no i : inheritances | not noDoubleRelationships [i]
  no i : inheritances, a : assocs |
    sameDirection [i, a] or reverseRelationship [i, a]
  hasDoubleRelationships = True
    implies not noDoubleRelationships [assocs]
    else noDoubleRelationships [assocs]
  hasReverseRelationships = True
    implies not noReverseRelationships [assocs]
    else noReverseRelationships [assocs]
  hasReverseInheritances = True
    implies not noReverseRelationships [inheritances]
    else noReverseRelationships [inheritances]
  hasMultipleInheritances = True
     implies not noMultipleInheritances [inheritances]
     else noMultipleInheritances [inheritances]
  hasNonTrivialInheritanceCycles = True
    implies not noNonTrivialInheritanceCycles [inheritances]
    else noNonTrivialInheritanceCycles [inheritances]
  hasCompositionCycles = True
    implies not noCompositionCycles [inheritances, compositions]
    else noCompositionCycles [inheritances, compositions]
  hasMarkedEdges = True
    implies not noMarkedEdges[assocs, inheritances]
    else hasMarkedEdges = False implies noMarkedEdges[assocs, inheritances]
}

pred changeOfFirstCD [
  c : one Change,
  wrongAssocs : one Int,
  wrongCompositions : one Int,
  selfRelationships : one Int,
  selfInheritances : one Int,
  hasDoubleRelationships : one Boolean,
  hasReverseRelationships : one Boolean,
  hasReverseInheritances : one Boolean,
  hasMultipleInheritances : one Boolean,
  hasNonTrivialInheritanceCycles : one Boolean,
  hasCompositionCycles : one Boolean,
  hasMarkedEdges : lone Boolean] {
    let Assoc2 = Assoc - (Change.add - c.add) - c.remove,
        Composition2 = Composition - (Change.add - c.add) - c.remove,
        Relationship2 = Relationship - (Change.add - c.add) - c.remove,
        Inheritance2 = Inheritance - (Change.add - c.add) - c.remove {
      change[c, Relationship - Change.add]
      classDiagram [Assoc2, Composition2, Inheritance2, Relationship2,
        wrongAssocs, wrongCompositions, selfRelationships, selfInheritances,
        hasDoubleRelationships, hasReverseRelationships, hasReverseInheritances,
        hasMultipleInheritances, hasNonTrivialInheritanceCycles, hasCompositionCycles,
        hasMarkedEdges]
  }
}
