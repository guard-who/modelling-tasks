module uml/cd/generate

open uml/cd/relationshipLimits

pred change [c : Change, rs : set Relationship] {
  some c.add + c.remove
  no c.add or not c.add in rs
  c.remove in rs
  let c1 = changedLimit [c],
      c2 = changedKind [c],
      c3 = flip [c] |
    one c.add and one c.remove iff c1 or c2 or c3
}

abstract sig Inheritance extends Relationship {}
sig ValidInheritance extends Inheritance {} {not this in Limited}
sig InvalidInheritance extends Inheritance {} {
  this in Limited
  validLimitsNonInheritance [this]
}

sig Aggregation extends NonInheritance {}
sig Association extends NonInheritance {}
sig Composition extends NonInheritance {}

pred selfRelationship [r : Relationship] {
  r.from = r.to
}

pred sameDirection [r, r2 : Relationship] {
  r.from = r2.from and r.to = r2.to
}

pred doubleRelationship [r, r2 : Relationship] {
  disj [r, r2] and sameDirection [r, r2]
}

pred reverseRelationship [r, r2 : Relationship] {
  disj [r, r2] and r.to = r2.from and r.from = r2.to
}

pred multipleInheritance [i, i2 : Inheritance] {
  disj [i, i2] and i.from = i2.from
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
  no c : Class |
    c in c.^((*~inheritance + *inheritance).composition)
}

pred isPartOfSuperfluousComposition [c : Class, is : set Inheritance, cs : set Composition] {
  let subs = *~(relationship [is]) |
    some disj x, y : cs {
      c in x.from.subs
      c in y.from.subs
      x.toLower in Zero + One
      y.toLower = One
    }
}

pred isPartOfMultipleCompositions [c : Class, is : set Inheritance, cs : set Composition] {
  let subs = *~(relationship [is]) |
    some disj x, y : cs {
      c in x.from.subs
      c in y.from.subs
      x.toLower = One
      y.toLower = One
    }
}

pred noCompositionsPreventParts [is : set Inheritance, cs : set Composition] {
  no c : Class | isPartOfMultipleCompositions [c, is, cs]
}

pred thickEdgeCriterion [xFrom, xTo, yFrom, yTo : Class, is : set Inheritance] {
  let subs = *~(relationship [is]) |
    (disj [xFrom, yFrom] or disj[xTo, yTo])
      and let first = yFrom in xFrom.subs, second = yTo in xTo.subs |
        (first and (second or xTo in yTo.subs)
          or second and xFrom in yFrom.subs)
}

pred thickEdge [a : NonInheritance, nonInheritances : set NonInheritance, is : set Inheritance] {
  some a2 : nonInheritances |
    thickEdgeCriterion [a.from, a.to, a2.from, a2.to, is]
    or thickEdgeCriterion [a.to, a.from, a2.from, a2.to, is]
}

pred noThickEdges [nonInheritances : set NonInheritance, is : set Inheritance] {
  no a : nonInheritances | thickEdge [a, nonInheritances, is]
}

pred noDoubleRelationships [rs : set Relationship] {
  no disj r, r2 : rs | doubleRelationship [r, r2]
}

pred noReverseRelationships [rs : set Relationship] {
  no disj r, r2 : rs | reverseRelationship [r, r2]
}

pred noMultipleInheritances [is : set Inheritance] {
  no disj i, i2 : is | multipleInheritance [i, i2]
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
  c.add in NonInheritance implies sameLimits [c.add, c.remove]
}

pred changedKind [c : Change] {
  sameDirection [c.add, c.remove]
  not sameKind [c.add, c.remove]
  c.add in NonInheritance and c.remove in NonInheritance implies sameFromLimits [c.add, c.remove]
  c.add in NonInheritance and c.remove in NonInheritance
    and (validLimitsComposition [c.remove] or not c.add in Composition)
    implies sameLimits [c.add, c.remove]
}

pred changedLimit [c : Change] {
  c.add in NonInheritance
  sameDirection [c.add, c.remove]
  sameKind [c.add, c.remove]
  validLimitsNonInheritance [c.add]
  c.add in Composition implies validLimitsComposition [c.add]
  shiftedRange [c.add, c.remove] iff not changedRange [c.add, c.remove]
}

pred sameRelationship [r, r2 : Relationship] {
  r.from = r2.from
  sameKind [r, r2]
  sameDirection [r, r2]
  r in NonInheritance implies sameLimits [r, r2]
}

fact changesAreUnique {
  all disj c, c2 : Change |
    disj [c.add, c2.add] and not sameRelationship [c.add, c2.add]
    or disj [c.remove, c2.remove] and not sameRelationship [c.remove, c2.remove]
}

abstract sig Boolean {}
one sig True, False extends Boolean {}

pred classDiagram [
  nonInheritances : set NonInheritance,
  compositions : set Composition,
  inheritances : set Inheritance,
  relationships : set Relationship,
  invalidInheritances : one Int,
  wrongNonInheritances : one Int,
  wrongCompositions : one Int,
  selfRelationships : one Int,
  selfInheritances : one Int,
  hasDoubleRelationships : lone Boolean,
  hasReverseRelationships : lone Boolean,
  hasReverseInheritances : one Boolean,
  hasMultipleInheritances : lone Boolean,
  hasNonTrivialInheritanceCycles : one Boolean,
  hasCompositionCycles : one Boolean,
  hasCompositionsPreventingParts : lone Boolean,
  hasThickEdges : lone Boolean] {
  #{ inheritances & InvalidInheritance } = invalidInheritances
  #{ a : nonInheritances | not validLimitsNonInheritance [a]} = wrongNonInheritances
  #{ a : nonInheritances | not validFromLimitsNonInheritance [a] iff validToLimitsNonInheritance [a]} = wrongNonInheritances
  #{ c : compositions | not validLimitsComposition [c]} = wrongCompositions
  #{ r : nonInheritances | selfRelationship [r]} = selfRelationships
  #{ i : inheritances | selfRelationship [i]} = selfInheritances
  noDoubleRelationships [inheritances]
  no i : inheritances, a : nonInheritances |
    sameDirection [i, a] or reverseRelationship [i, a]
  hasDoubleRelationships = True
    implies not noDoubleRelationships [nonInheritances]
    else hasDoubleRelationships = False implies noDoubleRelationships [nonInheritances]
  hasReverseRelationships = True
    implies not noReverseRelationships [nonInheritances]
    else hasReverseRelationships = False implies noReverseRelationships [nonInheritances]
  hasReverseInheritances = True
    implies not noReverseRelationships [inheritances]
    else noReverseRelationships [inheritances]
  hasMultipleInheritances = True
     implies not noMultipleInheritances [inheritances]
     else hasMultipleInheritances = False implies noMultipleInheritances [inheritances]
  hasNonTrivialInheritanceCycles = True
    implies not noNonTrivialInheritanceCycles [inheritances]
    else noNonTrivialInheritanceCycles [inheritances]
  hasCompositionCycles = True
    implies not noCompositionCycles [inheritances, compositions]
    else noCompositionCycles [inheritances, compositions]
  hasCompositionsPreventingParts = True
    implies not noCompositionsPreventParts [inheritances, compositions]
    else hasCompositionsPreventingParts = False
      implies noCompositionsPreventParts [inheritances, compositions]
  hasThickEdges = True
    implies not noThickEdges[nonInheritances, inheritances]
    else hasThickEdges = False implies noThickEdges[nonInheritances, inheritances]
  no c : Class | isPartOfSuperfluousComposition [c, inheritances, compositions]
}

pred changeOfFirstCD [
  c : one Change,
  invalidInheritances : one Int,
  wrongNonInheritances : one Int,
  wrongCompositions : one Int,
  selfRelationships : one Int,
  selfInheritances : one Int,
  hasDoubleRelationships : lone Boolean,
  hasReverseRelationships : lone Boolean,
  hasReverseInheritances : one Boolean,
  hasMultipleInheritances : lone Boolean,
  hasNonTrivialInheritanceCycles : one Boolean,
  hasCompositionCycles : one Boolean,
  hasCompositionsPreventingParts : lone Boolean,
  hasThickEdges : lone Boolean] {
    let NonInheritance2 = NonInheritance - (Change.add - c.add) - c.remove,
        Composition2 = Composition - (Change.add - c.add) - c.remove,
        Relationship2 = Relationship - (Change.add - c.add) - c.remove,
        Inheritance2 = Inheritance - (Change.add - c.add) - c.remove {
      change[c, Relationship - Change.add]
      classDiagram [NonInheritance2, Composition2, Inheritance2, Relationship2,
        invalidInheritances,
        wrongNonInheritances, wrongCompositions, selfRelationships, selfInheritances,
        hasDoubleRelationships, hasReverseRelationships, hasReverseInheritances,
        hasMultipleInheritances, hasNonTrivialInheritanceCycles, hasCompositionCycles,
        hasCompositionsPreventingParts, hasThickEdges]
  }
}
