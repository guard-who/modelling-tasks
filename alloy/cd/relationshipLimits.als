module uml/cd/relationshipLimits

abstract sig Class {}

abstract sig Relationship {
  from : one Class,
  to : one Class
}

abstract sig Limit {}

one sig Zero extends Limit {}
one sig One extends Limit {}
one sig Two extends Limit {}
one sig Star extends Limit {}

pred smallerOrSame [l, l2 : Limit] {
  not (l = Star)
  and (l = Two implies (l2 = Two or l2 = Star))
  and (l = One implies not l2 = Zero)
}

pred smaller [l, l2 : Limit] {
  not l = l2 and smallerOrSame [l, l2]
}

sig Limited in Relationship {
  fromLower : one Limit,
  fromUpper : one Limit,
  toLower : one Limit,
  toUpper : one Limit
} {
  fromLower != Star or fromUpper != Star
  toLower != Star or toUpper != Star
}

abstract sig NonInheritance extends Relationship {} {this in Limited}

pred validFromLimitsNonInheritance [a : Limited] {
  smallerOrSame [a.fromLower, a.fromUpper]
  a.fromLower = Zero implies a.fromUpper != Zero
}

pred validToLimitsNonInheritance [a : Limited] {
  smallerOrSame [a.toLower, a.toUpper]
  a.toLower = Zero implies a.toUpper != Zero
}

pred validLimitsNonInheritance [a : Limited] {
  validFromLimitsNonInheritance [a]
  validToLimitsNonInheritance [a]
}

pred validLimitsComposition [a : Limited] {
  (a.toLower = Zero or a.toLower = One) and a.toUpper = One
}

pred sameFromLimits [a, a2 : Limited] {
  a.fromLower = a2.fromLower
  a.fromUpper = a2.fromUpper
}

pred sameToLimits [a, a2 : Limited] {
  a.toLower = a2.toLower
  a.toUpper = a2.toUpper
}

pred identicalLimits [a, a2 : Limited] {
  sameFromLimits [a, a2]
  sameToLimits [a, a2]
}

pred increasedFromRange [a, a2 : Limited] {
  smaller [a2.fromLower, a.fromLower] and a.fromUpper = a2.fromUpper
  or smaller [a.fromUpper, a2.fromUpper] and a.fromLower = a2.fromLower
  sameToLimits [a, a2]
}

pred increasedToRange [a, a2 : Limited] {
  smaller [a2.toLower, a.toLower] and a.toUpper = a2.toUpper
  or smaller [a.toUpper, a2.toUpper] and a.toLower = a2.toLower
  sameFromLimits [a, a2]
}

pred increasedRange [a, a2 : Limited] {
  increasedFromRange [a, a2] iff not increasedToRange [a, a2]
}

pred changedRange [a, a2 : Limited] {
  increasedRange [a, a2] iff not increasedRange [a2, a]
}

fun shiftBy [l, shift : Limit] : lone Limit {
  shift = Zero implies l
  else shift = One implies (l = Zero implies One else l = One implies Two else l = Two implies Star else none)
  else shift = Two implies (l = Zero implies Two else l = One implies Star else none)
  else shift = Star implies (l = Zero implies Star else none)
  else none
}

pred shiftLimits [l1, l1b, l2, l2b : Limit] {
  one shift : Limit {
    shift != Zero
    l2 = shiftBy [l1, shift]
    l2b = shiftBy [l1b, shift]
  }
}

pred shiftedRangeUp [a, a2 : Limited] {
  shiftLimits [a.fromLower, a.fromUpper, a2.fromLower, a2.fromUpper] and sameToLimits [a, a2]
  iff not (shiftLimits [a.toLower, a.toUpper, a2.toLower, a2.toUpper] and sameFromLimits [a, a2])
}

pred shiftedRange [a, a2 : Limited] {
  shiftedRangeUp [a, a2] iff not shiftedRangeUp [a2, a]
}

assert identicalLimits {
  all disj a, a2 : Limited |
    identicalLimits [a, a2] iff sameFromLimits [a, a2] and sameToLimits [a, a2]
}

assert shiftingLimit {
  all l : Limit | shiftBy [l, Zero] = l
}

assert shiftingUp {
  all disj l, l2 : Limit |
    l2 != Zero and one shiftBy [l, l2] implies smaller [l, shiftBy [l, l2]]
}

assert shiftingEqually {
  all l, l2, l3, l4 : Limit | shiftLimits [l, l2, l3, l4] implies (l = l3 iff l2 = l4)
}

assert shiftedUpIsValid {
  all disj a, a2 : Limited |
    shiftedRangeUp [a, a2] and validLimitsNonInheritance [a] implies validLimitsNonInheritance [a2]
}

assert increasedRangeIsValid {
  all disj a, a2 : Limited |
    increasedRange [a, a2] and validLimitsNonInheritance [a] implies validLimitsNonInheritance [a2]
}
