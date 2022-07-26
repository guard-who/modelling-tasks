module uml/cd/assoclimits

open util/ordering[Class] as CO

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

abstract sig Assoc extends Relationship {
  fromLower : one Limit,
  fromUpper : one Limit,
  toLower : one Limit,
  toUpper : one Limit
} {
  fromLower != Star or fromUpper != Star
  toLower != Star or toUpper != Star
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

pred sameFromLimits [a, a2 : Assoc] {
  a.fromLower = a2.fromLower
  a.fromUpper = a2.fromUpper
}

pred sameToLimits [a, a2 : Assoc] {
  a.toLower = a2.toLower
  a.toUpper = a2.toUpper
}

pred sameLimits [a, a2 : Assoc] {
  sameFromLimits [a, a2]
  sameToLimits [a, a2]
}

pred increasedFromRange [a, a2 : Assoc] {
  smaller [a2.fromLower, a.fromLower] and a.fromUpper = a2.fromUpper
  or smaller [a.fromUpper, a2.fromUpper] and a.fromLower = a2.fromLower
  sameToLimits [a, a2]
}

pred increasedToRange [a, a2 : Assoc] {
  smaller [a2.toLower, a.toLower] and a.toUpper = a2.toUpper
  or smaller [a.toUpper, a2.toUpper] and a.toLower = a2.toLower
  sameFromLimits [a, a2]
}

pred increasedRange [a, a2 : Assoc] {
  increasedFromRange [a, a2] iff not increasedToRange [a, a2]
}

pred changedRange [a, a2 : Assoc] {
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

pred shiftedRangeUp [a, a2 : Assoc] {
  shiftLimits [a.fromLower, a.fromUpper, a2.fromLower, a2.fromUpper] and sameToLimits [a, a2]
  iff not (shiftLimits [a.toLower, a.toUpper, a2.toLower, a2.toUpper] and sameFromLimits [a, a2])
}

pred shiftedRange [a, a2 : Assoc] {
  shiftedRangeUp [a, a2] iff not shiftedRangeUp [a2, a]
}

assert sameLimits {
  all a, a2 : Assoc | sameLimits [a, a2] iff sameFromLimits [a, a2] and sameToLimits [a, a2]
}

assert shiftingLimit {
  all l : Limit | shiftBy [l, Zero] = l
}

assert shiftingUp {
  all l, l2 : Limit | l2 != Zero and one shiftBy [l, l2] implies smaller [l, shiftBy [l, l2]]
}

assert shiftingEqually {
  all l, l2, l3, l4 : Limit | shiftLimits [l, l2, l3, l4] implies (l = l3 iff l2 = l4)
}

assert shiftedUpIsValid {
  all a, a2 : Assoc | a != a2 and shiftedRangeUp [a, a2] and validLimitsAssoc [a] implies validLimitsAssoc [a2]
}

assert increasedRangeIsValid {
  all a, a2 : Assoc | a != a2 and increasedRange [a, a2] and validLimitsAssoc [a] implies validLimitsAssoc [a2]
}
