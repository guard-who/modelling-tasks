module uml/cd/assoclimits

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

pred smallerOrSame [l, l' : Limit] {
  not (l = Star)
  and (l = Two implies (l' = Two or l' = Star))
  and (l = One implies not l' = Zero)
}

pred smaller [l, l' : Limit] {
  not l = l' and smallerOrSame [l, l']
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

pred increasedFromRange [a, a' : Assoc] {
  smaller [a'.fromLower, a.fromLower] and a.fromUpper = a'.fromUpper
  or smaller [a.fromUpper, a'.fromUpper] and a.fromLower = a'.fromLower
  sameToLimits [a, a']
}

pred increasedToRange [a, a' : Assoc] {
  smaller [a'.toLower, a.toLower] and a.toUpper = a'.toUpper
  or smaller [a.toUpper, a'.toUpper] and a.toLower = a'.toLower
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
    l2 = shiftBy [l1, shift]
    l2' = shiftBy [l1', shift]
  }
}

pred shiftedRangeUp [a, a' : Assoc] {
  shiftLimits [a.fromLower, a.fromUpper, a'.fromLower, a'.fromUpper] and sameToLimits [a, a']
  iff not (shiftLimits [a.toLower, a.toUpper, a'.toLower, a'.toUpper] and sameFromLimits [a, a'])
}

pred shiftedRange [a, a' : Assoc] {
  shiftedRangeUp [a, a'] iff not shiftedRangeUp [a', a]
}
