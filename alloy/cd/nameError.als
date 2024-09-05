module uml/cd/nameError

open uml/cd/generate

pred noSelfCycle [inheritances : set Inheritance, nonInheritances : set NonInheritance] {
  let inheritance = relationship [inheritances],
      nonInheritance = relationship [nonInheritances] |
    no c : Class |
      c in c.*inheritance.nonInheritance + c.^~inheritance.nonInheritance
}

pred someInheritedReverseRelationship [inheritances : set Inheritance, nonInheritance1 : one NonInheritance, nonInheritance2 : one NonInheritance] {
  let inheritance = relationship [inheritances] - iden,
      reversed1 = relationship [nonInheritance1] - iden,
      reversed2 = relationship [nonInheritance2 - nonInheritance1] - iden |
      some c : Class  |
        c in ((c.^inheritance.reversed1 - c.*inheritance).(*inheritance + *~inheritance) - c.*inheritance).reversed2
        or c in ((c.^inheritance.~reversed1 - c.*inheritance).(*inheritance + *~inheritance) - c.*inheritance).~reversed2
}

pred noInheritedReverseRelationships [inheritances : set Inheritance, nonInheritances : set NonInheritance] {
  no disj nonInheritance1, nonInheritance2 : nonInheritances |
    someInheritedReverseRelationship [inheritances, nonInheritance1, nonInheritance2]
}
