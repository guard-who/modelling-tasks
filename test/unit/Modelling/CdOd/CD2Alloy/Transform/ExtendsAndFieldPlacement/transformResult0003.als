
// Alloy Model for CD1
// Produced by Haskell reimplementation of Eclipse plugin transformation
// Generated: -

module cd2alloy/CD1Module

///////////////////////////////////////////////////
// Generic Head of CD Model - adapted/simplified;
// and now specialized for a fixed FieldName set originally appearing further below
///////////////////////////////////////////////////



fact LimitIsolatedObjects {
 let get = x |
  #(B + C + D) > mul[2, #{o : B + C + D | no o.get and no get.o}]
}


fact SizeConstraints {
  #(B + C + D) >= 2
  let count = #x | count >= 4 and count =< 10
  all o : B + C + D | plus[#o.x, minus[#x.o, #(o.x & o)]] =< 4

}


fact SomeSelfLoops {
  some o : B + C + D | o in o.x
}


///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////

// Classes
sig A extends B {}
sig B {}
sig C {}
sig D {x : set C} {#x = 1}


///////////////////////////////////////////////////
// CD1
///////////////////////////////////////////////////

// Properties

pred cd1 {

  // Contents


  // Compositions
  all o : B + C + D | #o.x =< 1
}

