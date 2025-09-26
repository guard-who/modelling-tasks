
// Alloy Model for CD1
// Produced by Haskell reimplementation of Eclipse plugin transformation
// Generated: -

module cd2alloy/CD1Module

///////////////////////////////////////////////////
// Generic Head of CD Model - adapted/simplified;
// and now specialized for a fixed FieldName set originally appearing further below
///////////////////////////////////////////////////



fact LimitIsolatedObjects {
 let get = x + y |
  #(B + D) > mul[2, #{o : B + D | no o.get and no get.o}]
}


fact SizeConstraints {
  #(B + D) >= 2
  let count = plus[#x, #y] | count >= 4 and count =< 10
  all o : B + D | plus[plus[#o.x, minus[#x.o, #(o.x & o)]], plus[#o.y, minus[#y.o, #(o.y & o)]]] =< 4

}


fact SomeSelfLoops {
  some o : B + D | o in o.(x + y)
}


///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////

// Classes
sig A extends C {x : set B} {#x >= 1 and #x =< 2}
sig B {}
sig C extends B {}
sig D {y : set C} {#y = 1}


///////////////////////////////////////////////////
// CD1
///////////////////////////////////////////////////

// Properties

pred cd1 {

  // Contents

  // Associations

  // Compositions

}

