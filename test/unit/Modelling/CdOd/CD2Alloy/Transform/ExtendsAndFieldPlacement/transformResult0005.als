
// Alloy Model for CD1
// Produced by Haskell reimplementation of Eclipse plugin transformation
// Generated: -

module cd2alloy/CD1Module

///////////////////////////////////////////////////
// Generic Head of CD Model - adapted/simplified;
// and now specialized for a fixed FieldName set originally appearing further below
///////////////////////////////////////////////////



fact LimitIsolatedObjects {
 let get = x + y + z |
  #(B + C + D) > mul[2, #{o : B + C + D | no o.get and no get.o}]
}


fact SizeConstraints {
  #(B + C + D) >= 2
  let count = plus[plus[#x, #y], #z] | count >= 4 and count =< 10
  all o : B + C + D | plus[plus[plus[#o.x, minus[#x.o, #(o.x & o)]], plus[#o.y, minus[#y.o, #(o.y & o)]]], plus[#o.z, minus[#z.o, #(o.z & o)]]] =< 4

}


fact SomeSelfLoops {
  some o : B + C + D | o in o.(x + y + z)
}


///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////

// Classes
sig A extends C {x : set B} {#x >= 1 and #x =< 2}
sig B {z : set D} {#z = 1}
sig C {}
sig D {y : set C} {
  #y = 1
  #@z.this =< 2
}


///////////////////////////////////////////////////
// CD1
///////////////////////////////////////////////////

// Properties

pred cd1 {

  // Contents


  // Compositions
  all o : B + C + D | #o.z =< 1
}

