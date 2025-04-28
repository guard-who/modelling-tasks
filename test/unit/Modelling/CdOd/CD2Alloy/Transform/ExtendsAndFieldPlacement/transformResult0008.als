
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
  #(B + C + D) > mul[2, #{o : B + C + D | no o.get and no get.o}]
}


fact SizeConstraints {
  #(B + C + D) >= 2
  let count = plus[#x, #y] | count >= 4 and count =< 10
  all o : B + C + D | plus[plus[#o.x, minus[#x.o, #(o.x & o)]], plus[#o.y, minus[#y.o, #(o.y & o)]]] =< 4

}


fact SomeSelfLoops {
  some o : B + C + D | o in o.(x + y)
}


///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////

// Classes
sig A extends B {}
sig B {
  x : set C,
  y : set D
} {
  #x =< 1
  #y =< 1
}
sig C {}
sig D {}


///////////////////////////////////////////////////
// CD1
///////////////////////////////////////////////////

// Properties

pred cd1 {

  // Contents


  // Compositions
  all o : B + C + D | plus[#o.x, #o.y] =< 1
}

