
// Alloy Model for CD1
// Produced by Haskell reimplementation of Eclipse plugin transformation
// Generated: -

module cd2alloy/CD1Module

///////////////////////////////////////////////////
// Generic Head of CD Model - adapted/simplified;
// and now specialized for a fixed FieldName set originally appearing further below
///////////////////////////////////////////////////

//Parent of all classes relating fields and values
abstract sig Object {
  x : set Object,
  y : set Object
}

fact LimitIsolatedObjects {
 let get = x + y |
  #Object > mul[2, #{o : Object | no o.get and no get.o}]
}


fact SizeConstraints {
  #Object >= 2
  let count = plus[#x, #y] | count >= 4 and count =< 10
  all o : Object | plus[plus[#o.x, minus[#x.o, #(o.x & o)]], plus[#o.y, minus[#y.o, #(o.y & o)]]] =< 4

}


fact SomeSelfLoops {
  some o : Object | o in o.(x + y)
}


///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////

// Classes
sig A extends Object {}
sig B extends Object {}
sig C extends Object {}
sig D extends Object {}


///////////////////////////////////////////////////
// CD1
///////////////////////////////////////////////////

// Properties

pred cd1 {

  Object = A + B + C + D

  // Contents

  // Associations

  x.Object in B + A
  Object.x in C
  all o : B + A | #o.x =< 1


  y.Object in B + A
  Object.y in D
  all o : B + A | #o.y =< 1


  // Compositions
  all o : Object | plus[#o.x, #o.y] =< 1
}

