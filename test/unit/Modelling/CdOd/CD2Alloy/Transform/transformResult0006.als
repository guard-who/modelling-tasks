
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
  y : set Object,
  z : set Object
}

fact LimitIsolatedObjects {
 let get = x + y + z |
  #Object > mul[2, #{o : Object | no o.get and no get.o}]
}


fact SizeConstraints {
  #Object >= 2
  let count = plus[plus[#x, #y], #z] | count >= 4 and count =< 10
  all o : Object | plus[plus[plus[#o.x, minus[#x.o, #(o.x & o)]], plus[#o.y, minus[#y.o, #(o.y & o)]]], plus[#o.z, minus[#z.o, #(o.z & o)]]] =< 4

}


fact SomeSelfLoops {
  some o : Object | o in o.(x + y + z)
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

  x.Object in A
  Object.x in B
  all o : A | #o.x >= 1 and #o.x =< 2


  y.Object in C + A + D
  Object.y in D

  all o : D | #y.o = 1

  z.Object in D
  Object.z in B
  all o : D | #o.z =< 2
  all o : B | #z.o = 1

  // Compositions
  all o : Object | #z.o =< 1
}

