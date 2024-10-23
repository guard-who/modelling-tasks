
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
  all o : Object | let x = plus[plus[#o.x, minus[#x.o, #(o.x & o)]], plus[#o.y, minus[#y.o, #(o.y & o)]]] | x =< 4

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

  Object = none + A + B + C + D

  // Contents

  // Associations

  x.Object in A
  Object.x in B + D
  all o : A | #o.x >= 1 and #o.x =< 2


  y.Object in C + A
  Object.y in D

  all o : D | #y.o = 1

  // Compositions

}

