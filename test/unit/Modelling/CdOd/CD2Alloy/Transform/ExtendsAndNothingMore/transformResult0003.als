
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
  x : set Object
}

fact LimitIsolatedObjects {
 let get = x |
  #Object > mul[2, #{o : Object | no o.get and no get.o}]
}


fact SizeConstraints {
  #Object >= 2
  let count = #x | count >= 4 and count =< 10
  all o : Object | plus[#o.x, minus[#x.o, #(o.x & o)]] =< 4

}


fact SomeSelfLoops {
  some o : Object | o in o.x
}


///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////

// Classes
sig A extends B {}
sig B extends Object {}
sig C extends Object {}
sig D extends Object {}


///////////////////////////////////////////////////
// CD1
///////////////////////////////////////////////////

// Properties

pred cd1 {

  Object = B + C + D

  // Contents

  // Associations

  x in D -> C
  all o : D | #o.x = 1


  // Compositions
  all o : Object | #o.x =< 1
}

