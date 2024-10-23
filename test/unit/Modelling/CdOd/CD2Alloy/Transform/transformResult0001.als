
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

}

fact LimitIsolatedObjects {
  false
}


fact SizeConstraints {
  #Object >= 2
  false

}

fact SomeSelfLoops {
  false
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

  // Compositions

}

