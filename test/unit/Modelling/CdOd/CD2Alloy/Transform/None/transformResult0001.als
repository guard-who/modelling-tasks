
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
  some none // i.e. false
}


fact SizeConstraints {
  #Object >= 2
  some none // i.e. false

}

fact SomeSelfLoops {
  some none // i.e. false
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

