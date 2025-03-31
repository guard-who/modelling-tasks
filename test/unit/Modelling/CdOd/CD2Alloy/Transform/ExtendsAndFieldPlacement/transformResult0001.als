
// Alloy Model for CD1
// Produced by Haskell reimplementation of Eclipse plugin transformation
// Generated: -

module cd2alloy/CD1Module

///////////////////////////////////////////////////
// Generic Head of CD Model - adapted/simplified;
// and now specialized for a fixed FieldName set originally appearing further below
///////////////////////////////////////////////////



fact LimitIsolatedObjects {
  some none // i.e. false
}


fact SizeConstraints {
  #(A + B + C + D) >= 2
  some none // i.e. false

}

fact SomeSelfLoops {
  some none // i.e. false
}


///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////

// Classes
sig A {}
sig B {}
sig C {}
sig D {}


///////////////////////////////////////////////////
// CD1
///////////////////////////////////////////////////

// Properties

pred cd1 {

  // Contents


  // Compositions

}

