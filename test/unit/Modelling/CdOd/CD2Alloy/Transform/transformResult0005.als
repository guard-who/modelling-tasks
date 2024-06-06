
// Alloy Model for CD1
// Produced by Haskell reimplementation of Eclipse plugin transformation
// Generated: -

module cd2alloy/CD1Module

///////////////////////////////////////////////////
// Generic Head of CD Model - adapted/simplified
///////////////////////////////////////////////////

//Names of fields/associations in classes of the model
abstract sig FieldName {}

//Parent of all classes relating fields and values
abstract sig Object {
  get : FieldName -> set Object
}

pred ObjectFieldNames[objects : set Object, fieldNames : set FieldName] {
  no objects.get[FieldName - fieldNames]
}

pred ObjectLowerUpperAttribute[objects : set Object, fieldName : FieldName, fType : set Object, low, up : Int] {
  ObjectLowerAttribute[objects, fieldName, fType, low]
  all o : objects | #o.get[fieldName] =< up
}

pred ObjectLowerAttribute[objects : set Object, fieldName : FieldName, fType : set Object, low : Int] {
  objects.get[fieldName] in fType
  all o : objects | #o.get[fieldName] >= low
}

pred ObjectLowerUpper[objects : set Object, fieldName : FieldName, fType : set Object, low, up : Int] {
  ObjectLower[objects, fieldName, fType, low]
  ObjectUpper[objects, fieldName, fType, up]
}

pred ObjectLower[objects : set Object, fieldName : FieldName, fType : set Object, low : Int] {
  all r : objects | #{l : fType | r in l.get[fieldName]} >= low
}

pred ObjectUpper[objects : set Object, fieldName : FieldName, fType : set Object, up : Int] {
  all r : objects | #{l : fType | r in l.get[fieldName]} =< up
}

pred Composition[left : set Object, lFieldName : set FieldName, right : set Object] {
  // all l1, l2 : left | (#{l1.get[lFieldName] & l2.get[lFieldName]} > 0) => l1 = l2
  all r : right | #{l : left, lF : lFieldName | r in l.get[lF]} =< 1
}


fact LimitIsolatedObjects {
  #Object > mul[2, #{o : Object | no o.get and no get.o}]
}


fact SizeConstraints {
  #Object >= 2
  #get >= 4
  #get <= 10
  all o : Object | let x = plus[#o.get,minus[#get.o,#o.get.o]] | x <= 4

}


fact SomeSelfLoops {
  some o : Object | o in o.get[FieldName]
}


///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////

// Concrete names of fields
one sig x extends FieldName {}
one sig y extends FieldName {}
one sig z extends FieldName {}


// Classes (non-abstract)
sig A extends Object {}
sig B extends Object {}
sig C extends Object {}
sig D extends Object {}


///////////////////////////////////////////////////
// CD1
///////////////////////////////////////////////////

// Types wrapping subtypes
fun ASubsCD1 : set Object {
  A
}
fun BSubsCD1 : set Object {
  B
}
fun CSubsCD1 : set Object {
  C + ASubsCD1
}
fun DSubsCD1 : set Object {
  D
}

// Types wrapping field names
fun AFieldNamesCD1 : set FieldName {
  CFieldNamesCD1 + x
}
fun BFieldNamesCD1 : set FieldName {
  none
}
fun CFieldNamesCD1 : set FieldName {
  none + y
}
fun DFieldNamesCD1 : set FieldName {
  none + z
}

// Types wrapping composite structures and field names
fun ACompositesCD1 : set Object {
  CCompositesCD1
}
fun ACompFieldNamesCD1 : set FieldName {
  CCompFieldNamesCD1
}
fun BCompositesCD1 : set Object {
  none + DSubsCD1
}
fun BCompFieldNamesCD1 : set FieldName {
  none + z
}
fun CCompositesCD1 : set Object {
  none
}
fun CCompFieldNamesCD1 : set FieldName {
  none
}
fun DCompositesCD1 : set Object {
  none
}
fun DCompFieldNamesCD1 : set FieldName {
  none
}

// Properties

pred cd1 {

  Object = none + A + B + C + D

  // Contents
  ObjectFieldNames[A, AFieldNamesCD1]
  ObjectFieldNames[B, BFieldNamesCD1]
  ObjectFieldNames[C, CFieldNamesCD1]
  ObjectFieldNames[D, DFieldNamesCD1]

  // Associations
  ObjectLowerUpperAttribute[ASubsCD1, x, BSubsCD1, 1, 2]
  ObjectLower[BSubsCD1, x, ASubsCD1, 0]
  ObjectLowerAttribute[CSubsCD1, y, DSubsCD1, 0]
  ObjectLowerUpper[DSubsCD1, y, CSubsCD1, 1, 1]
  ObjectLowerUpperAttribute[DSubsCD1, z, BSubsCD1, 0, 2]
  ObjectLowerUpper[BSubsCD1, z, DSubsCD1, 1, 1]

  // Compositions
  Composition[ACompositesCD1, ACompFieldNamesCD1, A]
  Composition[BCompositesCD1, BCompFieldNamesCD1, B]
  Composition[CCompositesCD1, CCompFieldNamesCD1, C]
  Composition[DCompositesCD1, DCompFieldNamesCD1, D]

}

