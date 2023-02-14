
// Alloy Model for CD1
// Produced by Haskell reimplementation of Eclipse plugin transformation
// Generated: -

module umlp2alloy/CD1Module

///////////////////////////////////////////////////
// Generic Head of CD Model - adapted/simplified
///////////////////////////////////////////////////

//Names of fields/associations in classes of the model
abstract sig FName {}

//Parent of all classes relating fields and values
abstract sig Obj {
  get : FName -> set Obj
}

pred ObjFNames[objs : set Obj, fNames : set FName] {
  no objs.get[FName - fNames]
}

pred ObjLUAttrib[objs : set Obj, fName : FName, fType : set Obj, low, up : Int] {
  ObjLAttrib[objs, fName, fType, low]
  all o : objs | #o.get[fName] =< up
}

pred ObjLAttrib[objs : set Obj, fName : FName, fType : set Obj, low : Int] {
  objs.get[fName] in fType
  all o : objs | #o.get[fName] >= low
}

pred ObjLU[objs : set Obj, fName : FName, fType : set Obj, low, up : Int] {
  ObjL[objs, fName, fType, low]
  ObjU[objs, fName, fType, up]
}

pred ObjL[objs : set Obj, fName : FName, fType : set Obj, low : Int] {
  all r : objs | #{l : fType | r in l.get[fName]} >= low
}

pred ObjU[objs : set Obj, fName : FName, fType : set Obj, up : Int] {
  all r : objs | #{l : fType | r in l.get[fName]} =< up
}

pred Composition[left : set Obj, lFName : set FName, right : set Obj] {
  // all l1, l2 : left | (#{l1.get[lFName] & l2.get[lFName]} > 0) => l1 = l2
  all r : right | #{l : left, lF : lFName | r in l.get[lF]} =< 1
}


fact LimitIsolatedObjects {
  #Obj > mul[2, #{o : Obj | no o.get and no get.o}]
}


fact SizeConstraints {
  #Obj >= 2
  #get >= 4
  #get <= 10
  all o : Obj | let x = plus[#o.get,minus[#get.o,#o.get.o]] | x <= 4

}


fact SomeSelfLoops {
  some o : Obj | o in o.get[FName]
}
///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////

// Concrete names of fields
one sig x extends FName {}
one sig y extends FName {}
one sig z extends FName {}


// Classes (non-abstract)
sig A extends Obj {}
sig B extends Obj {}
sig C extends Obj {}
sig D extends Obj {}


///////////////////////////////////////////////////
// CD1
///////////////////////////////////////////////////

// Types wrapping subtypes
fun ASubsCD1 : set Obj {
  A
}
fun BSubsCD1 : set Obj {
  B
}
fun CSubsCD1 : set Obj {
  C + ASubsCD1 + DSubsCD1
}
fun DSubsCD1 : set Obj {
  D
}

// Types wrapping field names
fun AFieldNamesCD1 : set FName {
  CFieldNamesCD1 + x
}
fun BFieldNamesCD1 : set FName {
  none
}
fun CFieldNamesCD1 : set FName {
  none + y
}
fun DFieldNamesCD1 : set FName {
  CFieldNamesCD1 + z
}

// Types wrapping composite structures and field names
fun ACompositesCD1 : set Obj {
  CCompositesCD1
}
fun ACompFieldNamesCD1 : set FName {
  CCompFieldNamesCD1
}
fun BCompositesCD1 : set Obj {
  none + DSubsCD1
}
fun BCompFieldNamesCD1 : set FName {
  none + z
}
fun CCompositesCD1 : set Obj {
  none
}
fun CCompFieldNamesCD1 : set FName {
  none
}
fun DCompositesCD1 : set Obj {
  CCompositesCD1
}
fun DCompFieldNamesCD1 : set FName {
  CCompFieldNamesCD1
}

// Properties

pred cd1 {

  Obj = none + A + B + C + D

  // Contents
  ObjFNames[A, AFieldNamesCD1]
  ObjFNames[B, BFieldNamesCD1]
  ObjFNames[C, CFieldNamesCD1]
  ObjFNames[D, DFieldNamesCD1]

  // Associations
  ObjLUAttrib[ASubsCD1, x, BSubsCD1, 1, 2]
  ObjL[BSubsCD1, x, ASubsCD1, 0]
  ObjLAttrib[CSubsCD1, y, DSubsCD1, 0]
  ObjLU[DSubsCD1, y, CSubsCD1, 1, 1]
  ObjLUAttrib[DSubsCD1, z, BSubsCD1, 0, 2]
  ObjLU[BSubsCD1, z, DSubsCD1, 1, 1]

  // Compositions
  Composition[ACompositesCD1, ACompFieldNamesCD1, A]
  Composition[BCompositesCD1, BCompFieldNamesCD1, B]
  Composition[CCompositesCD1, CCompFieldNamesCD1, C]
  Composition[DCompositesCD1, DCompFieldNamesCD1, D]

}

