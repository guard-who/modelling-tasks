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
