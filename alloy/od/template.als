///////////////////////////////////////////////////
// Generic Head of CD Model - adapted/simplified
///////////////////////////////////////////////////

//Names of fields/associations in classes of the model
abstract sig FieldName {}

//Parent of all classes relating fields and values
abstract sig Object {
  get : FieldName -> set Object
}
