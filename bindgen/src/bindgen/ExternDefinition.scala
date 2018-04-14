package bindgen

case object ExternDefinition extends HasNative { // Find better name instead of Definition
  override def toString: String = "extern"
}