package bindgen.preprocessor

case class Define(name: bindgen.Identifier, value: String) extends PreprocessorDefinition {
  override def toString: String = s"val $name = $value"
}
