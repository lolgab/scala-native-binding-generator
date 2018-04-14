package bindgen.preprocessor

case class Line(line: String) extends PreprocessorDefinition {
  override def toString: String = line
}