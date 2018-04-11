object DefinitionsUtils {
  def withoutUselessNameAliases(definitions: Seq[Definition]): Seq[Definition] = {
    val (selected, others) = definitions.partition(_ match {
      case NameAlias(a, b) if a == b => true
      case _ => false
    })

    val aliases = selected.map(_.asInstanceOf[NameAlias])

    val extern = aliases.filter{ a =>
      definitions.filter(_ != a).forall(_.name != a.newName)
    }
    extern.map(na => ExternTypeAlias(na.newName)) ++ others
  }
}
