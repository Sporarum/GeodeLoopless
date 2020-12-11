

object Util:
  val tab = "  "
  def indentFromBraces(s: List[String], depth: Int = 0): List[String] =
      def indent(s: String, ammount: Int) = tab * ammount ++ s
      s match {
        case h :: tail if h endsWith "{" =>
          indent(h, depth) :: indentFromBraces(tail, depth+1)
        case h :: tail if h startsWith "}"=>
          if (depth == 0)
            throw new IllegalStateException("unmatched closing brace")
          else
            indent(h, depth-1) :: indentFromBraces(tail, depth-1)
        case h :: tail => 
          indent(h, depth) :: indentFromBraces(tail, depth)
        case Nil =>
          if (depth != 0)
            List(f"!!!$depth unmatched oppening brace${if(depth > 1) "s" else ""}!!!")
          else
            Nil
      }