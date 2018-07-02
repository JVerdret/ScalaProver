object  MyProver {
  def main(args: Array[String]): Unit = {

    println(args(0))
    println(args(1))

    if (formula_solver(args(0), args(1))) {
      println("provable")
    }
    else {
      println("not provable")
    }

    def solver(left_formula: String, right_formula: String): Boolean = {
      true
    }

    def formula_solver(left_formula: String, right_formula: String): Boolean = {
      if (left_formula.contains("|")){
        var left_token = left_formula.split('|')
        for (i <- left_token.indices)
          if (formula_solver(left_token(i), right_formula))
            return true
        false
      }
      else if (right_formula.contains("|")){
        var right_token = right_formula.split('|')
        for (i <- right_token.indices)
          if (formula_solver(left_formula, right_token(i)))
            return true
        false
      }
      else if (left_formula.contains("&")) {
        var ltoken = left_formula.split('&')
        var new_formula: String = ltoken(0)
        formula_solver(new_formula, right_formula)
      }

      else if (right_formula.contains("&")) {
        var rtoken = right_formula.split('&')
        var new_formula: String = rtoken(0)
        formula_solver(left_formula, new_formula)
      }
      else
        axiom_solver(left_formula, right_formula)
    }

    def axiom_solver(left_formula: String, right_formula: String): Boolean = {
      if (left_formula.contentEquals(right_formula))
        return true
      val left_formula_tokens = left_formula.split(",")
      val right_formula_tokens = right_formula.split(",")
      for (i: Int <- 0 until left_formula_tokens.length) {
        for (j: Int <- 0 until right_formula_tokens.length) {
          if (left_formula_tokens(i) == right_formula_tokens(j)) {
              return true
          }
        }
      }
      return false
    }
  }
}
