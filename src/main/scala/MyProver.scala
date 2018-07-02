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

    def formula_solver(left_formula: String, right_formula: String): Boolean = {
      if (left_formula.contains("|")){
        var index = left_formula.indexOf('|')
        var toktokwhosthere = left_formula.split('|')
        if (left_formula.charAt(index + 1).equals('(')){
          var iparenthesis = left_formula.indexOf(')')
          var string = ""
          for (i <- index + 1 until iparenthesis)
            string += left_formula(i)
          formula_solver(toktokwhosthere(0), string)
        }
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
        if (right_formula.contains(left_formula))
          return axiom_solver(left_formula, right_formula)
        var ltoken = left_formula.split('&')
        //var new_formula: String = ltoken(0)
        //formula_solver(new_formula, right_formula)
        for (i <- ltoken.indices)
          if(formula_solver(ltoken(i), right_formula))
            return true
        false
      }

      else if (right_formula.contains("&")) {
        if (left_formula.contains(right_formula))
          return axiom_solver(left_formula, right_formula)
        var rtoken = right_formula.split('&')
        for (i <- rtoken.indices)
          if(formula_solver(left_formula, rtoken(i)))
            return true
        false
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
