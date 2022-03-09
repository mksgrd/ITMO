enum class ExprType {
    RULE, TOKEN, EPS
}

data class Expr(
    val name: String,
    val type: ExprType,
    val attrs: String,
    val code: String
)

data class Attribute(val type: String, val name: String)

data class RuleBody(val expressions: List<Expr>)

data class Rule(
    val name: String,
    val attrs: List<Attribute>,
    val returnAttrs: List<Attribute>,
    val bodies: List<RuleBody>,
    val first: MutableSet<String> = mutableSetOf(),
    val follow: MutableSet<String> = mutableSetOf()
)

class ParserGeneratorException(m: String) : Throwable(m)