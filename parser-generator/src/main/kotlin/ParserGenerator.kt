import InputGrammarParser.*

private const val EPS = "#"
private const val END = "END"
private const val START = "start"
private const val CTX_ACCESS = "$"

class ParserGenerator(
    private val grammarName: String, rulesSectionContext: Rules_sectionContext, private val packageName: String
) {
    private fun extractExpression(ctx: Expr_nameContext): Expr {
        val attrs = ctx.expr_attrs()?.text.orEmpty()
        val code = ctx.expr_code().text

        return when {
            ctx.RULE_NAME() != null -> Expr(ctx.RULE_NAME().text, ExprType.RULE, attrs, code)
            ctx.TOKEN_NAME() != null -> Expr(ctx.TOKEN_NAME().text, ExprType.TOKEN, attrs, code)
            else -> Expr(ctx.EPS().text, ExprType.EPS, attrs, code)
        }
    }

    private fun extractRuleExpressions(ctx: Rule_exprContext) = ctx.expr_name().map { extractExpression(it) }

    private fun extractRuleBodies(ctx: Rule_bodyContext) = ctx.rule_expr().map { RuleBody(extractRuleExpressions(it)) }

    private fun extractInputAttrs(ctx: Input_attrsContext?) = ctx?.attr()?.map {
        Attribute(it.word(0).text, it.word(1).text)
    }.orEmpty()

    private fun extractRules(ctx: Rules_sectionContext) = ctx.just_rule().map {
        Rule(
            name = it.RULE_NAME().text,
            attrs = extractInputAttrs(it.definition().input_attrs()),
            returnAttrs = extractInputAttrs(it.definition().return_expr().input_attrs()),
            bodies = extractRuleBodies(it.rule_body())
        )
    }

    private val rules = extractRules(rulesSectionContext)
    private val rulesMap = rules.associateBy { it.name }

    private fun ruleByName(name: String) = rulesMap[name] ?: throw ParserGeneratorException("Unmapped rule name")

    fun generate(): String {
        addEpsToFirst()

        checkLeftRecursion()

        checkRightBranching()

        buildFirst()

        buildFollow()

        return buildParser()
    }

    private fun addEpsToFirst() {
        var changed = true

        while (changed) {
            changed = false

            for (rule in rules) {
                for (body in rule.bodies) {
                    val firstExpr = body.expressions.first()

                    if ((firstExpr.type == ExprType.EPS || (firstExpr.type == ExprType.RULE && ruleByName(firstExpr.name).first.contains(
                            EPS
                        ))) && !rule.first.contains(EPS)
                    ) {
                        rule.first.add(EPS)

                        changed = true
                    }
                }
            }
        }
    }

    private fun checkLeftRecursion() {
        for (rule in rules) {
            if (dfsLeftRecursion(rule, mutableSetOf())) {
                throw ParserGeneratorException("Given grammar has left recursion")
            }
        }
    }

    private fun dfsLeftRecursion(r: Rule, used: MutableSet<Rule>): Boolean {
        if (used.contains(r)) {
            return true
        }

        used.add(r)

        for (body in r.bodies) {
            for (expr in body.expressions) {
                if (expr.type == ExprType.EPS || expr.type == ExprType.TOKEN) {
                    break
                }

                val rule = ruleByName(expr.name)

                if (dfsLeftRecursion(rule, used)) {
                    return true
                }

                if (!rule.first.contains(EPS)) {
                    break
                }
            }
        }

        used.remove(r)

        return false
    }

    private fun checkRightBranching() {
        for (rule in rules) {
            val firsts = rule.bodies.filter { it.expressions.size > 1 }.map { it.expressions.first().name }
            if (firsts.toSet().size != firsts.size) {
                throw ParserGeneratorException("Given grammar has right branching")
            }
        }
    }

    private fun buildFirst() {
        val used = mutableSetOf<Rule>()
        for (rule in rules) {
            if (!used.contains(rule)) {
                dfsBuildFirst(rule, used)
            }
        }
    }

    private fun dfsBuildFirst(r: Rule, used: MutableSet<Rule>) {
        used.add(r)

        for (body in r.bodies) {
            for (expr in body.expressions) {
                if (expr.type == ExprType.EPS) {
                    break
                }

                if (expr.type == ExprType.TOKEN) {
                    r.first.add(expr.name)
                    break
                }

                if (!used.contains(ruleByName(expr.name))) {
                    dfsBuildFirst(ruleByName(expr.name), used)
                }

                r.first.addAll(ruleByName(expr.name).first)

                if (!ruleByName(expr.name).first.contains(EPS)) {
                    break
                }
            }
        }
    }

    private fun getFirst(expressions: List<Expr>): MutableSet<String> {
        val result = mutableSetOf<String>()

        for (expr in expressions) {
            if (expr.type == ExprType.EPS) {
                result.add(EPS)
                break
            }

            if (expr.type == ExprType.TOKEN) {
                result.add(expr.name)
                break
            }

            result.addAll(ruleByName(expr.name).first)

            if (!result.contains(EPS)) {
                break
            }
        }

        return result
    }

    private fun buildFollow() {
        ruleByName(START).follow.add(END)

        var changed = true
        while (changed) {
            changed = false

            for (rule in rules) {
                for (body in rule.bodies) {
                    for ((i, expr) in body.expressions.withIndex()) {
                        if (expr.type != ExprType.RULE) {
                            continue
                        }

                        val exprRule = ruleByName(expr.name)

                        val oldSize = exprRule.follow.size

                        if (i == body.expressions.size - 1) {
                            exprRule.follow.addAll(rule.follow)
                        } else {
                            val follow = getFirst(body.expressions.drop(i + 1))

                            if (follow.contains(EPS)) {
                                exprRule.follow.addAll(rule.follow)
                            }

                            exprRule.follow.addAll(follow - EPS)
                        }

                        changed = exprRule.follow.size != oldSize
                    }
                }
            }
        }
    }

    private fun buildParser() = """
        package $packageName
        
        import ${packageName}.${grammarName}Lexer.Token.*
        
        class ${grammarName}Parser(private val lexer: ${grammarName}Lexer) {
            class ParserException(m: String) : Throwable(m)
        
            init {
                lexer.nextToken()
            }
            
            open class Tree(val nodeName: String) {
                val children = mutableListOf<Tree>()
            }
            
            class TerminalNode(nodeName: String, val text: String) : Tree(nodeName)
            
            fun parse (${ruleByName(START).attrs.joinToString { "${it.name}: ${it.type}" }}): ${
        START.replaceFirstChar(
            Char::titlecase
        )
    }Context {
                val res = start(${ruleByName(START).attrs.joinToString { it.name }})
                
                if (lexer.curToken == END) {
                    return res
                }
                else {
                    throw ParserException("Unexpected ${'$'}{lexer.curToken} at the end of the input")
                }
            }
            
            ${addRuleFunctions()}
        }
    """.trimIndent()

    private fun addRuleFunctions() = rules.joinToString(separator = "\n", postfix = "\n") { rule ->
        """
            ${addRuleClass(rule)}
            
            fun ${rule.name}(${rule.attrs.joinToString { "${it.name}: ${it.type}" }}): ${rule.name.replaceFirstChar(Char::titlecase)}Context {
                val _localctx = ${rule.name.replaceFirstChar(Char::titlecase)}Context("${rule.name}"${
            rule.attrs.joinToString(
                separator = ""
            ) { ", ${it.name}" }
        })
        
                when (lexer.curToken) {
                    ${addWhen(rule)}
                }
            }
        """.trimIndent()
    }

    private fun addRuleClass(rule: Rule): String {
        val rulesAndTokens = mutableSetOf<String>()
        for (body in rule.bodies) {
            for (expr in body.expressions) {
                if (expr.type == ExprType.RULE) {
                    rulesAndTokens.add("var ${expr.name}: ${expr.name.replaceFirstChar(Char::titlecase)}Context? = null")
                } else if (expr.type == ExprType.TOKEN) {
                    rulesAndTokens.add("var ${expr.name}: TerminalNode? = null")
                }
            }
        }

        return """
            class ${rule.name.replaceFirstChar(Char::titlecase)}Context(name: String${
            rule.attrs.joinToString(
                separator = ""
            ) { ", val ${it.name}: ${it.type}" }
        }): Tree(name) {
                ${rule.returnAttrs.joinToString(separator = "\n") { "var ${it.name}: ${it.type}? = null" }}
                
                ${rulesAndTokens.joinToString(separator = "\n")}
            }
            
    """.trimIndent()
    }

    private fun addWhen(rule: Rule): String {
        val sb = StringBuilder()
        var code = ""
        var hasEPS = false

        for (body in rule.bodies) {
            val bodyFirst = getFirst(body.expressions)
            hasEPS = bodyFirst.remove(EPS)

            if (bodyFirst.isEmpty()) {
                if (body.expressions[0].code.isNotEmpty()) {
                    code = body.expressions[0].code
                }
                continue
            }

            sb.append(bodyFirst.joinToString())
            sb.appendLine(" -> {")

            for (expr in body.expressions) {
                if (expr.type == ExprType.TOKEN) {
                    sb.appendLine(
                        """
                        if (lexer.curToken != ${expr.name}) {
                            throw ParserException("${'$'}{lexer.curToken} unexpected in ${rule.name}")
                        }
                        
                        val ${expr.name} = TerminalNode("${expr.name}", lexer.curTokenText)
                        _localctx.${expr.name} = ${expr.name}
                        _localctx.children.add(${expr.name})
                    """.trimIndent()
                    )

                    if (expr.code.isNotEmpty()) {
                        sb.appendLine(expr.code.drop(1).dropLast(1).replace(CTX_ACCESS, "_localctx."))
                    }
                    sb.appendLine("lexer.nextToken()")
                } else {
                    sb.append("val ${expr.name} = ${expr.name}(")
                    if (expr.attrs.isNotEmpty()) {
                        sb.append(expr.attrs.drop(1).dropLast(1).replace(CTX_ACCESS, "_localctx."))
                    }
                    sb.appendLine(")")

                    sb.appendLine(
                        """
                        _localctx.${expr.name} = ${expr.name}
                        _localctx.children.add(${expr.name})
                    """.trimIndent()
                    )

                    if (expr.code.isNotEmpty()) {
                        sb.appendLine(expr.code.drop(1).dropLast(1).replace(CTX_ACCESS, "_localctx."))
                    }
                }
            }

            sb.appendLine("return _localctx")
            sb.appendLine("}")
        }

        if (hasEPS) {
            sb.append(rule.follow.joinToString())
            sb.appendLine(" -> {")

            if (code.isNotEmpty()) {
                sb.appendLine(code.drop(1).dropLast(1).replace(CTX_ACCESS, "_localctx."))
            }

            sb.appendLine("return _localctx")
            sb.appendLine("}")
        }

        sb.append(
            """
            else -> throw ParserException("${'$'}{lexer.curToken} unexpected in ${rule.name}")
        """.trimIndent()
        )

        return sb.toString()
    }
}