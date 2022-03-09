import InputGrammarParser.Tokens_sectionContext

class LexerGenerator(
    private val grammarName: String, tokensContext: Tokens_sectionContext, private val packageName: String
) {
    private data class Token(val name: String, val value: String)

    private val tokens = tokensContext.token().map { Token(it.TOKEN_NAME().text, it.token_expr().text) }

    fun generate() = """
package $packageName

import java.io.IOException
import java.io.InputStream
import java.lang.StringBuilder

class ${grammarName}Lexer(private val input: InputStream) { 
    class LexerException(m: String) : Throwable(m)

    enum class Token { ${tokens.joinToString { it.name }}, END }
    
    private val tokenStrings = listOf(${
        tokens.joinToString {
            if (it.value.startsWith("`")) {
                "null"
            } else {
                "\"${it.value.removeSurrounding("'")}\""
            }
        }
    })
    
    private val tokenRegexps = mutableMapOf(${
        tokens.withIndex().filter { it.value.value.startsWith("`") }.joinToString {
            "${it.index} to \"${it.value.value.removeSurrounding("`")}\".toRegex()"
        }
    })

    private var curChar = -1

    private var curPos = 0

    lateinit var curTokenText: String
        private set

    lateinit var curToken: Token
        private set

    init {
        nextChar()
    }
    
    private fun skipSpaces() {
        while (curChar != -1 && curChar.toChar().isWhitespace()) {
            nextChar()
        }
    }
    
    private fun tokenId(s: String): Int {
        for (i in tokenStrings.indices) {
            if (tokenStrings[i] != null) {
                if (tokenStrings[i] == s) {
                    return i
                }
            } else {
                for (p in tokenRegexps) {
                    if (p.value.matches(s)) {
                        return p.key
                    }
                }
            }
        }
        return -1
    }

    private fun nextChar() {
        try {
            curChar = input.read()
            curPos++
        } catch (e: IOException) {
            throw LexerException("Cannot read next char from input")
        }
    }

    fun nextToken() {
        skipSpaces()

        if (curChar == -1) {
            curToken = Token.END
            return
        }

        val sb = StringBuilder()

        while (curChar != -1 && !curChar.toChar().isWhitespace()) {
            sb.append(curChar.toChar())
            nextChar()
        }

        curToken = when (tokenId(sb.toString())) {
            ${
        tokens.withIndex().joinToString(separator = "\n", postfix = "\n") { "${it.index} -> Token.${it.value.name}" }
    }
            else -> throw LexerException("Unexpected character '${'$'}{curChar.toChar()}' at pos ${'$'}curPos")
        }

        curTokenText = sb.toString()
    }
}
"""
}