import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import kotlin.io.path.Path
import kotlin.io.path.bufferedWriter

val testPath = Path("src/test/kotlin")

fun generate(grammarFileName: String, grammarName: String) {
    val inputGrammarStream = CharStreams.fromPath(testPath.resolve(grammarFileName))
    val lexer = InputGrammarLexer(inputGrammarStream)
    val tokenStream = CommonTokenStream(lexer)
    val parser = InputGrammarParser(tokenStream)
    val tree = parser.start()

    val generatedLexer = LexerGenerator(grammarName, tree.tokens_section(), "generated").generate()
    val generatedParser = ParserGenerator(grammarName, tree.rules_section(), "generated").generate()

    testPath.resolve("generated/${grammarName}Lexer.kt").bufferedWriter().use {
        it.write(generatedLexer)
    }

    testPath.resolve("generated/${grammarName}Parser.kt").bufferedWriter().use {
        it.write(generatedParser)
    }
}

fun main() {
    generate("lab2_grammar.txt", "Lab2Grammar")
    generate("calc_grammar.txt", "CalcGrammar")
}