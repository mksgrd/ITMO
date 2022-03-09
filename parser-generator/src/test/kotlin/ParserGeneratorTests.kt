import generated.CalcGrammarLexer
import generated.CalcGrammarParser
import generated.Lab2GrammarLexer
import generated.Lab2GrammarParser
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.assertDoesNotThrow

import kotlin.io.path.Path
import kotlin.io.path.bufferedWriter
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFails

internal class ParserGeneratorTests {
    private fun runCalcParser(input: String): Int {
        return CalcGrammarParser(CalcGrammarLexer(input.byteInputStream())).parse().res
            ?: throw Exception("Unexpected null")
    }

    private fun runLab2Parser(input: String) {
        Lab2GrammarParser(Lab2GrammarLexer(input.byteInputStream())).parse()
    }

    private fun genFromGrammar(grammar: String) {
        val inputGrammarStream = CharStreams.fromString(grammar)
        val lexer = InputGrammarLexer(inputGrammarStream)
        val tokenStream = CommonTokenStream(lexer)
        val parser = InputGrammarParser(tokenStream)
        val tree = parser.start()

        LexerGenerator("Sample", tree.tokens_section(), "generated").generate()
        ParserGenerator("Sample", tree.rules_section(), "generated").generate()
    }

    @Test
    fun `calculator tests`() {
        assertEquals(4, runCalcParser("2 + 2"))
        assertEquals(-8, runCalcParser("1 - 2 - 3 - 4"))
        assertEquals(-20, runCalcParser("4 - ( 2 + 10 / 2 - 4 ) * 8"))
        assertEquals(7, runCalcParser("( 0 ) + ( 1 ) + 2 * 3"))
        assertEquals(9, runCalcParser("- ( 4 - 3 * 7 + 8 )"))
        assertEquals(4, runCalcParser("32 / 2 / 4"))
    }

    @Test
    fun `Lab2 parser tests`() {
        assertDoesNotThrow {
            runLab2Parser("a ( b a * ca ) * b z * a * | f t o ")
            runLab2Parser("ab +")
            runLab2Parser("a ( b ) ( x * + ) cd")
            runLab2Parser("a * b * c * d *")
            runLab2Parser("a b a * ctf a b a")
            runLab2Parser("a ( b + a * c a + ) * b a")
            runLab2Parser("( a b c ) * ( d e f ) * ( kek ) ")
            runLab2Parser("a ( ba * ca ) * bz * a * | fto")
        }

        assertFails { runLab2Parser("") }
        assertFails { runLab2Parser(" a | ") }
        assertFails { runLab2Parser("a | b |") }
        assertFails { runLab2Parser("a * *") }
        assertFails { runLab2Parser("* ( ( z x y ) * ) *") }
        assertFails { runLab2Parser("a a z * * y *") }
        assertFails { runLab2Parser("a b + + c d") }
        assertFails { runLab2Parser("a + +") }
    }

    @Test
    fun `Not LL(1)`() {
        assertFails {
            genFromGrammar(
                """
            grammar LeftRecursion;
            
            a : b c;
            b : a d;
            c : C;
            d : D;
            
            C : 't';
            D : 'f';
        """.trimIndent()
            )
        }

        assertFails {
            genFromGrammar(
                """
            grammar RightBranching;
            
            a : b c | b d;
            
            b : B;
            c : C;
            d : D;
            
            B : 'h';
            C : 't';
            D : 'f';
        """.trimIndent()
            )
        }
    }

}