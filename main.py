""" Pascal interpreter """
from lexer import Lexer
from parser import Parser
from semantic_analyzer import SemanticAnalyzer


def main():
    text = """
    PROGRAM Test;
    VAR
       number     : INTEGER;
       a, b, c, x : INTEGER;
       y          : REAL;
    PROCEDURE proc1(z : INTEGER);
    VAR
       x, y : REAL;
    BEGIN
       x := a;
    END;
    BEGIN
       BEGIN
          number := 2;
          a := number;
          b := 10 * a + 10 * number DIV 4;
          c := a - - b
       END;
       x := 11;
       y := 20 / 7 + 3.14;
    END.
    """

    lexer = Lexer(text)
    parser = Parser(lexer)
    prog = parser.parse()
    symtab = SemanticAnalyzer()
    symtab.visit(prog)
    print(symtab.current_scope)


if __name__ == '__main__':
    main()
