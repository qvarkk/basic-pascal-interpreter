""" LISP Notation translator """

import unittest

from pascal_interpreter import Lexer, Parser, NodeVisitor


class Translator(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser

    def visit_BinOp(self, node):
        return f'({node.op.value} {self.visit(node.left)} {self.visit(node.right)})'

    def visit_Number(self, node):
        return str(node.value)

    def translate(self):
        tree = self.parser.parse()
        return self.visit(tree)


def infix2lisp(s):
    lexer = Lexer(s)
    parser = Parser(lexer)
    translator = Translator(parser)
    translation = translator.translate()
    return translation


class Infix2LispTestCase(unittest.TestCase):

    def test_1(self):
        self.assertEqual(infix2lisp('1 + 2'), '(+ 1 2)')

    def test_2(self):
        self.assertEqual(infix2lisp('2 * 7'), '(* 2 7)')

    def test_3(self):
        self.assertEqual(infix2lisp('2 * 7 + 3'), '(+ (* 2 7) 3)')

    def test_4(self):
        self.assertEqual(infix2lisp('2 + 3 * 5'), '(+ 2 (* 3 5))')

    def test_5(self):
        self.assertEqual(infix2lisp('7 + 5 * 2 - 3'), '(- (+ 7 (* 5 2)) 3)')

    def test_6(self):
        self.assertEqual(
            infix2lisp('1 + 2 + 3 + 4 + 5'),
            '(+ (+ (+ (+ 1 2) 3) 4) 5)'
        )


if __name__ == '__main__':
    unittest.main()
