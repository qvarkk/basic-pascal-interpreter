""" Reverse Polish Notation translator """

import unittest

from pascal_interpreter import Lexer, Parser, NodeVisitor


class Translator(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser

    def visit_BinOp(self, node):
        return f'{self.visit(node.left)} {self.visit(node.right)} {node.op.value}'

    def visit_Number(self, node):
        return str(node.value)

    def translate(self):
        tree = self.parser.parse()
        return self.visit(tree)


def infix2postfix(s):
    lexer = Lexer(s)
    parser = Parser(lexer)
    translator = Translator(parser)
    translation = translator.translate()
    return translation


class Infix2PostfixTestCase(unittest.TestCase):

    def test_1(self):
        self.assertEqual(infix2postfix('2 + 3'), '2 3 +')

    def test_2(self):
        self.assertEqual(infix2postfix('2 + 3 * 5'), '2 3 5 * +')

    def test_3(self):
        self.assertEqual(
            infix2postfix('5 + ((1 + 2) * 4) - 3'),
            '5 1 2 + 4 * + 3 -',
        )

    def test_4(self):
        self.assertEqual(
            infix2postfix('(5 + 3) * 12 / 3'),
            '5 3 + 12 * 3 /',
        )


if __name__ == '__main__':
    unittest.main()
