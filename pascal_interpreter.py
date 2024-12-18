""" Pascal interpreter """
from argparse import ArgumentParser, Namespace
from typing import TextIO
from ast_nodes import ProgramNode
from lexer import Lexer
from parser import Parser
from semantic_analyzer import SemanticAnalyzer


def main():
    arg_parser: ArgumentParser = ArgumentParser(
                                    prog='pascal_interpreter.py',
                                    description='Interprets programs written in basic Pascal')

    arg_parser.add_argument('filepath',
                            help='Pascal source file')

    arg_parser.add_argument('-s', '--scope',
                            help='Log scope information', action='store_true')

    args: Namespace = arg_parser.parse_args()

    print(args)

    input_file_name: str = args.filepath
    input_file: TextIO = open(input_file_name, 'r')
    input_text: str = input_file.read()

    lexer: Lexer = Lexer(input_text)
    parser: Parser = Parser(lexer)
    prog: ProgramNode = parser.parse()
    symtab: SemanticAnalyzer = SemanticAnalyzer(debug=args.scope)
    symtab.visit(prog)


if __name__ == '__main__':
    main()
