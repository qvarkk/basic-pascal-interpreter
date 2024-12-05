import argparse
from typing import TextIO
from lexer import Lexer
from parser import Parser
from source2source_pascal import Source2SourcePascalParser


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('--filepath', action='store', dest='filepath', default='test.pas')
    args = parser.parse_args()
    input_file_name: str = args.filepath
    input_file: TextIO = open(input_file_name, 'r')
    input_text: str = input_file.read()

    file_name = 'out.txt'
    s2s = Source2SourcePascalParser(file_name)

    lexer = Lexer(input_text)
    parser = Parser(lexer)
    program = parser.parse()

    s2s.visit(program)
    print('\nSuccess! Check out.txt file!')


if __name__ == '__main__':
    main()
