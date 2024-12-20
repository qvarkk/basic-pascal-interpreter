from typing import TextIO
from ast_nodes import *
from node_visitor import NodeVisitor
from symbol_table import ScopedSymbolTable
from symbols import *


class Source2SourcePascalParser(NodeVisitor):
    def __init__(self, file_name: str) -> None:
        self.program_lines: list[str] = []
        self.current_scope: ScopedSymbolTable | None = None
        self.file: TextIO = open(file_name, 'w')

    def __del__(self):
        self.file.close()

    def get_tabulation(self, offset: int = 0) -> str:
        if self.current_scope is None:
            raise Exception(f'Unexpected error happened')

        return '\t' * (self.current_scope.scope_level + offset)

    def add_line_to_output(self, line: str) -> None:
        self.program_lines.append(line)

    def output_lines(self) -> None:
        self.file.writelines('\n'.join(self.program_lines))

    def visit_ProgramNode(self, node: ProgramNode) -> None:
        self.current_scope = ScopedSymbolTable(
            scope_name='builtins',
            scope_level=0
        )
        self.current_scope.init_builtins()

        global_scope: ScopedSymbolTable = ScopedSymbolTable(
            scope_name='global',
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope
        )

        self.current_scope = global_scope

        self.add_line_to_output('program {program_name};'.format(
            program_name=node.name + str(self.current_scope.scope_level - 1)
        ))

        self.visit(node.block_node)

        self.add_line_to_output('{tabulation}end; {{ END OF {program_name} }}'.format(
            tabulation=self.get_tabulation(-1),
            program_name=node.name + str(self.current_scope.scope_level - 1)
        ))

        self.output_lines()
        self.current_scope = self.current_scope.enclosing_scope

    def visit_BlockNode(self, node: BlockNode) -> None:
        for declaration in node.declarations:
            self.visit(declaration)

        self.add_line_to_output('{tabulation}begin'.format(
            tabulation=self.get_tabulation(-1),
        ))
        self.visit(node.compound_node)

    def visit_VariableDeclarationNode(self, node: VariableDeclarationNode) -> None:
        if self.current_scope is None:
            raise Exception(f'Unexpected error happened')

        type_name: str = str(node.type_node.type)
        type_symbol: BuiltinTypeSymbol | None = self.current_scope.lookup(type_name)
        var_name: str = str(node.variable_node.name) + str(self.current_scope.scope_level)

        if self.current_scope.lookup(var_name, current_scope_only=True) is not None:
            raise Exception(f'Duplicate identifier {var_name} found')

        self.current_scope.define(VariableSymbol(var_name, type_symbol))

        var_line: str = '{tabulation}var {var_name} : {type_name};'.format(
            tabulation=self.get_tabulation(),
            var_name=var_name,
            type_name=type_name
        )
        self.add_line_to_output(var_line)

    def visit_ProcedureDeclarationNode(self, node: ProcedureDeclarationNode) -> None:
        if self.current_scope is None:
            raise Exception(f'Unexpected error happened')

        procedure_name: str = node.name + str(self.current_scope.scope_level)
        procedure_symbol: ProcedureSymbol = ProcedureSymbol(procedure_name)
        self.current_scope.define(procedure_symbol)

        procedure_scope: ScopedSymbolTable = ScopedSymbolTable(
            scope_name=procedure_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope
        )

        self.current_scope = procedure_scope

        parameters_lines: list[str] = []

        if node.parameters is not None:
            for parameter in node.parameters:
                type_symbol: BuiltinTypeSymbol | None = self.current_scope.lookup(parameter.type_node.type)
                param_name: str = parameter.variable_node.name
                param_symbol: VariableSymbol = VariableSymbol(param_name + str(self.current_scope.scope_level), type_symbol)
                self.current_scope.define(param_symbol)
                procedure_symbol.parameters.append(param_symbol)
                parameters_lines.append('{param_name} : {type_name}'.format(
                    param_name=param_name + str(self.current_scope.scope_level),
                    type_name=type_symbol.name
                ))

        self.add_line_to_output('')
        self.add_line_to_output('{tabulation}procedure {procedure_name}({parameters});'.format(
            tabulation=self.get_tabulation(-1),
            procedure_name=procedure_name,
            parameters='; '.join(parameters_lines)
        ))

        self.visit(node.block_node)

        # end is outside of block_node because there's no way to get name of the procedure inside visit_Block
        self.add_line_to_output('{tabulation}end; {{ END OF {procedure_name} }}'.format(
            tabulation=self.get_tabulation(-1),
            procedure_name=procedure_name,
        ))

        self.current_scope = self.current_scope.enclosing_scope

    def visit_CompoundNode(self, node: CompoundNode) -> None:
        for child in node.children:
            child_string: str = self.visit(child)
            if child_string is not None:
                child_string = self.get_tabulation() + child_string
                self.add_line_to_output(child_string)

        if len(node.children) == 1:
            self.add_line_to_output('')

    def visit_AssignNode(self, node: AssignNode) -> str:
        operator: str = node.operator_token.value
        left: str = self.visit(node.left_operand)
        right: str = self.visit(node.right_operand)
        return f'{left} {operator} {right};'

    def visit_VariableNode(self, node: VariableNode) -> str:
        if self.current_scope is None:
            raise Exception(f'Unexpected error happened')

        var: VariableSymbol | None = None

        for i in range(self.current_scope.scope_level, 0, -1):
            var = self.current_scope.lookup(node.name + str(i))
            if var is not None:
                break

        if var is None:
            raise NameError(f'Variable {node.name} not found')

        return f'<{var.name}:{var.type.name}>'

    def visit_NoOpNode(self, node: NoOpNode) -> None:
        pass

    def visit_BinaryOperationNode(self, node: BinaryOperationNode) -> str:
        operator: str = node.operator_token.value
        left: str = self.visit(node.left_operand)
        right: str = self.visit(node.right_operand)
        return f'{left} {operator} {right}'

    def visit_UnaryOperationNode(self, node: UnaryOperationNode) -> str:
        operator: str = node.operator_token.value
        operand: str = self.visit(node.operand)
        return f'{operator} {operand}'

    def visit_NumberNode(self, node: NumberNode) -> str:
        return str(node.value)

