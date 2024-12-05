from node_visitor import NodeVisitor
from symbols import VarSymbol, ProcedureSymbol
from symbol_table import ScopedSymbolTable


class SemanticAnalyzer(NodeVisitor):
    def __init__(self):
        self.current_scope = None

    def visit_Program(self, node):
        self.current_scope = ScopedSymbolTable('builtins', 0)
        self.current_scope.init_builtins()

        global_scope = ScopedSymbolTable(
            scope_name='global',
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope
        )

        self.current_scope = global_scope
        self.visit(node.block)

        self.current_scope = self.current_scope.enclosing_scope

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.current_scope.lookup(type_name)
        var_name = node.var_node.name

        if self.current_scope.lookup(var_name, current_scope_only=True) is not None:
            raise Exception(f'Duplicate identifier {var_name} found')

        self.current_scope.define(VarSymbol(var_name, type_symbol))

    def visit_ProcedureDecl(self, node):
        procedure_name = node.name
        procedure_symbol = ProcedureSymbol(procedure_name)
        self.current_scope.define(procedure_symbol)

        procedure_scope = ScopedSymbolTable(
            scope_name=procedure_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope)

        self.current_scope = procedure_scope

        for parameter in node.parameters:
            type_symbol = self.current_scope.lookup(parameter.type_node.value)
            param_name = parameter.parameter.name
            param_symbol = VarSymbol(param_name, type_symbol)
            self.current_scope.define(param_symbol)
            procedure_symbol.parameters.append(param_symbol)

        self.visit(node.block_node)
        self.current_scope = self.current_scope.enclosing_scope

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node):
        var = self.current_scope.lookup(node.left.name)

        if var is None:
            raise NameError(f'Variable {node.left.name} not found')

        self.visit(node.right)

    def visit_Var(self, node):
        var = self.current_scope.lookup(node.name)

        if var is None:
            raise NameError(f'Variable {node.name} not found')

    def visit_NoOp(self, node):
        pass

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_Number(self, node):
        pass

    def visit_UnaryOp(self, node):
        self.visit(node.factor)
