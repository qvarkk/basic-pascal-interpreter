class AST(object):
    pass


class Program(AST):
    def __init__(self, name, block_node):
        self.name = name
        self.block = block_node


class Block(AST):
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement


class VarDecl(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class ProcedureDecl(AST):
    def __init__(self, name, parameters, block_node):
        self.name = name
        self.parameters = parameters
        self.block_node = block_node


class ParameterDecl(AST):
    def __init__(self, var_node, type_node):
        self.parameter = var_node
        self.type_node = type_node


class Type(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

    def __str__(self):
        return f'<Type(name={self.value})>'


class Compound(AST):
    def __init__(self):
        self.children = []


class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(AST):
    def __init__(self, token):
        self.token = token
        self.name = token.value


class NoOp(AST):
    pass


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

    def __str__(self):
        return f'BinOp(left: {self.left}, op: {self.op}, right: {self.right})'


class UnaryOp(AST):
    def __init__(self, op, factor):
        self.token = self.op = op
        self.factor = factor


class Number(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

    def __str__(self):
        return f'Number(value: {self.value})'

