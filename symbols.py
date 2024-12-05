class Symbol(object):
    def __init__(self, name, type=None):
        self.name = name
        self.type = type


class BuiltinTypeSymbol(Symbol):
    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return f'<BuiltInTypeSymbol(name={self.name})>'

    __repr__ = __str__


class VarSymbol(Symbol):
    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return f'<VarSymbol(name={self.name}, type={self.type})>'

    __repr__ = __str__


class ProcedureSymbol(Symbol):
    def __init__(self, name, parameters=None):
        super().__init__(name)
        self.parameters = parameters if parameters is not None else []

    def __str__(self):
        return f'<ProcedureSymbol(name={self.name}, parameters={self.parameters})>'

    __repr__ = __str__
