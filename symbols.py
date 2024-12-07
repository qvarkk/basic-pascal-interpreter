from typing import Self


class Symbol(object):
    def __init__(self, name: str, type: Self | any = None) -> None:
        self.name: str = name
        self.type: any = type


class BuiltinTypeSymbol(Symbol):
    def __init__(self, name: str) -> None:
        super().__init__(name)

    def __str__(self):
        return f'<BuiltInTypeSymbol(name={self.name})>'

    __repr__ = __str__


class VariableSymbol(Symbol):
    def __init__(self, name: str, type: Symbol) -> None:
        super().__init__(name, type)

    def __str__(self):
        return f'<VarSymbol(name={self.name}, type={self.type})>'

    __repr__ = __str__


class ProcedureSymbol(Symbol):
    def __init__(self, name: str, parameters: list[Symbol] | None = None):
        super().__init__(name)
        self.parameters: list[Symbol] = parameters if parameters is not None else []

    def __str__(self):
        return f'<ProcedureSymbol(name={self.name}, parameters={self.parameters})>'

    __repr__ = __str__
