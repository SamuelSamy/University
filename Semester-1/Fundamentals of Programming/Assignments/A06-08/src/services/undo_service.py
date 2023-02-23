from src.services.serv_expections import UndoException


class UndoService:

    def __init__(self):
        self._history = []
        self._index = 0
        self._record_flag = True


    def record_operation(self, operation):

        if self._record_flag is False:
            return

        self._history = self._history[:self._index]
        self._history.append(operation)
        self._index += 1


    def undo(self):

        if self._index > 0:

            self._record_flag = False

            self._index -= 1
            self._history[self._index].undo()

            self._record_flag = True

        else:
            raise UndoException("Nothing left to undo")


    def redo(self):

        if self._index < len(self._history):

            self._record_flag = False

            self._history[self._index].redo()
            self._index += 1

            self._record_flag = True

        else:
            raise UndoException("Nothing left to redo")



class Operation:
    def __init__(self, function_undo, function_redo):
        self._function_undo = function_undo
        self._function_redo = function_redo

    def undo(self):
        self._function_undo.call()

    def redo(self):
        self._function_redo.call()


class CascadedOperation:

    def __init__(self):
        self._operations = []

    def add(self, operation):
        self._operations.append(operation)

    def undo(self):
        for operation in self._operations:
            operation.undo()

    def redo(self):
        for operation in self._operations:
            operation.redo()


class FunctionCall:
    def __init__(self, function_name, *function_params):
        self._function_name = function_name
        self._function_params = function_params

    def call(self):
        self._function_name(*self._function_params)
