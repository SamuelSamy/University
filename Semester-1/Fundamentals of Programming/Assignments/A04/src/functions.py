"""
  Program functionalities module
"""

from os import truncate


def init_transactions():
    """
    Creates and returns a list of transactions, used to initialize `transactions` in the main() function

    Returns:
        [list of dictionaries]: A new list of transactions
    """
    transactions = []

    add_transaction(transactions, create_transaction(4, 20, "out", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(5, 30, "out", "soda"), store_operation = False)
    add_transaction(transactions, create_transaction(10, 45, "out", "pizza"), store_operation = False)
    add_transaction(transactions, create_transaction(15, 500, "in", "payment"), store_operation = False)
    add_transaction(transactions, create_transaction(12, 500, "out", "components"), store_operation = False)
    add_transaction(transactions, create_transaction(19, 75, "out", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(17, 3000, "in", "salary"), store_operation = False)
    add_transaction(transactions, create_transaction(11, 40, "out", "taxi"), store_operation = False)
    add_transaction(transactions, create_transaction(29, 100, "out", "taxi"), store_operation = False)
    add_transaction(transactions, create_transaction(30, 1000, "out", "pc"), store_operation = False)

    return transactions


def create_transaction(day, money, _type, description):
    """
    Returns a dictionary represnting a transaction

    Args:
        day (int): The day in which the transaction occurred
        money (int): The amount of money transfed
        _type (string): `in` / `out`
        description (string): The description of the transaction

    Raises:
        ValueError: If <day> is lower than 1 or greater than 30
        ValueError: If `_type` is not `in` or `out`

    Returns:
        [dictionary]: A dictionary that represents the transaction
    """

    if day < 1 or day > 30:
        raise ValueError("`day` can not be lower than 1 or greater than 30")

    if _type not in ['in', 'out']:
        raise ValueError("`type` must be either `in` or `out`")

    return {
        "day": day,
        "money": money,
        "type": _type,
        "description": description
    }


def get_day(transaction):
    """
    Returns the day in which the transaction occured

    Args:
        transaction (dictionary): The transaction object

    Returns:
        [int]: The day in which the transaction occured
    """
    return transaction['day']


def get_money(transaction):
    """
    Returns the amount of money that were transfered

    Args:
        transaction (dictionary): The transaction object

    Returns:
        [int]: The amount of money that were transfered
    """
    return transaction['money']


def get_type(transaction):
    """
    Returns the type of the transaction

    Args:
        transaction (dictionary): The transaction object

    Returns:
        [string]: The type of the transaction ('in' or 'out')
    """
    return transaction['type']


def get_description(transaction):
    """
    Returns the description of the transaction

    Args:
        transaction (dictionary): The transaction object

    Returns:
        [string]: The description of the transaction
    """
    return transaction['description']


def get_transaction(transactions, index):
    """
    Returns the transaction found at index = `index` from the transactions list

    Args:
        transactions (list of dictionary): A list of transactions

    Returns:
        [dictionary]: A dictionary representing the transactions with index = `index`
    """
    return transactions[index]


def set_money(transaction, value):
    """
    Sets the `money` amount to `value`

    Args:
        transaction (dictionary): The transaction dictionary
        value (int): The new amount of money

    Raises:
        ValueError: If <value> is not an int
    """
 
    if type(value) != int:
        raise ValueError(f"Can not set `money` value to {value}")

    transaction['money'] = value


def add_transaction(transactions, transaction, operations = None, index = None, store_operation = True):
    """
    Appends `transaction` to `transactions`
    Stores the reverse opeartion

    Args:
        transactions (list): A list of transactions
        transaction (dictionary): A dictionary representing a transaction
    """

    transactions.append(transaction)

    if store_operation:
        add_operation(operations, create_operation(index, transaction, "remove"))
        


def remove_transaction(transactions, transaction, operations = None, index = None, store_operation = True):
    """
    Removes `transaction` from `transactions`

    Args:
        transactions (list): A list of transactions
        transaction (dictionary): A dictionary representing a transaction
    """

    transactions.remove(transaction)

    if store_operation:
        add_operation(operations, create_operation(index, transaction, "add"))

# [New

def create_operation(index, transaction, command):
    """
    Creates and returns an 'operation' disctionary ({"index", "transaction", "command"})

    Args:
        index (int): Index of the command
        transaction (dict): A dictionary representing a transaction 
        command (string): 'add', 'remove' or 'replace'

    Raises:
        ValueError: if `command` is not 'add', 'remove' or 'replace'

    Returns:
        [disctionary]: A dictionary representing the operation
    """
    if command not in ['add', 'remove', 'replace']:
        raise ValueError("`command` must be either `add`, `remove` or `replace`")

    return {
        "index": index,
        "transaction": transaction,
        "command": command
    }


def get_operation_index(operation):
    """
    Returns the command index of an operation

    Args:
        operation (dict): A dictionary representing an operation

    Returns:
        [int]: Command index of the specified operation
    """

    return operation['index']


def get_operation_transaction(operation):
    """
    Returns the transaction of an operation

    Args:
        operation (dict): A dictionary representing an operation

    Returns:
        [dict]: A dictionary representing a transaction
    """

    return operation['transaction']

    
def get_operation_command(operation):
    """
    Retrurns the command word of an operation

    Args:
        operation (dict): A dictionary representing an operation

    Returns:
        [string]: A command word ('add` / 'remove' / 'replace')
    """
    return operation['command']


def get_operation(operations, index):
    """
    Returns operations[index]

    Args:
        transactions (list of dictionary): A list of operations

    Returns:
        [dictionary]: A dictionary representing an operation
    """
    
    return operations[index]


def add_operation(operations, operation):
    """
    Appends `operation` to `operations`

    Args:
        operations (list): A list of operations
    """
    operations.append(operation)


def remove_operation(operations, operation):
    """
    Removes `operation` from `operations`

    Args:
        operations (list): A list of operations
        operation (dictionary): A dictionary representing a transaction
    """
    operations.remove(operation)


def revert_replace_transaction(transactions, to_find_transaction):
    """
    Reverts a replace command

    Args:
        transactions (list): A list of transactions
        to_find_transaction (dict): A dictionary representing the transaction before it was modified
    """
    for transaction in transactions:

        if  get_day(transaction) == get_day(to_find_transaction) and \
              get_type(transaction) == get_type(to_find_transaction) and \
                get_description(transaction) == get_description(to_find_transaction):
            set_money(transaction, get_money(to_find_transaction))


def revert_last_operation(transactions, operations, index):
    """
    Reverts the last operation that modified the program's data

    Args:
        transactions (list): A list of transactions
        operations (list): A list of operations
        index (int): Index of the last operation
    """

    last_operation_index = get_operation_index(get_operation(operations, -1))

    while len(operations) != 0 and last_operation_index == get_operation_index(get_operation(operations, -1)):

        operation = get_operation(operations, -1)     
        transaction = get_operation_transaction(operation)

        if get_operation_command(operation) == "add":
            add_transaction(transactions, transaction, store_operation = False)
        elif get_operation_command(operation) == "remove":
            remove_transaction(transactions, transaction, store_operation = False)
        elif get_operation_command(operation) == "replace":
            revert_replace_transaction(transactions, transaction)

        remove_operation(operations, operation)

    index -= 1


def compute_total(transactions, _type):
    
    """
    Computes the total amount of money from `_type` transactions

    Args:
        transactions (list): A list of transactions
        params (string): The command params ('in' / 'out' and 'day')

    Raises:
        ValueError: if `_type` is not 'in' / 'out'

    Returns:
        [int]: The total amount of money from `_type` transactions
    """
    
    if _type not in ['in', 'out']:
        raise ValueError("`type` must be 'in' or 'out'")
    
    total = 0

    for transaction in transactions:
        if get_type(transaction) == _type:
            total += get_money(transaction)
      

    return total


def find_maxim(transactions, params):
    """
    Search and return the maximum `type` (params[0]) transaction on day `day` (params[1])

    Args:
        transactions ([type]): [description]
        params (list):  params[0] -> type
                        params[1] -> day

    Raises:
        ValueError: if `type` (params[0]) not in ['in', 'out'] 
        ValueError: if `day` (params[1]) is not an integer
        ValueError: if `day` (params[1]) is lower than 1 or higher than 30
        ValueError: if no maximum transaction was found

    Returns:
        [dict]: A dictionary representing the maximum transactions or None
    """

    _type = params[0]
    day = params[1]

    if _type not in ['in', 'out']:
        raise ValueError("`type` must be 'in' or 'out'")

    try:
        day = int(day)
    except:
        raise ValueError("`day` must be an integer")

    if day < 1 or day > 30:
        raise ValueError("`day` can not be lower than 1 or higher than 30")

    maxim = 0
    max_transaction = None

    for transaction in transactions:

        if get_day(transaction) == day and get_type(transaction) == _type:
            if maxim < get_money(transaction):
                maxim = get_money(transaction)
                max_transaction = transaction

    if max_transaction is None:
        raise ValueError(f"No `{_type}` transactions found on day {day}")

    return max_transaction


def filter_transactions(transactions, params, operations, index, store_operation = True):
    """
    Removes all transactions that are not the same type as params[0] or have smaller amount of money than params[1] (if params[1] is specified)


    Args:
        transactions ([type]): [description]
        params (string):    `type` (params[0]) - the type ('in' / 'out') that will be kept
                            `value` (params[1]) - optional - the minimum amount of money in order to not remove the tansaction
        operations (list): A list of operations
        index (int): Index of the last operation performed

    Raises:
        ValueError: if `type` (params[0]) is not in ['in', 'out']
        ValueError: if `value` (params[1]) is specified and it is not an integer
    """

    _type = params[0]
    amount = params[1] if len(params) == 2 else None

    if _type not in ['in', 'out']:
        raise ValueError("`type` must be 'in' or 'out'")

    try:
        if amount is not None:
            amount = int(amount)
    except:
        raise ValueError("`value` must be an integer")

    i = 0

    while i < len(transactions):
        transaction = get_transaction(transactions, i)

        if  (get_type(transaction) != _type) or \
            (amount is not None and amount <= get_money(transaction)):
            
            remove_transaction(transactions, transaction, operations, index, store_operation)
            i -= 1

        i +=  1
       
# New]

def split_command(command):
    """
    Splits the user command into command word and parameters

    Args:
        command (string): User command

    Returns:
        [string, string]:   • command_word
                            • command_params
    """

    command = command.strip()

    aux = command.split(maxsplit = 1)

    command_word = aux[0].strip().lower() if aux else None
    command_params = aux[1].strip().lower() if aux and len(aux) == 2 else None

    return command_word, command_params


def remove_transactions_from_start_to_end(transactions, start, end, operations, index):
    
    """
    Removes all transacions that have been made between `start` day and `end` day

    Args:
        transactions (list): A list of transactions
        start (int): An integer representing the start day
        end (int):  An integer representing the end day

    Raises:
        ValueError: if `start` is lower than 1 or higher than 30
        ValueError: if `end` is lower than 1 or higher than 30
        ValueError: if `start` is higer than `end`
    """
    
    try:
        start = int(start)
        end = int(end)
    except:
        raise ValueError("`start_day` and `end_day` must be integers")

    if start < 1 or start > 30:
        raise ValueError("`start_day` can not be lower than 1 or greater than 30")

    if end < 1 or end > 30:
        raise ValueError("`end_end` can not be lower than 1 or greater than 30")

    if start > end:
        raise ValueError("`end_day` can not be lower than `start_day`")

    i = 0

    while i < len(transactions):
        transaction = get_transaction(transactions, i)

        if get_day(transaction) >= start and get_day(transaction) <= end:
            remove_transaction(transactions, transaction, operations, index)
            i -= 1

        i +=  1


def remove_transactions_of_type(transactions, _type, operations, index):
    """
    Removes all transacions that have the same type as `_type` ('in' / 'out')

    Args:
        transactions (list): A list of transactions
        _type (string): 'in' / 'out'

    Raises:
        ValueError: if `_type` is not 'in' or 'out'
    """

    if _type not in ['in', 'out']:
        raise ValueError("incorrect usage of `remove` command")

    i = 0

    while i < len(transactions):
        transaction = get_transaction(transactions, i)

        if (get_type(transaction) == _type):
            remove_transaction(transactions, transaction, operations, index)
            i -= 1

        i += 1



def remove_transactions_by_day(transactions, day, operations, index):
    """
    Removes all transactions from a specified day

    Args:
        transactions (list): A list of transactions
        day (int): The day in which the transaction occurred

    Raises:
        ValueError: if `_type` is not 'in' or 'out'
    """

    try:
        day = int(day)
    except:
        raise ValueError("`day` must be a number!")

    if day < 1 or day > 30:
        raise ValueError("`day` can not be lower than 1 or greater than 30")
    

    i = 0

    while i < len(transactions):

        transaction = get_transaction(transactions, i)

        if get_day(transaction) == day:
            remove_transaction(transactions, transaction, operations, index)
            i -= 1
        
        i += 1


def replace_transaction(transactions, params, operations, index):
    """
    Replaces the amount for the `_type` transaction having the `description` description from day `day` with `money` 

    Args:
        transactions (list): A list of transactions
        params (list): A list of params representing the following:
            - day (int): The day in which the transaction occurred (params[0])
            - _type (string): The type of the transaction ('in' or 'out') (params[1])
            - description (string): The description of the transaction (params[2])
            - money (int): The amount of money (params[4])

    Raises:
        ValueError: if `day` or `money` are not integers
        ValueError: if `day`is lower than 1 or higer than 30
        ValueError: if `_type` is not 'in' or 'out'
        ValueError: if there was no transaction found with the specified values
    """    

    day = params[0] 
    _type = params[1]
    description=params[2]
    money = params[4]


    try:
        day = int(day)
        money = int(money)
    except:
        raise ValueError("`day` and `money_value` must be integers!")

    if day < 1 or day > 30:
        raise ValueError("`day` can not be lower than 1 or greater than 30")
    
    if _type not in ['in', 'out']:
        raise ValueError("incorrect usage of `replace` command")


    for transaction in transactions:
        if  get_day(transaction) == day and get_type(transaction) == _type and get_description(transaction) == description:
            add_operation(operations, create_operation(index, transaction.copy(), "replace"))
            set_money(transaction, money)
            return

    raise ValueError("can not find transaction with specified values")


def generate_transactions_of_type(transactions, _type):
    """
    Creates and returns a list of transactions of type `_type`

    Args:
        transactions (list of dictionaries): A list of transactions
        _type (string): The type of the transaction ('in' or 'out')


    Returns:
        [list]: A list with all the transactions that have `type` = `_type`

    Raises:
        ValueError: if `_type` is not 'all', 'in' or 'out'
    """    

    if _type not in ['all', 'in', 'out']:
        raise ValueError("incorrect usage of `list` command")


    new_transactions = []

    for transaction in transactions:
        if _type == 'all' or _type == get_type(transaction):
            add_transaction(new_transactions, transaction, store_operation = False)

    return new_transactions


def get_transactions_based_on_money(transactions,sign, value):
    """
    Creates and returns a list based on the values of `sign` and `value`

    Args:
        transactions (list): A list of transactions
        sign (string): The sign (`<`, `=`, `>`)
        value (int): The money value

    Returns:
        [list]: A new list containing the new transaction

    Raises:
        ValueError: if `day` is not an integers
        ValueError: if `sign`is not '<', '=' or '>'
    """   

    if sign not in ['>', '=', '<']:
        raise ValueError("• incorrect usage of `list` command [`>` | `=` | `<`]")

    try:
        value = int(value)
    except:
        raise ValueError("`money_value` must be an integer")


    new_transactions = []

    for transaction in transactions:
        if  (sign == '>' and get_money(transaction) > value) or \
            (sign == '=' and get_money(transaction) == value) or \
            (sign == '<' and get_money(transaction) < value):
            add_transaction(new_transactions, transaction, store_operation = False)

    return new_transactions


def compute_balance(transactions, day):
    """
    Computes the total balance at the end of the day `day`

    Args:
        transactions (list): A list of transactions
        day (int): The specified day

    Returns:
        [int]: The balance at the end of day `day`

    Raises:
        ValueError: if `day` is not an integers
        ValueError: if `day` is lower than 1 or higher than 30
    """   

    try:
        day = int(day)
    except:
        raise ValueError("`day` must be an integer")

    if day < 1 or day > 30:
        raise ValueError("`day` can not be lower than 1 or greater than 30")

    total = 0

    for transaction in transactions:
        if get_day(transaction) <= day:
            if get_type(transaction) == 'in':
                total += get_money(transaction)
            else:
                total -= get_money(transaction)

    return total



"""
    Tests
"""

def test_create_transaction():

    transaction = create_transaction(
        day = 10,
        money = 20,
        _type = 'in', 
        description = 'test'
    )
    
    assert transaction == {'day': 10, 'money': 20, 'type': 'in', 'description': 'test'}


def test_get_day():
    transaction = create_transaction(
        day = 10,
        money = 20,
        _type = 'in', 
        description = 'test'
    )
    
    assert get_day(transaction) == 10


def test_get_money():
    transaction = create_transaction(
        day = 10,
        money = 20,
        _type = 'in', 
        description = 'test'
    )
    
    assert get_money(transaction) == 20


def test_get_type():
    transaction = create_transaction(
        day = 10,
        money = 20,
        _type = 'in', 
        description = 'test'
    )
    
    assert get_type(transaction) == 'in'


def test_get_description():
    transaction = create_transaction(
        day = 10,
        money = 20,
        _type = 'in', 
        description = 'test'
    )
    
    assert get_description(transaction) == 'test'


def test_get_transaction():
    transactions = [
        create_transaction(day = 10, money = 10, _type = 'in', description = 'test'),
        create_transaction(day = 15, money = 50, _type = 'out', description = 'test'),
        create_transaction(day = 20, money = 25, _type = 'in', description = 'test')
    ]

    assert get_transaction(transactions, 0) == create_transaction(day = 10, money = 10, _type = 'in', description = 'test')
    assert get_transaction(transactions, 1) == create_transaction(day = 15, money = 50, _type = 'out', description = 'test')
    assert get_transaction(transactions, 2) == create_transaction(day = 20, money = 25, _type = 'in', description = 'test')


def test_set_money():
    
    transaction = create_transaction(
        day = 10,
        money = 20,
        _type = 'in', 
        description = 'test'
    )

    set_money(transaction, 100)

    assert transaction == create_transaction(day = 10, money = 100, _type = 'in', description = 'test')


def test_add_transaction():
    transactions = [
        create_transaction(
            day = 10,
            money = 20,
            _type = 'in', 
            description = 'test'
        )
    ]

    add_transaction(transactions, create_transaction(day = 15, money = 30, _type = 'out', description = 'test'), store_operation = False)

    assert transactions == [
        create_transaction(day = 10, money = 20, _type = 'in', description = 'test'), 
        create_transaction(day = 15, money = 30, _type = 'out', description = 'test')
    ]


def test_split_command():   

    assert split_command("add 10 in test") == ('add', '10 in test')
    assert split_command("insert 5 10 in test") ==  ('insert', '5 10 in test')
    assert split_command("remove 5") == ('remove', '5')
    assert split_command("remove 5 to 10") == ('remove', '5 to 10')
    assert split_command("replace 10 in test with 100") == ('replace', '10 in test with 100')
    assert split_command("list") == ('list', None)
    assert split_command('list in') == ('list', 'in')
    assert split_command("list < 100") == ('list', '< 100')
    assert split_command("list balance 10") == ('list', 'balance 10')


def test_remove_transactions_from_start_to_end():
    
    transactions = [
        create_transaction(day = 5, money = 10, _type = 'in', description = 'test'),
        create_transaction(day = 6, money = 10, _type = 'in', description = 'test'),
        create_transaction(day = 8, money = 10, _type = 'in', description = 'test'),
        create_transaction(day = 10, money = 10, _type = 'in', description = 'test'),
    ]

    transactions_left = [
        create_transaction(day = 5, money = 10, _type = 'in', description = 'test'),
        create_transaction(day = 10, money = 10, _type = 'in', description = 'test'),
    ]

    operations = []

    remove_transactions_from_start_to_end(transactions, start = 6, end = 9, operations = operations, index = 0)

    assert transactions == transactions_left


def test_remove_transactions_of_type():
    
    transactions = [
        create_transaction(day = 1, money = 10, _type = 'in', description = 'test'),
        create_transaction(day = 2, money = 20, _type = 'out', description = 'test'),
        create_transaction(day = 3, money = 35, _type = 'in', description = 'test'),
        create_transaction(day = 4, money = 15, _type = 'out', description = 'test'),
        create_transaction(day = 5, money = 30, _type = 'in', description = 'test'),
        create_transaction(day = 6, money = 25, _type = 'out', description = 'test')
    ]

    transactions_copy = transactions.copy()

    transaction_in = [
        create_transaction(day = 1, money = 10, _type = 'in', description = 'test'),
        create_transaction(day = 3, money = 35, _type = 'in', description = 'test'),
        create_transaction(day = 5, money = 30, _type = 'in', description = 'test'),
    ]

    transaction_out = [
        create_transaction(day = 2, money = 20, _type = 'out', description = 'test'),
        create_transaction(day = 4, money = 15, _type = 'out', description = 'test'),
        create_transaction(day = 6, money = 25, _type = 'out', description = 'test')
    ]

    operations = []

    remove_transactions_of_type(transactions, 'out', operations, 0)
    remove_transactions_of_type(transactions_copy, 'in', operations, 1)

    assert transactions == transaction_in
    assert transactions_copy == transaction_out


def test_remove_transactions_by_day():

    transactions = [
        create_transaction(day = 5, money = 10, _type = 'in', description = 'test'),
        create_transaction(day = 5, money = 20, _type = 'out', description = 'test'),
        create_transaction(day = 10, money = 30, _type = 'in', description = 'test'),
        create_transaction(day = 10, money = 40, _type = 'out', description = 'test'),
        create_transaction(day = 15, money = 50, _type = 'out', description = 'test')
    ]

    transactions_left = [
        create_transaction(day = 10, money = 30, _type = 'in', description = 'test'),
        create_transaction(day = 10, money = 40, _type = 'out', description = 'test'),
        create_transaction(day = 15, money = 50, _type = 'out', description = 'test')
    ]

    operations = []

    remove_transactions_by_day(transactions, 5, operations, 0)

    assert transactions == transactions_left


def test_replace_transaction():

    transactions = [
        create_transaction(day = 15, money = 75, _type = 'out', description = 'test'),
        create_transaction(day = 10, money = 100, _type = 'in', description = 'test'),
        create_transaction(day = 10, money = 30, _type = 'out', description = 'test')
    ]

    transactions_modified  = [
        create_transaction(day = 15, money = 75, _type = 'out', description = 'test'),
        create_transaction(day = 10, money = 500, _type = 'in', description = 'test'),
        create_transaction(day = 10, money = 30, _type = 'out', description = 'test')
    ]

    operations = []

    replace_transaction(transactions, [10, 'in', 'test', 'with', 500], operations, 0)
    
    assert transactions == transactions_modified


def test_generate_transactions_of_type():
    
    transactions = [
        create_transaction(5, 10, 'in', 'test'),
        create_transaction(5, 10, 'in', 'test'),
        create_transaction(5, 10, 'out', 'test'),
        create_transaction(5, 10, 'out', 'test'),
    ]

    transactions_all = generate_transactions_of_type(transactions, 'all')
    transactions_in = generate_transactions_of_type(transactions, 'in')
    transactions_out = generate_transactions_of_type(transactions, 'out')

    assert transactions_all == transactions
    assert transactions_in == [create_transaction(5, 10, 'in', 'test'), create_transaction(5, 10, 'in', 'test')]
    assert transactions_out == [create_transaction(5, 10, 'out', 'test'), create_transaction(5, 10, 'out', 'test')]


def test_get_transactions_based_on_money():
    transactions = [
        create_transaction(5, 10, 'in', 'test'),
        create_transaction(5, 30, 'out', 'test'),
    ]

    transactions_smaller_than_25 = get_transactions_based_on_money(transactions, '<', 25)
    transactions_greater_than_25 = get_transactions_based_on_money(transactions, '>', 25)
    transactions_equal_with_10 = get_transactions_based_on_money(transactions, '=', 10)

    assert transactions_smaller_than_25 == [create_transaction(5, 10, 'in', 'test')]
    assert transactions_greater_than_25 == [create_transaction(5, 30, 'out', 'test')]
    assert transactions_equal_with_10 == [create_transaction(5, 10, 'in', 'test')]


def test_compute_balance():
    transactions = [
        create_transaction(5, 10, 'out', 'test'),
        create_transaction(5, 20, 'out', 'test'),
        create_transaction(5, 30, 'in', 'test'),
        create_transaction(5, 40, 'in', 'test'),
    ]

    total = compute_balance(transactions, 5)

    assert total == 40


def test_remove_transaction():

    transactions = []

    add_transaction(transactions, create_transaction(5, 1, "out", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(5, 5, "in", "food"), store_operation = False)

    remove_transaction(transactions, create_transaction(5, 5, "in", "food"), store_operation = False)

    assert transactions == [create_transaction(5, 1, "out", "food")]


# [New


def test_create_operation():
    
    transaction = create_transaction(
        day = 10,
        money = 10,
        _type = 'in', 
        description = 'test'
    )

    operation = create_operation(
        index = 0,
        transaction = transaction,
        command = 'add'
    )

    assert operation == {
        "index": 0,
        "transaction": transaction,
        "command": 'add'
    }


def test_get_operation_index():

    transaction = create_transaction(
        day = 10,
        money = 20,
        _type = 'in', 
        description = 'test'
    )
    
    operation = create_operation(
        index = 0,
        transaction = transaction, 
        command = "add"
    )
    
    assert get_operation_index(operation) == 0


def test_get_operation_transaction():

    transaction = create_transaction(
        day = 10,
        money = 20,
        _type = 'in', 
        description = 'test'
    )
    
    operation = create_operation(
        index = 0,
        transaction = transaction, 
        command = "add"
    )
    
    assert get_operation_transaction(operation) == transaction


def test_get_operation_command():

    transaction = create_transaction(
        day = 10,
        money = 20,
        _type = 'in', 
        description = 'test'
    )
    
    operation = create_operation(
        index = 0,
        transaction = transaction, 
        command = "add"
    )
    
    assert get_operation_command(operation) == "add"


def test_get_operation():

    transactions = [
        create_transaction(day = 10, money = 10, _type = 'in', description = 'test'),
        create_transaction(day = 15, money = 50, _type = 'out', description = 'test'),
        create_transaction(day = 20, money = 25, _type = 'in', description = 'test')
    ]

    operations = [
        create_operation(
            index = 0, 
            transaction = get_transaction(transactions, 0),
            command = "add"
        ),
        create_operation(
            index = 1,
            transaction = get_transaction(transactions, 1),
            command = "remove"
        )
    ]

    assert get_operation(operations, 1) == create_operation(index = 1, transaction = get_transaction(transactions, 1), command = "remove")

def test_add_operation():
    operations = [
        create_operation(
            index = 0, 
            transaction = create_transaction(
                day = 10,
                money = 20,
                _type = 'in', 
                description = 'test'
            ),
            command = "add"
            
        )
    ]

    add_operation(operations, create_operation(1, create_transaction(day = 15, money = 30, _type = 'out', description = 'test'), "add"))

    assert operations == [
        create_operation(
            index = 0, 
            transaction = create_transaction(day = 10, money = 20, _type = 'in', description = 'test'),
            command = "add"
        ),
        create_operation(
            index = 1,
            transaction = create_transaction(day = 15, money = 30, _type = 'out', description = 'test'),
            command = "add"
        )
    ]


def test_remove_operation():
    operations = [
       create_operation(
            index = 0, 
            transaction = create_transaction(day = 10, money = 20, _type = 'in', description = 'test'),
            command = "add"
        ),
        create_operation(
            index = 1,
            transaction = create_transaction(day = 15, money = 30, _type = 'out', description = 'test'),
            command = "add"
        )
    ]

    remove_operation(operations, create_operation(1, create_transaction(day = 15, money = 30, _type = 'out', description = 'test'), "add"))

    assert operations == [
        create_operation(
            index = 0, 
            transaction = create_transaction(day = 10, money = 20, _type = 'in', description = 'test'),
            command = "add"
        )
    ]


def test_compute_total():
    
    transactions = []

    add_transaction(transactions, create_transaction(1, 1, "out", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(2, 5, "in", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(3, 2, "out", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(4, 5, "in", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(5, 3, "out", "food"), store_operation = False)

    assert compute_total(transactions, 'in') == 10
    assert compute_total(transactions, 'out') == 6


def test_find_maxim():

    transactions = []

    add_transaction(transactions, create_transaction(5, 1, "out", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(5, 5, "in", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(5, 2, "out", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(5, 10, "in", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(5, 3, "out", "food"), store_operation = False)


    assert find_maxim(transactions, ['in', 5]) == create_transaction(5, 10, "in", "food")
    assert find_maxim(transactions, ['out', 5]) == create_transaction(5, 3, "out", "food")


def test_filter_transactions():

    transactions = []

    add_transaction(transactions, create_transaction(5, 1, "out", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(5, 5, "in", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(5, 2, "out", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(5, 10, "in", "food"), store_operation = False)
    add_transaction(transactions, create_transaction(5, 5, "out", "food"), store_operation = False)

    operations = []
    index = 0

    in_transactions = transactions.copy()
    out_transaction = transactions.copy()

    filter_transactions(in_transactions, ['in'], operations, index, store_operation = False)
    filter_transactions(out_transaction, ['out', 3], operations, index, store_operation = False)

    assert in_transactions == [create_transaction(5, 5, "in", "food"), create_transaction(5, 10, "in", "food")]
    assert out_transaction == [create_transaction(5, 1, "out", "food"), create_transaction(5, 2, "out", "food")]


def test_revert_replace_transaction():

    transactions = []

    add_transaction(transactions, create_transaction(day = 10, money = 10, _type = 'in', description = 'test'), store_operation = False)
    add_transaction(transactions, create_transaction(day = 15, money = 50, _type = 'out', description = 'test'), store_operation = False)
    add_transaction(transactions, create_transaction(day = 20, money = 25, _type = 'in', description = 'test'), store_operation = False)

    initial_transactions = transactions.copy()

    operations = []

    replace_transaction(transactions, ["10", "in", "test", "with", 100], operations, 0)
    revert_replace_transaction(transactions, get_transaction(transactions, 0))
    
    assert initial_transactions == transactions


def test_revert_last_operation():

    transactions = []
    operations = []
    index = 0

    add_transaction(transactions, create_transaction(day = 10, money = 10, _type = 'in', description = 'test'), operations, index, store_operation = True)
    index += 1
    add_transaction(transactions, create_transaction(day = 15, money = 50, _type = 'out', description = 'test'), operations, index, store_operation = True)
    index += 1

    revert_last_operation(transactions, operations, index)

    assert transactions == [create_transaction(day = 10, money = 10, _type = 'in', description = 'test')]

    revert_last_operation(transactions, operations, index)

    assert transactions == []

    add_transaction(transactions, create_transaction(day = 15, money = 50, _type = 'out', description = 'test'), operations, index, store_operation = True)
    index += 1
    remove_transaction(transactions, create_transaction(day = 15, money = 50, _type = 'out', description = 'test'), operations, index, store_operation = True)
    index += 1

    revert_last_operation(transactions, operations, index)

    assert transactions == [create_transaction(day = 15, money = 50, _type = 'out', description = 'test')]


# New]

def run_all_tests():

    test_create_transaction()
    test_get_day()
    test_get_money()
    test_get_type()
    test_get_description()
    test_get_transaction()
    test_set_money()
    test_add_transaction()
    test_split_command()
    test_remove_transactions_from_start_to_end()
    test_remove_transactions_of_type()
    test_remove_transactions_by_day()
    test_replace_transaction()
    test_generate_transactions_of_type()
    test_get_transactions_based_on_money()
    test_compute_balance()
    test_remove_transaction()

    test_create_operation()
    test_get_operation_index()
    test_get_operation_transaction()
    test_get_operation_command()
    test_get_operation()
    test_add_operation()
    test_remove_operation()
    test_compute_total()
    test_find_maxim()
    test_filter_transactions()
    test_revert_replace_transaction()
    test_revert_last_operation()


