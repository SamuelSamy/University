from datetime import datetime

"""
  Write non-UI functions below
"""

def init_transactions():
    """
    Creates and returns a list of transactions, used to initialize `transactions` in the main() function

    Returns:
        [list of dictionaries]: A new list of transactions
    """
    transactions = []

    transactions.append(create_transaction(4, 20, "out", "food"))
    transactions.append(create_transaction(5, 100, "out", "pizza"))
    transactions.append(create_transaction(5, 50, "out", "soda"))
    transactions.append(create_transaction(7, 100, "out", "pizza"))
    transactions.append(create_transaction(8, 1000, "in", "salary2"))
    transactions.append(create_transaction(10, 1000, "in", "salary"))
    transactions.append(create_transaction(10, 20, "out", "food"))
    transactions.append(create_transaction(14, 500, "in", "payment"))
    transactions.append(create_transaction(21, 500, "out", "laptop"))
    transactions.append(create_transaction(27, 15, "out", "soda"))
    transactions.append(create_transaction(29, 1500, "out", "pc"))

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
        transactions (list of dictionary): A lit of transactions

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


def add_transaction(transactions, transaction):
    """
    Appends `transaction` to `transactions`

    Args:
        transactions (list): A list of transactions
        transaction (dictionary): A dictionary representing a transaction
    """

    transactions.append(transaction)


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


def remove_transactions_from_start_to_end(transactions, start, end):
    
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

    index = 0

    while index < len(transactions):
        transaction = get_transaction(transactions, index)

        if get_day(transaction) >= start and get_day(transaction) <= end:
            transactions.remove(transaction)
            index -= 1

        index +=  1


def remove_transactions_of_type(transactions, _type):
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

    index = 0

    while index < len(transactions):
        transaction = get_transaction(transactions, index)

        if (get_type(transaction) == _type):
            transactions.remove(transaction)
            index -= 1

        index += 1


def remove_transactions_by_day(transactions, day):
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
    

    index = 0

    while index < len(transactions):

        transaction = get_transaction(transactions, index)

        if get_day(transaction) == day:
            transactions.remove(transaction)
            index -= 1
        
        index += 1


def replace_transaction(transactions, day, money, _type, description):
    """
    Replaces the amount for the `_type` transaction having the `description` description from day `day` with `money` 

    Args:
        transactions (list): A list transactions
        day (int): The day in which the transaction occurred
        money (int): The amount of money
        _type (string): The type of the transaction ('in' or 'out')
        description (string): The description of the transaction

    Raises:
        ValueError: if `day` or `money` are not integers
        ValueError: if `day`is lower than 1 or higer than 30
        ValueError: if `_type` is not 'in' or 'out'
        ValueError: if there was no transaction found with the specified values
    """    

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
            set_money(transaction, money)
            return

    raise ValueError("can not find transaction with specified values")


def generate_transactions_of_type(transactions, _type):
    """
    Creates and returns a list where `type` is `_type`

    Args:
        transactions (list of dictionaries): A list of transactions
        _type (string): The type of the transaction ('in' or 'out')


    Returns:
        [list]: A list with all the transactions that have `type` = `_type`
    """    

    new_transactions = []

    for transaction in transactions:
        if _type == 'all':
            new_transactions.append(transaction)
        elif _type == get_type(transaction):
            new_transactions.append(transaction)


    return new_transactions


def get_transactions_based_on_money(transactions,sign, value):
    """
    Creates and returns a list based on the values of `sign` and `value`

    Args:
        transactions (list): A lit of transactions
        sign (string): The sign (`<`, `=`, `>`)
        value (int): The money value

    Returns:
        [list]: A new list containing the new transaction
    """    

    new_transactions = []

    for transaction in transactions:
        if sign == '>' and get_money(transaction) > value:
            new_transactions.append(transaction)
        elif sign == '=' and get_money(transaction) == value:
            new_transactions.append(transaction)
        elif sign == '<' and get_money(transaction) < value:
            new_transactions.append(transaction)

    return new_transactions


def compute_balance(transactions, day):
    """
    Computes the total balance at the end of the day `day`

    Args:
        transactions (list): A list of transactions
        day (int): The specified day

    Returns:
        [int]: The balance at the end of day `day`
    """   

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

    add_transaction(transactions, create_transaction(day = 15, money = 30, _type = 'out', description = 'test'))

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

    remove_transactions_from_start_to_end(transactions, start = 6, end = 9)

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

    remove_transactions_of_type(transactions, 'out')
    remove_transactions_of_type(transactions_copy, 'in')

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

    remove_transactions_by_day(transactions, 5)

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

    replace_transaction(transactions, day = 10, money = 500, _type = 'in', description = 'test')
    
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


"""
    Write the command-driven UI below
"""

# add command


def add_transaction_ui(transactions, command_params):

    params = command_params.split(' ') if command_params else []

    if len(params) != 3:
        raise ValueError(f"incorrect usage of `add` command")

    try:
        money = int(params[0])
    except:
        raise ValueError("`money_value` must be an integer")

    _type = params[1]
    description = params[2]
    day = datetime.now().day if datetime.now().day <= 30 else 30

    try:
        add_transaction(
            transactions, 
            transaction = create_transaction(
                day = day,
                money = money,
                _type = _type,
                description = description
            )
        )

        print("• Transaction added succsesfully")
    except ValueError as ve:
        print(f"• {ve}")


# insert command


def insert_transaction_ui(transactions, command_params):

    params = command_params.split(' ') if command_params else []

    if len(params) != 4:
        raise ValueError(f"incorrect usage of `insert` command")

    try:
        day = int(params[0])
        money = int(params[1])
    except:
        raise ValueError("`day` and `money_value` must be integers")

    _type = params[2]
    description = params[3]

    try:
        add_transaction(
            transactions, 
            transaction = create_transaction(
                day = day,
                money = money,
                _type = _type,
                description = description
            )
        )

        print("• Transaction inserted succsesfully")
    except ValueError as ve:
        print(f"• {ve}")


# list command


def print_transaction(transaction, index):
    print(f"{index}. Day: {get_day(transaction)} | Money: {get_money(transaction)} | Type: {get_type(transaction)} | Description: {get_description(transaction)}")


def print_transactions_of_type(transactions, _type):
    
    if _type not in ['all', 'in', 'out']:
        raise ValueError("• incorrect usage of `list` command")

    index = 0

    transactions_to_print = generate_transactions_of_type(transactions, _type)

    for transaction in transactions_to_print:
        print_transaction(transaction, index)
        index += 1
            
    if len(transactions_to_print) == 0:
        print("• No transactions found!")


def print_transactions_based_on_money(transactions, sign, value):

    if sign not in ['>', '=', '<']:
        raise ValueError("• incorrect usage of `list` command [`>` | `=` | `<`]")

    try:
        value = int(value)
    except:
        raise ValueError("`money_value` must be an integer")

    index = 0

    new_transactions = get_transactions_based_on_money(transactions, sign, value)
    
    for transaction in new_transactions:
        print_transaction(transaction, index)
        index += 1

    if len(new_transactions) == 0:
        print ("• No transactions found!")


def print_balance(transactions, day):

    try:
        day = int(day)
    except:
        raise ValueError("`day` must be an integer")

    if day < 1 or day > 30:
        raise ValueError("`day` can not be lower than 1 or greater than 30")

    balance = compute_balance(transactions, day)  

    print(f"The account’s balance at the end of day {day} is {balance} RON")


def print_transactions(transactions, command_params):

    params = command_params.split(' ') if command_params else []

    if len(params) == 0: # list
        print_transactions_of_type(transactions, 'all')
    elif len(params) == 1 and params[0] in ['in', 'out']: #list in 
        print_transactions_of_type(transactions, params[0])
    elif len(params) == 2 and params[0] in ['>', '=', '<']: # listlist > 100
        print_transactions_based_on_money(transactions, params[0], params[1])
    elif len(params) == 2 and params[0] == 'balance': # list balance 10
        print_balance(transactions, params[1])
    else:
        raise ValueError("incorrect usage of `list` command")


# remove command


def remove_transaction_ui(transactions, command_params):

    params = command_params.split(' ') if command_params else []

    if len(params) == 3:
        remove_transactions_from_start_to_end(transactions, params[0], params[2])
    elif len(params) == 1 and params[0] in ['in', 'out']:
        remove_transactions_of_type(transactions, params[0])
    elif len(params) == 1:
        remove_transactions_by_day(transactions, params[0])
    else:
        raise ValueError("incorrect usage of `remove` command")

    print ("• Transactions removed succesfully!")


# replace command


def replace_transaction_ui(transactions, command_params):

    params = command_params.split(' ') if command_params else []

    if len(params) != 5 or params[3] != "with":
        raise ValueError("incorrect usage of `replace` command")

    try:
        replace_transaction(transactions, day = params[0], money = params[4], _type = params[1], description = params[2])
        print(f"• Replaced the amount for the `{params[1]}` transaction having the `{params[2]}` description from day {params[0]} with `{params[4]} RON`")
    except ValueError as ve:
        print (f"• {ve}")


# main


def print_commands():

    print(">> commands (displays the following list)")

    print(">> add <money_value> <type> <description>")
    print(">> insert <day> <money_value> <type> <description>")

    print(">> remove <day>")
    print(">> remove <start day> to <end day>`")
    print(">> remove <type>`")
    print(">> replace <day> <type> <description> with <money_value>`")

    print(">> list")
    print(">> list <type>")
    print(">> list [ < | = | > ] <money_value>`")
    print(">> list balance <day>`")
   
    print(">> exit")
    

def main():
    
    transactions = init_transactions()

    print_commands()
    print ('\n')

    while True:     

        command = input("<< Enter a command: ")
        command_word, command_params = split_command(command)
        
        try:
            if command_word == "add":
                add_transaction_ui(transactions, command_params)
            elif command_word == "insert":
                insert_transaction_ui(transactions, command_params)
            elif command_word == "remove":
                remove_transaction_ui(transactions, command_params)
            elif command_word == "replace":
                replace_transaction_ui(transactions, command_params)
            elif command_word == "list":
                print_transactions(transactions, command_params)
            elif command_word == "commands":
                print_commands()
            elif command_word == "exit":
                return
            else:
                print("• This command does not exist")
        except ValueError as ve:
            print(f"• {ve}")
        except:
            print("Unexcepted error ooccured")

        print ('\n')


main()