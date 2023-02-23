"""
  User interface module
"""

from datetime import datetime
from functions import *

def add_transaction_ui(transactions, command_params, operations, index):

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
        add_transaction(transactions, create_transaction(day, money, _type, description), operations, index)
        index += 1

        print("• Transaction added succsesfully")
    except ValueError as ve:
        print(f"• {ve}")


# insert command


def insert_transaction_ui(transactions, command_params, operations, index):

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
        add_transaction(transactions, create_transaction(day, money, _type, description), operations, index)
        index += 1

        print("• Transaction inserted succsesfully")
    except ValueError as ve:
        print(f"• {ve}")


# list command


def print_transaction(transaction, index):
    print(f"{index}. Day: {get_day(transaction)} | Money: {get_money(transaction)} | Type: {get_type(transaction)} | Description: {get_description(transaction)}")


def print_transactions_of_type(transactions, _type):
    
    index = 0

    transactions_to_print = generate_transactions_of_type(transactions, _type)

    for transaction in transactions_to_print:
        print_transaction(transaction, index)
        index += 1

    if len(transactions_to_print) == 0:
        print("• No transactions found!")


def print_transactions_based_on_money(transactions, sign, value):

    index = 0

    new_transactions = get_transactions_based_on_money(
        transactions, sign, value)

    for transaction in new_transactions:
        print_transaction(transaction, index)
        index += 1

    if len(new_transactions) == 0:
        print("• No transactions found!")


def print_balance(transactions, day):

    balance = compute_balance(transactions, day)

    print(f"The account’s balance at the end of day {day} is {balance} RON")


def print_transactions(transactions, command_params):

    params = command_params.split(' ') if command_params else []

    if len(params) == 0:  # list
        print_transactions_of_type(transactions, 'all')
    elif len(params) == 1 and params[0] in ['in', 'out']:  # list in
        print_transactions_of_type(transactions, params[0])
    elif len(params) == 2 and params[0] in ['>', '=', '<']:  # listlist > 100
        print_transactions_based_on_money(transactions, params[0], params[1])
    elif len(params) == 2 and params[0] == 'balance':  # list balance 10
        print_balance(transactions, params[1])
    else:
        raise ValueError("incorrect usage of `list` command")


# remove command


def remove_transaction_ui(transactions, command_params, operations, index):

    params = command_params.split(' ') if command_params else []

    if len(params) == 3:
        remove_transactions_from_start_to_end(transactions, params[0], params[2], operations, index)
    elif len(params) == 1 and params[0] in ['in', 'out']:
        remove_transactions_of_type(transactions, params[0], operations, index)
    elif len(params) == 1:
        remove_transactions_by_day(transactions, params[0], operations, index)
    else:
        raise ValueError("incorrect usage of `remove` command")

    index += 1
    print("• Transactions removed succesfully!")


# replace command


def replace_transaction_ui(transactions, command_params, operations, index):

    params = command_params.split(' ') if command_params else []

    if len(params) != 5 or params[3] != "with":
        raise ValueError("incorrect usage of `replace` command")

    try:
        replace_transaction(transactions, params, operations, index)
        index += 1
        print(f"• Replaced the amount for the `{params[1]}` transaction having the `{params[2]}` description from day {params[0]} with `{params[4]} RON`")
    except ValueError as ve:
        print(f"• {ve}")


# [New

def display_total_ui(transactions, command_params):
    
    params = command_params.split(' ') if command_params else []

    if len(params) != 1:
        raise ValueError("incorrect usage of `sum` command")

    try:
        total = compute_total(transactions, params[0])
        print (f"The total amount from {params[0]} transactions is {abs(total)}")
    except ValueError as ve:
        print(f"• {ve}")


def display_max_ui(transactions, command_params):
    
    params = command_params.split(' ') if command_params else []

    if len(params) != 2:
        raise ValueError("incorrect usage of `max` command")

    try:
        max_transaction = find_maxim(transactions, params)
        print(f"• The maximum {params[0]} transaction on day {params[1]} is:")
        print_transaction(max_transaction, 0)
    except ValueError as ve:
        print(f"• {ve}")



def filter_transactions_ui(transactions, command_params, operations, index):

    params = command_params.split(' ') if command_params else []

    if len(params) in [1, 2]:
        filter_transactions(transactions, params, operations, index)
        print("• Transactions filtered!")
    else:
        raise ValueError("incorrect usage of `filter` command")
        

def revert_last_operation_ui(transactions, operations, index):

    if len(operations) == 0:
        print("• No operations left to undo!")
    else:
        try:
            revert_last_operation(transactions, operations, index)
            print("• Last operation reversed")
        except ValueError as ve:
            print(f"• {ve}")

# New]


"""
Main 
"""

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

    print(">> sum <type>")
    print(">> max <type> <day>")

    print(">> filter <type>")
    print(">> filter <type> <value>")

    print(">> undo")

    print(">> exit")


def print_operations(operations):
    print ('\n')
    for operation in operations:
        print(operation)


def main():
    
    run_all_tests()

    transactions = init_transactions()
    operations = []
    index = 0
    
    print_commands()

    while True:     

        print ('\n')
        command = input("<< Enter a command: ")

        command_word, command_params = split_command(command)
        

        try:

            if command_word == "add":
                add_transaction_ui(transactions, command_params, operations, index)
                index += 1
            elif command_word == "insert":
                insert_transaction_ui(transactions, command_params, operations, index)
                index += 1
            elif command_word == "remove":
                remove_transaction_ui(transactions, command_params, operations, index)
                index += 1
            elif command_word == "replace":
                replace_transaction_ui(transactions, command_params, operations, index)
                index += 1
            elif command_word == "list":
                print_transactions(transactions, command_params)
            elif command_word == "commands":
                print_commands()
            elif command_word == "sum":
                display_total_ui(transactions, command_params)
            elif command_word == "max":
                display_max_ui(transactions, command_params)
            elif command_word == "undo":
                revert_last_operation_ui(transactions, operations, index)
            elif command_word == "filter":
                filter_transactions_ui(transactions, command_params, operations, index)
                index += 1
            elif command_word == "exit":
                return
            else:
                print("• This command does not exist")

        except ValueError as ve:
            print(f"• {ve}")

        except:
            print("Unexpected error occured. Please try again")

            