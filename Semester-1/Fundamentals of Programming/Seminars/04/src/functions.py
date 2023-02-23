
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

    command_word = aux[0].strip().lower() if aux else command
    command_params = aux[1].strip().lower() if aux and len(aux) == 2 else None

    return command_word, command_params


def test_split_command():
    
    assert split_command('generate 5') == ('generate', '5')
    assert split_command('display') == ('display', None)
    assert split_command('exit') == ('exit', None)


def split_params(command_params):
    """
    Splits the `command_params` string into a list of parameters

    Args:
        command_params (string): A string containing the parameters

    Returns:
        list: A list of strings that represent the splitted parameters / An empty list if `command_params` is empty
    """

    if not command_params:
        return []

    params = []

    command_params = command_params.strip()

    for param in command_params.split(' '):
        params.append(param.strip())

    return params

def test_split_params():
    assert split_params('5') == ['5']
    assert split_params('  10  ') == ['10']
    assert split_params('0 0 40 40') == ['0', '0', '40', '40']
    assert (split_params('0 0 40 40 40') == ['0', '0', '40', '40']) == False
