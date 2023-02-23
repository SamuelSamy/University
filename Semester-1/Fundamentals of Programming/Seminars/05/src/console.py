import functions
import circle
import rectangle

def print_options():
    print ('\n')
    print(">> generate <n>")
    print(">> delete <x> <y> <width> <height>")
    print(">> display")
    print(">> exit")


def menu(circles_list):

    command = input("<< Enter a command: ")
    print('\n')
    
    command_word, command_params = functions.split_command(command)

    try:
        if command_word == "generate":
            generate_circles_ui(circles_list, command_params)
        elif command_word == "delete":
            delete_enclosed_circles_ui(circles_list, command_params)
        elif command_word == "display":
            display_circles(circles_list)
        elif command_word == "exit":
            return True
        else:
            print("• This command does not exist")
    except ValueError as ve:
        print(ve)

    return False


def generate_circles_ui(circles_list, command_params):

    params = functions.split_params(command_params)

    if len(params) != 1:
        raise ValueError("• Incorrent usage of `generate` command")

    try:
        n = int(params[0])
        assert (n >= 0) == True
    except:
        print ("• `n` must be a positive integer")
        return
    
    circle.generate_circles(circles_list, n)

    print ("• Circles generated succesfully!")


def display_circles(circles_list):
    
    sorted_circles = circle.sort_circles(circles_list)

    if sorted_circles:
        index = 0

        for circ in sorted_circles:
            print(f"{index}. center at {circle.get_center(circ)} radius of {circle.get_radius(circ)}")
            index += 1
    else:
        print("• There are no circles")


def delete_enclosed_circles_ui(circles_list, command_params):

    params = functions.split_params(command_params)

    if len(params) != 4:
        raise ValueError("• Incorrent usage of `delete` command")

    try:
        x = int(params[0])
        y = int(params[1])
        width = int(params[2])
        height = int(params[3])
    except ValueError as ve:
        print("• The paramaters must be numbers")
        return


    try:
        rect = rectangle.create_rectangle(x, y, width, height)
    except ValueError as ve:
        print (ve)
        return
    

    deleted_circles_count = circle.delete_enclosed_circles(circles_list, rect)

    print(f"• {deleted_circles_count} circles deleted")
