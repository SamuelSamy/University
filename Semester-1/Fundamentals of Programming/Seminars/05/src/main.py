import console
import testing

def main():

    testing.run_tests()
    
    circles_list = []

    exit = False

    while not exit:
        console.print_options()
        exit = console.menu(circles_list)


main()