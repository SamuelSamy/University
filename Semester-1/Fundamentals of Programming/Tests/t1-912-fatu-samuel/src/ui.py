import time
from functions import build_number, generate_number, game_ended, is_valid_guess, get_codes, get_runners, get_digits



def main():

    secret_number = generate_number()
    
    start_timestamp = time.time()
    game_max_time = 60  # maxmimum game time in seconds

    while not game_ended(start_timestamp, game_max_time): 

        try:
            number =  read_number()

            if game_ended(start_timestamp, game_max_time):
                print(f"Game Over!\n{game_max_time} seconds passed since the game started")
                return

            if number == "8086":
                print(f"The random secret number is {build_number(secret_number)}")
                
            else:
                number = get_digits(number)

                if is_valid_guess(number):
                    
                    codes = get_codes(secret_number, number)
                    runners = get_runners(secret_number, number)

                    if codes == 4:
                        print("Game Over!\nPlayer Wins")
                        return
                    else:
                        print(f"This number has {codes} codes and {runners} runners")

                else:
                    print("Game Over!\nThe Computer Wins")
                    return
                
        except ValueError as ve: 
            print(f"{ve}")

    print(f"Game Over!\n{game_max_time} seconds passed and the game ended")


def read_number():

    number = input(">> Enter a number: ")

    if not number.isnumeric():
        raise ValueError("The number must be an integer")

    if len(number) != 4:
        raise ValueError("The number must have 4 digits")

    return number