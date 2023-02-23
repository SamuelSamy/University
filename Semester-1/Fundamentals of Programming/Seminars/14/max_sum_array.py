#  Calculate the maximum subarray sum 

array = [-2, -5, 6, -2, -3, 1, 5, -6]

max_sum = current_sum = array[0]


for i in range(1, len(array)):
    current_sum = max(current_sum + array[i], array[i])
    max_sum = max(max_sum, current_sum)


print(f"The maximum sum is: {max_sum}")