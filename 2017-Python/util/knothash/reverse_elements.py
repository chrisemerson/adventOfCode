def reverse_elements(number_list, current_position, length):
    if length <= 1:
        return number_list

    elif current_position + length <= len(number_list):
        sub_list = number_list[current_position:current_position + length]

        reversed_sublist = list(reversed(sub_list))

        return number_list[0:current_position] + reversed_sublist + number_list[current_position + length:]

    else:
        sub_list_start = number_list[current_position:]
        sub_list_end = number_list[0:length - len(sub_list_start)]

        sub_list = sub_list_start + sub_list_end

        reversed_sublist = list(reversed(sub_list))

        return\
            reversed_sublist[len(sub_list_start):]\
            + number_list[len(sub_list_end):(len(sub_list_end) + len(number_list) - length)]\
            + reversed_sublist[:len(sub_list_start)]
