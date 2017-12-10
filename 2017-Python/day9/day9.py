import re

stream = ''


def remove_cancelled_chars(string):
    currentpos = 0
    outputstring = ''

    while currentpos < len(string):
        if string[currentpos] == '!':
            currentpos += 1
        else:
            outputstring += string[currentpos]

        currentpos += 1

    return outputstring


def remove_garbage(string, removedchars=0):
    matches = re.search('^(.*?)<([^>]*?)>(.*?)$', string)

    if matches is None:
        print("Total removed chars in garbage: " + str(removedchars))

        return string

    else:
        start = matches.group(1)
        end = matches.group(3)
        chars_in_garbage = len(matches.group(2))

        if len(start) > 0 and start[-1:] == ',':
            return remove_garbage(start[:-1] + end, removedchars + chars_in_garbage)
        elif len(end) > 0 and end[0] == ',':
            return remove_garbage(start + end[1:], removedchars + chars_in_garbage)
        else:
            return remove_garbage(start + end, removedchars + chars_in_garbage)


def get_sub_groups(group):
    sub_groups = []

    if len(group) == 0:
        return sub_groups

    matches = re.search('^{(.*)}$', group)
    group_contents = matches.group(1)

    while len(group_contents) > 0:
        if group_contents[0] == '{':
            counter = 1
            pos = 1
            sub_group = '{'

            while counter != 0:
                if group_contents[pos] == '{':
                    counter += 1
                elif group_contents[pos] == '}':
                    counter -= 1

                sub_group += group_contents[pos]
                pos += 1

            sub_groups.append(sub_group)
            group_contents = group_contents[(pos + 1):]

        else:
            print('Unexpected character starting group: ' + group_contents[0])

    return sub_groups


def score_group(group, level=1):
    score = level

    for sub_group in get_sub_groups(group):
        score += score_group(sub_group, level + 1)

    return score


with open('input.txt', 'r') as fp:
    stream += fp.readline()

print("Input: " + stream)
stream = remove_cancelled_chars(stream)
print("Remove cancelled chars: " + stream)
stream = remove_garbage(stream)
print("Remove garbage: " + stream)
score = score_group(stream)
print("Group score: " + str(score))
