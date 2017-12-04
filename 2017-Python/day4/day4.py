validPassPhrases = 0
validPassPhrasesPt2 = 0

with open('input.txt', 'r') as fp:
    for line in fp:
        words = list(line.strip().split(" "))
        uniqueWords = set(line.strip().split(" "))
        wordsInAlphabeticalOrder = set()

        if len(words) == len(uniqueWords):
            validPassPhrases += 1

        for word in words:
            wordsInAlphabeticalOrder.add(''.join(sorted(word)))

        if len(words) == len(wordsInAlphabeticalOrder):
            validPassPhrasesPt2 += 1

print(validPassPhrases)
print(validPassPhrasesPt2)
