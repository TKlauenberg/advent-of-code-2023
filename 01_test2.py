from typing import List, Dict

digit_words = [{'Text': word, 'Value': i + 1} for i, word in enumerate(["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"])]

def get_calibration_number2(line: str) -> int:
    digits = [{'Value': int(c), 'Position': i} for i, c in enumerate(line) if c.isdigit()]

    words = []
    for word in digit_words:
        matching_words = [{'Value': word['Value'], 'Position': pos} for pos in [line.find(word['Text']), line.rfind(word['Text'])] if pos > -1]
        matching_words.sort(key=lambda word: word['Position'])
        words.extend(matching_words)

    combined = sorted(digits + words, key=lambda x: x['Position'])
    values = [x['Value'] for x in combined]
    result = str(values[0]) + str(values[-1]) if values else '0'
    print(result)
    return int(result)

# sample2_result = sum(get_calibration_number2(line) for line in sample2.split('\n'))
# print(f"Sample Part 2: {sample2_result}")

with open("data/1.txt") as f:
    sample2 = f.readlines()
# Assuming Files[1] is a list of lines
print(f"Part 2: {sum(get_calibration_number2(line) for line in sample2)}")