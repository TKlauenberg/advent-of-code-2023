import re

sample2Example = """
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""

with open("data/1.txt") as f:
    sample2 = f.readlines()

digit_words = [
    {"Text": word, "Value": i + 1}
    for i, word in enumerate(["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"])
]

def get_calibration_number2(line):
    digits = [
        {"Value": int(c), "Position": i}
        for i, c in enumerate(line) if c.isdigit()
    ]

    words = []
    for word in digit_words:
        positions = [m.start() for m in re.finditer(word["Text"], line)]
        words.extend([{"Value": word["Value"], "Position": p} for p in positions if p > -1])

    combined = sorted(digits + words, key=lambda x: x["Position"])
    values = [x["Value"] for x in combined]

    return int(str(values[0]) + str(values[-1]))


sample2Result = sum(get_calibration_number2(line) for line in sample2 if line)
print(f"Sample 2: {sample2Result}")