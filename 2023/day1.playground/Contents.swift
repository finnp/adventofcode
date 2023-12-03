import Foundation

let input = """
5eightfourpzv8bjbbeightwopp
"""

var sum = 0

let letters: [(String, Int)] = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9), ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9)]

for substrLine in input.split(separator: "\n") {
    var firstDigit = 0
    var lastDigit = 0
    
    let line = String(substrLine)
    
    var firstDigitPosition = 100000000
    var lastDigitPosition = -1

    for letter in letters {
        if let first = line.range(of: letter.0) {
            let firstPosition = line.distance(from: line.startIndex, to: first.lowerBound)
            if (firstPosition < firstDigitPosition) {
                firstDigit = letter.1
                firstDigitPosition = firstPosition
            }

        }
        if let last = line.range(of: letter.0, options: .backwards) {
            let lastPosition = line.distance(from: line.startIndex, to: last.lowerBound)
            if (lastPosition > lastDigitPosition) {
                lastDigit = letter.1
                lastDigitPosition = lastPosition
            }
        }
    }

    let fullNumber = firstDigit * 10 + lastDigit
    sum += fullNumber
}

sum
