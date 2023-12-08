import Foundation

func main() {
    signal(SIGINT, signalHandler)

    var lines = [String]()

    while let line = readLine() {
        lines.append(line)
    }

    let (instructions, mapping, startValues) = parseInput(lines)

    print(instructions)
    print(startValues)

    let result = count(instructions, mapping, startValues)
    print ("Bye")
    print (result)
}

func count(_ instructions: String, _ mapping: [String: String], _ startValues: [String]) -> Int {
    var values = startValues
    var i = 0;

    while(true) {
        for instruction in instructions {
            i += 1
            // print(i)
            values = values.map { value in mapping[String(instruction) + value]! }

            if doAllEndWithZ(values) {
                return i
            }
        }
    }
}

main()

func doAllEndWithZ(_ values: [String]) -> Bool {
    for value in values {
        if (!value.hasSuffix("Z")) {
            return false
        }
    }
    return true
}

func parseInput(_ lines: [String]) -> (instructions: String, mapping: [String: String], startValues: [String]) {
    var mapping = [String: String]()
    var startValues = [String]()
    
    let instructions = lines.first ?? ""

    for line in lines.dropFirst() {
        let components = line.split(separator: "=").map { $0.trimmingCharacters(in: .whitespaces) }
        if components.count == 2 {
            let key = components[0]
            let valueComponents = components[1].trimmingCharacters(in: CharacterSet(charactersIn: "()"))
                                               .split(separator: ",")
                                               .map { $0.trimmingCharacters(in: .whitespaces) }

            if valueComponents.count == 2 {
                mapping["L" + key] = String(valueComponents[0])
                mapping["R" + key] = String(valueComponents[1])
            }

            if (key.hasSuffix("A")) {
                startValues.append(key)
            }
        }
    }

    return (instructions, mapping, startValues)
}

func signalHandler(_ signal: Int32) {
    print("\nTermination signal received. Exiting immediately...")
    exit(EXIT_SUCCESS) // Exit the program immediately
}