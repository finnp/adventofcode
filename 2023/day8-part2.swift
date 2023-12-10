import Foundation

func main() {
    signal(SIGINT, signalHandler)

    var lines = [String]()

    while let line = readLine() {
        lines.append(line)
    }

    let (instructions, mapping, backwardsMapping, startValues) = parseInput(lines)

    let allEndNodes = Array(backwardsMapping.keys).filter { Array($0)[2] == "Z" }.sorted{ $0 < $1 }

    let allStartNotes = Array(mapping.keys).filter { Array($0)[3] == "A" }.sorted { $0 < $1 }


    print(allStartNotes)
    print(allEndNodes)
    let endNodes = [allEndNodes[1]]
    print("Endnodes: \(endNodes)")
 
    let waysToEnds = endNodes.map { findWaysToValue($0, [], backwardsMapping) }.flatMap{ $0 }
    print (waysToEnds)
    print (waysToEnds.map { $0.count })
}

//  11A [(R, 11A)]

func findWaysToValue(_ value: String, _ path: [(String, String)], _ backwardsMapping: [String: [(String, String)]]) -> [[(String, String)]] {
    if value.hasSuffix("A") {
        print("Found a path to ", value, path.count)

        print(path.map { $0.0 }.joined())
        return [path]
    }

    guard let possibleOrigins = backwardsMapping[value] else {
        return [path]
    }

    var allWays: [[(String, String)]] = []

    // print(path.count)

    for step in possibleOrigins {
        let (_, origin ) = step 

        // if (origin.hasSuffix("A")) {
        //     allWays += [step] + path
        // }

        // print(path.map { $1 }.contains(origin))
        if (!path.map { $1 }.contains(origin) && path.map{$1}.first(where: {$0.hasSuffix("A")}) == nil) {
            // print(path.map {$1}.joined(separator:","), "\n")
            allWays += findWaysToValue(origin, [step] + path, backwardsMapping)
        }
    }

    // print(findWaysToValue(origin, backwardsMapping))

    // print(possibleOrigins)

    return allWays
}






//     var potentialPaths: [[(String, String)]] = []



//     for potentialPath in potentialPaths {
//         let origin = potentialPath.last
//         if (!potentialPath.map { $1 }.contains(origin)) {
//             print("loop")
//             for newPath in findWaysToValue(origin, backwardsMapping) {
//                 print(newPath)
//             }
//         }
//     }

//     print(potentialPaths)

//     return potentialPaths
// }


// -- LR

// -- 11A = (11B, XXX)
// -- 11B = (XXX, 11Z)
// -- 11Z = (11B, XXX)
// -- 22A = (22B, XXX)
// -- 22B = (22C, 22C)
// -- 22C = (22Z, 22Z)
// -- 22Z = (22B, 22B)
// -- XXX = (XXX, yyy)

// -- 11B -> (L, 11A)
// -- XXX -> (R, 11A)

// -- YYYY <- XXX <- XXX (stop)
// -- YYYY <- XXX <- 22B <- 22A (1 result) a values are never on the right, so we're done


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

func parseInput(_ lines: [String]) -> (instructions: String, mapping: [String: String], backwardsMapping: [String: [(String, String)]], startValues: [String]) {
    var mapping = [String: String]()
    var backwardsMapping = [String: [(String, String)]]()
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


                backwardsMapping[valueComponents[0], default: []].append(("L", key))
                backwardsMapping[valueComponents[1], default: []].append(("R", key))
            }

            if (key.hasSuffix("A")) {
                startValues.append(key)
            }
        }
    }

    return (instructions, mapping, backwardsMapping, startValues)
}

func signalHandler(_ signal: Int32) {
    print("\nTermination signal received. Exiting immediately...")
    exit(EXIT_SUCCESS) // Exit the program immediately
}