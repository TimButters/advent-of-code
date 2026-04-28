import Foundation

struct Machine: Hashable, CustomStringConvertible {
    var targetState: UInt64
    var state: UInt64 = 0
    var stateWidth: Int
    var buttons: [UInt64]
    var joltage: [Int]

    var description: String {
        // Helper to format bits with leading zeros
        func formatBits(_ value: UInt64) -> String {
            let binary = String(value, radix: 2)
            let padding = String(repeating: "0", count: max(0, stateWidth - binary.count))
            return padding + binary
        }

        let buttonList = buttons.map { formatBits($0) }.joined(separator: ", ")

        return """
            --- Machine ---
            State:       [\(formatBits(state))]
            Target:      [\(formatBits(targetState))]
            Width:       \(stateWidth)
            Buttons:     \(buttonList)
            Joltage:     \(joltage)
            ----------------
            """
    }
}

func loadData(from path: String) -> [Machine] {
    let fileURL = URL(fileURLWithPath: path)

    do {
        let content = try String(contentsOf: fileURL, encoding: .utf8)
        let lines = content.split(separator: "\n")

        var machines: [Machine] = []
        for line in lines {

            var targetState: UInt64 = 0
            var stateWidth: Int = 0
            var buttons: [UInt64] = []
            var joltage: [Int] = []

            if let lightsMatches = line.firstMatch(of: #/\[(?<lights>[.#]+)\]/#) {
                let lightsString = lightsMatches.lights
                stateWidth = lightsString.count

                for (idx, char) in lightsString.enumerated() {
                    if char == "#" {
                        targetState |= (UInt64(1) << idx)
                    }
                }
            }

            let buttonMatches = line.matches(of: #/\((?<button>.+?)\)/#)
            buttons = buttonMatches.map { match in
                var b: UInt64 = 0
                let digits = String(match.button).split(separator: ",")

                for digit in digits {
                    if let bitIndex = Int(digit.trimmingCharacters(in: .whitespaces)) {
                        b |= (UInt64(1) << bitIndex)
                    }
                }
                return b
            }

            if let joltageMatch = line.firstMatch(of: #/\{(?<joltages>.+?)\}/#) {
                joltage = joltageMatch.joltages.split(separator: ",").compactMap {
                    Int($0.trimmingCharacters(in: .whitespaces))
                }
            }

            machines.append(
                Machine(
                    targetState: targetState, stateWidth: stateWidth, buttons: buttons,
                    joltage: joltage))
        }

        return machines

    } catch {
        print("ERROR: \(error.localizedDescription)")
        return []
    }
}

let machines = loadData(from: "./test_input.txt")
print(machines[0])
print(machines[1])
print(machines[2])
