import Foundation

struct Machine: Hashable, CustomStringConvertible {
    var targetState: [Int]
    var state: [Int]
    var buttons: [[Int]]
    var joltage: [Int]

    var description: String {
        return "(\((state)):   [\((targetState))] \t \(buttons) \t {\(joltage)})"
    }
}

func loadData(from path: String) -> [Machine] {
    let fileURL = URL(fileURLWithPath: path)

    do {
        let content = try String(contentsOf: fileURL, encoding: .utf8)
        let lines = content.split(separator: "\n")

        var machines: [Machine] = []
        for line in lines {

            var targetState: [Int] = []
            var buttons: [[Int]] = []
            var joltage: [Int] = []

            if let lightsMatches = line.firstMatch(of: #/\[(?<lights>[.#]+)\]/#) {
                targetState = lightsMatches.lights.map { $0 == "#" ? 1 : 0 }
            }

            let buttonMatches = line.matches(of: #/\((?<button>.+?)\)/#)
            buttons = buttonMatches.map { match in
                String(match.button).split(separator: ",").compactMap {
                    Int($0.trimmingCharacters(in: .whitespaces))
                }
            }

            if let joltageMatch = line.firstMatch(of: #/\{(?<joltages>.+?)\}/#) {
                joltage = joltageMatch.joltages.split(separator: ",").compactMap {
                    Int($0.trimmingCharacters(in: .whitespaces))
                }
            }

            let state = Array(repeating: 0, count: targetState.count)
            machines.append(
                Machine(targetState: targetState, state: state, buttons: buttons, joltage: joltage))
        }

        return machines

    } catch {
        print("ERROR: \(error.localizedDescription)")
        return []
    }
}

func updateState(currentState: [Int], button: [Int]) -> [Int] {
    var state = currentState

    for idx in button {
        state[idx] ^= 1
    }

    return state
}

func expandButtons(machine: Machine) -> [[Int]] {
    let width = machine.state.count
    var buttons: [[Int]] = []

    for button in machine.buttons {
        var b = Array(repeating: 0, count: width)
        for idx in button {
            b[idx] = 1
        }
        buttons.append(b)
    }

    return buttons
}

let machines = loadData(from: "./test_input.txt")
print(machines[0])
print(machines[1])
print(machines[2])
