import Foundation

struct Point: Hashable, CustomStringConvertible {
    var x: Int
    var y: Int

    var description: String {
        return "(\(x), \(y))"
    }
}

func loadData(from path: String) -> [Point] {
    let fileURL = URL(fileURLWithPath: path)

    do {
        let content = try String(contentsOf: fileURL, encoding: .utf8)
        let lines = content.split(separator: "\n")

        var points: [Point] = []
        for line in lines {
            let parts = line.split(separator: ",")
            if let xStr = parts.first, let yStr = parts.last, let x = Int(xStr), let y = Int(yStr) {
                let p = Point(x: x, y: y)
                points.append(p)
            }
        }

        return points

    } catch {
        print("ERROR: \(error.localizedDescription)")
        return []
    }
}

func findLargestRectangle(tileLocations points: [Point]) -> Int {
    var largestArea = 0
    for c1 in points {
        for c2 in points {
            if c1 == c2 {
                continue
            }

            let area = (abs(c1.x - c2.x) + 1) * (abs(c1.y - c2.y) + 1)
            if area > largestArea {
                largestArea = area
            }
        }
    }
    return largestArea
}

let points = loadData(from: "./test_input.txt")
let largestArea = findLargestRectangle(tileLocations: points)
print(largestArea)

