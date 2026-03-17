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

        return lines.compactMap { line in
            let parts = line.split(separator: ",")
            guard let xStr = parts.first,
                let yStr = parts.last,
                let x = Int(xStr),
                let y = Int(yStr)
            else { return nil }
            return Point(x: x, y: y)
        }

    } catch {
        print("ERROR: \(error.localizedDescription)")
        return []
    }
}

func findLargestRectangle(tileLocations points: [Point]) -> Int {
    var largestArea = 0
    for i in 0..<points.count {
        for j in (i + 1)..<points.count {

            let c1 = points[i]
            let c2 = points[j]

            let area = (abs(c1.x - c2.x) + 1) * (abs(c1.y - c2.y) + 1)
            if area > largestArea {
                largestArea = area
            }
        }
    }
    return largestArea
}

let points = loadData(from: "./input.txt")
let largestArea = findLargestRectangle(tileLocations: points)
print(largestArea)
