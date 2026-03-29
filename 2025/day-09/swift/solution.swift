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

func buildPerimeter(tileLocations pointsIn: [Point]) -> Set<Point> {
    var perimeter = Set<Point>(pointsIn)
    let points = pointsIn + [pointsIn[0]]

    var i = 0
    for j in (1..<points.count) {
        let p1 = points[i]
        let p2 = points[j]

        if p1.x == p2.x {
            let increment = p1.y < p2.y ? 1 : -1

            for k in stride(from: p1.y + increment, to: p2.y, by: increment) {
                perimeter.insert(Point(x: p1.x, y: k))
            }
        } else {
            let increment = p1.x < p2.x ? 1 : -1

            for k in stride(from: p1.x + increment, to: p2.x, by: increment) {
                perimeter.insert(Point(x: k, y: p1.y))
            }
        }

        i += 1
    }

    return perimeter
}

func isPointContained(testPoint p: Point, perimeter: Set<Point>, perimeterLookup: [Int: [Point]]) -> Bool {

    if perimeter.contains(p) { return true }

    guard let rowPoints = perimeterLookup[p.y] else { return false }

    let potentialCrossings = rowPoints.filter { $0.x > p.x }

    var crossings = 0
    for pt in potentialCrossings {
        if perimeter.contains(Point(x: pt.x, y: pt.y + 1)) {
            crossings += 1
        }
    }

    return crossings % 2 != 0
}

func isPerimeterContained(innerPerimeter: Set<Point>, outerPerimeter: Set<Point>, perimeterLookup: [Int: [Point]]) -> Bool {
    for p in innerPerimeter {
        if !isPointContained(testPoint: p, perimeter: outerPerimeter, perimeterLookup: perimeterLookup) {
            return false
        }
    }

    let minX = innerPerimeter.map { $0.x }.min()!
    let maxX = innerPerimeter.map { $0.x }.max()!
    let minY = innerPerimeter.map { $0.y }.min()!
    let maxY = innerPerimeter.map { $0.y }.max()!

    for y in (minY + 1)..<maxY {
        if let rowPoints = perimeterLookup[y] {
            if rowPoints.contains(where: { $0.x > minX && $0.x < maxX }) {
                return false
            }
        }
    }

    return true
}

func findLargestRectangleAllInner(tileLocations points: [Point]) -> Int {
    let perimeter = buildPerimeter(tileLocations: points)
    let perimeterByY = Dictionary(grouping: perimeter) { $0.y }

    var largestArea = 0
    for i in 0..<points.count {
        for j in (i + 1)..<points.count {

            let c1 = points[i]
            let c2 = points[j]

            let area = (abs(c1.x - c2.x) + 1) * (abs(c1.y - c2.y) + 1)
            if area > largestArea {
                let otherCorner1 = Point(x: c1.x, y: c2.y)
                let otherCorner2 = Point(x: c2.x, y: c1.y)
                let innerPerimeter = Set([c1, otherCorner1, c2, otherCorner2])

                // Is the perimeter of this rectangle contained by the shape perimiter?
                //let innerPerimeter = buildPerimeter(tileLocations: [
                //    c1, otherCorner1, c2, otherCorner2,
                //])
                if isPerimeterContained(
                    innerPerimeter: innerPerimeter, outerPerimeter: perimeter,
                    perimeterLookup: perimeterByY)
                {
                    largestArea = area
                }
            }

        }
    }

    return largestArea
}

let points = loadData(from: "./input.txt")
let largestArea = findLargestRectangle(tileLocations: points)
print(largestArea)

let largestContainedArea = findLargestRectangleAllInner(tileLocations: points)
print(largestContainedArea)
