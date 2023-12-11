var total = 0

while (true) {
    val line = readLine() ?: break
    var row = line.split(" ").map { it.toInt() }.toMutableList()
    total += solveRow(row)
}

println(total)

fun solveRow(readings: MutableList<Int>): Int {
    val pyramid = mutableListOf(readings)

    var index = 0
    while(!pyramid[index].all { it == 0 }) {
        var nextRow = mutableListOf<Int>()
        for (position in 0 until (pyramid[index].size - 1)) {
            val left = pyramid[index][position]
            val right = pyramid[index][position + 1]
        
            nextRow.add(right - left)

        }
        pyramid.add(nextRow)
        index++
    }

    var value = 0

    while(index >= 0) {
        value = pyramid[index].first() - value
        index--
    }

    return value
}
