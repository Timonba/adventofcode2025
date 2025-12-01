package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

const (
	dialSize     = 100
	startValue   = 50
	defaultInput = "input01.txt"
)

func main() {
	inputPath := defaultInput
	if len(os.Args) > 1 {
		inputPath = os.Args[1]
	}

	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "open %s: %v\n", inputPath, err)
		os.Exit(1)
	}
	defer file.Close()

	pos := startValue
	partOne := 0
	partTwo := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		dir := line[0]
		distance, err := strconv.Atoi(strings.TrimSpace(line[1:]))
		if err != nil {
			fmt.Fprintf(os.Stderr, "parse distance %q: %v\n", line, err)
			os.Exit(1)
		}

		partTwo += zeroHitsDuring(pos, distance, dir)

		move := distance % dialSize
		switch dir {
		case 'L':
			pos = (pos - move + dialSize) % dialSize
		case 'R':
			pos = (pos + move) % dialSize
		default:
			fmt.Fprintf(os.Stderr, "unknown direction %q\n", dir)
			os.Exit(1)
		}

		if pos == 0 {
			partOne++
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "read input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", partOne)
	fmt.Printf("Part 2: %d\n", partTwo)
}

// zeroHitsDuring counts how many clicks in a single rotation land on 0.
func zeroHitsDuring(pos, distance int, dir byte) int {
	if distance <= 0 {
		return 0
	}

	var stepsToZero int
	switch dir {
	case 'R':
		stepsToZero = (dialSize - pos) % dialSize
	case 'L':
		stepsToZero = pos % dialSize
	default:
		return 0
	}

	if stepsToZero == 0 {
		stepsToZero = dialSize
	}
	if distance < stepsToZero {
		return 0
	}
	return 1 + (distance-stepsToZero)/dialSize
}
