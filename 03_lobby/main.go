package main

import (
	"bufio"
	"flag"
	"fmt"
	"math/big"
	"os"
	"strings"
)

const (
	defaultInput = "input_03.txt"
	partOnePick  = 2
	partTwoPick  = 12
)

func main() {
	inputPath := flag.String("input", defaultInput, "path to the puzzle input")
	debug := flag.Bool("debug", false, "print per-bank calculations")
	flag.Parse()
	if flag.NArg() > 0 {
		*inputPath = flag.Arg(0)
	}

	file, err := os.Open(*inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "open %s: %v\n", *inputPath, err)
		os.Exit(1)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	total := 0
	partTwoTotal := big.NewInt(0)
	lineNum := 0

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		lineNum++
		if line == "" {
			continue
		}

		digits, err := parseDigits(line)
		if err != nil {
			fmt.Fprintf(os.Stderr, "line %d: %v\n", lineNum, err)
			os.Exit(1)
		}

		partOneChoice, err := bestSelection(digits, partOnePick)
		if err != nil {
			fmt.Fprintf(os.Stderr, "line %d (part 1): %v\n", lineNum, err)
			os.Exit(1)
		}
		bankValue := digitsToInt(partOneChoice.digits)
		if err != nil {
			fmt.Fprintf(os.Stderr, "line %d: %v\n", lineNum, err)
			os.Exit(1)
		}
		total += bankValue

		partTwoChoice, err := bestSelection(digits, partTwoPick)
		if err != nil {
			fmt.Fprintf(os.Stderr, "line %d (part 2): %v\n", lineNum, err)
			os.Exit(1)
		}
		partTwoString := digitsToString(partTwoChoice.digits)
		value := new(big.Int)
		if _, ok := value.SetString(partTwoString, 10); !ok {
			fmt.Fprintf(os.Stderr, "line %d: failed to parse %q as big integer\n", lineNum, partTwoString)
			os.Exit(1)
		}
		if *debug {
			fmt.Printf("line %3d: %s\n", lineNum, line)
			fmt.Printf("  pick %2d -> %s (indices %v)\n", partOnePick, digitsToString(partOneChoice.digits), partOneChoice.indices)
			fmt.Printf("  pick %2d -> %s (indices %v)\n", partTwoPick, partTwoString, partTwoChoice.indices)
		}
		partTwoTotal.Add(partTwoTotal, value)
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "read input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1 (pick %d): %d\n", partOnePick, total)
	fmt.Printf("Part 2 (pick %d): %s\n", partTwoPick, partTwoTotal.String())
}

type selection struct {
	digits  []int
	indices []int
}

// bestSelection returns the lexicographically largest subsequence of the given digit
// slice that keeps the original order while picking exactly pick digits.
func bestSelection(digits []int, pick int) (selection, error) {
	if pick <= 0 {
		return selection{}, fmt.Errorf("invalid pick count %d", pick)
	}
	if len(digits) < pick {
		return selection{}, fmt.Errorf("bank too short: need %d digits, have %d", pick, len(digits))
	}

	stackDigits := make([]int, 0, pick)
	stackIndices := make([]int, 0, pick)
	for i, digit := range digits {
		digitsLeft := len(digits) - i
		for len(stackDigits) > 0 && digit > stackDigits[len(stackDigits)-1] &&
			len(stackDigits)-1+digitsLeft >= pick {
			stackDigits = stackDigits[:len(stackDigits)-1]
			stackIndices = stackIndices[:len(stackIndices)-1]
		}
		if len(stackDigits) < pick {
			stackDigits = append(stackDigits, digit)
			stackIndices = append(stackIndices, i)
		}
	}

	if len(stackDigits) != pick {
		return selection{}, fmt.Errorf("failed to choose %d digits, ended with %d", pick, len(stackDigits))
	}

	// Copy to ensure later modifications don't affect stored choices.
	finalDigits := append([]int(nil), stackDigits...)
	finalIndices := append([]int(nil), stackIndices...)
	return selection{digits: finalDigits, indices: finalIndices}, nil
}

func parseDigits(bank string) ([]int, error) {
	if len(bank) == 0 {
		return nil, fmt.Errorf("empty bank")
	}
	digits := make([]int, len(bank))
	for i := range bank {
		if bank[i] < '0' || bank[i] > '9' {
			return nil, fmt.Errorf("unsupported digit %q", bank[i])
		}
		digits[i] = int(bank[i] - '0')
	}
	return digits, nil
}

func digitsToInt(ds []int) int {
	value := 0
	for _, d := range ds {
		value = value*10 + d
	}
	return value
}

func digitsToString(ds []int) string {
	var b strings.Builder
	b.Grow(len(ds))
	for _, d := range ds {
		b.WriteByte(byte('0' + d))
	}
	return b.String()
}
