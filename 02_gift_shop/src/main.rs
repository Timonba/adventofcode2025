use std::fs;

fn main() {
    // Try common input locations: project root and `src/` (when run via `cargo run`).
    let candidates = ["input02.txt", "src/input02.txt"];
    let mut input = String::new();
    for p in &candidates {
        if let Ok(s) = fs::read_to_string(p) {
            input = s;
            break;
        }
    }
    if input.is_empty() {
        eprintln!("Warning: could not read any input file - using empty input");
    }

    // Parse ranges separated by commas, each as `a-b`.
    let mut total: u128 = 0;

    for part in input.split(|c| c == ',' || c == '\n' || c == '\r') {
        let s = part.trim();
        if s.is_empty() { continue; }
        let mut pieces = s.splitn(2, '-');
        let a = pieces.next().and_then(|x| x.trim().parse::<u128>().ok());
        let b = pieces.next().and_then(|x| x.trim().parse::<u128>().ok());
        let (a, b) = match (a, b) {
            (Some(a), Some(b)) => (a, b),
            _ => continue,
        };

        // Determine max digits in b to limit k
        let max_digits = b.to_string().len();
        let max_k = max_digits / 2; // k is half-length of repeated block

        for k in 1..=max_k {
            let ten_k = 10u128.pow(k as u32);
            let mul = ten_k + 1u128; // n = x * (10^k + 1)

            let x_min_digits = 10u128.pow((k - 1) as u32);
            let x_max_digits = ten_k - 1u128;

            // ceil division for lower bound
            let x_low = ((a + mul - 1) / mul).max(x_min_digits);
            let x_high = (b / mul).min(x_max_digits);
            if x_low > x_high { continue; }

            let count = x_high - x_low + 1;
            let sum_x = (x_low + x_high) * count / 2; // arithmetic series sum
            total += mul * sum_x;
        }
    }

    println!("Part 1: {}", total);
    // Part 2: numbers that are some block `x` repeated r times, with r >= 2.
    // We'll generate all candidates up to the maximum digits seen in the input ranges,
    // deduplicate them, and sum those that lie inside any of the ranges.

    // First, collect the parsed ranges into a Vec for membership tests.
    let mut ranges: Vec<(u128, u128)> = Vec::new();
    for part in input.split(|c| c == ',' || c == '\n' || c == '\r') {
        let s = part.trim();
        if s.is_empty() { continue; }
        let mut pieces = s.splitn(2, '-');
        let a = pieces.next().and_then(|x| x.trim().parse::<u128>().ok());
        let b = pieces.next().and_then(|x| x.trim().parse::<u128>().ok());
        if let (Some(a), Some(b)) = (a, b) {
            ranges.push((a, b));
        }
    }

    let mut part2_sum: u128 = 0;
    use std::collections::BTreeSet;
    let mut seen = BTreeSet::new();

    // Determine the maximum upper bound to limit generation
    let max_b = ranges.iter().map(|(_, b)| *b).max().unwrap_or(0);
    let max_digits = max_b.to_string().len();

    // For block length k (digits in x) from 1..=max_digits/2 maybe larger because r can be >=2
    for k in 1..=max_digits { // allow larger k but will stop when repeated number grows too big
        let ten_k = 10u128.pow(k as u32);
        let x_min = 10u128.pow((k - 1) as u32);
        let x_max = ten_k - 1u128;

        // Repetition count r >= 2. Stop when number length exceeds max_digits
        for r in 2..= (max_digits / k + 2) {
            // Build multiplier: repeated concatenation is like x * (10^{k*(r-1)} + 10^{k*(r-2)} + ... + 1)
            let mut mul: u128 = 0;
            let mut shift = 0u32;
            for _ in 0..r {
                mul += 10u128.pow(shift);
                shift += k as u32;
            }

            // If smallest possible repeated number exceeds max_b, we can break inner loop for larger r
            let min_n = x_min.saturating_mul(mul);
            if min_n > max_b { break; }

            for x in x_min..=x_max {
                let n = x * mul;
                if n > max_b { break; }
                // check ranges membership
                let mut in_any = false;
                for (ra, rb) in &ranges {
                    if *ra <= n && n <= *rb { in_any = true; break; }
                }
                if in_any {
                    if seen.insert(n) {
                        part2_sum += n;
                    }
                }
            }
        }
    }

    println!("Part 2: {}", part2_sum);
}
