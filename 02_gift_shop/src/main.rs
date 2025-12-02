use std::fs;
use std::time::Instant;

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

    // Parse ranges into Vec
    let mut ranges: Vec<(u128,u128)> = Vec::new();
    for part in input.split(|c| c == ',' || c == '\n' || c == '\r') {
        let s = part.trim();
        if s.is_empty() { continue; }
        let mut pieces = s.splitn(2, '-');
        if let (Some(a), Some(b)) = (pieces.next(), pieces.next()) {
            if let (Ok(a), Ok(b)) = (a.trim().parse::<u128>(), b.trim().parse::<u128>()) {
                ranges.push((a,b));
            }
        }
    }

    use std::collections::BTreeSet;

    // utils
    fn is_double(s: &str) -> bool {
        let l = s.len();
        if l % 2 != 0 { return false; }
        let k = l / 2;
        &s[..k] == &s[k..]
    }

    fn is_repeated(s: &str) -> bool {
        let l = s.len();
        if l < 2 { return false; }
        for r in 2..=l {
            if l % r != 0 { continue; }
            let k = l / r;
            let p = &s[..k];
            let mut ok = true;
            for i in 0..r {
                if &s[i*k..(i+1)*k] != p { ok = false; break; }
            }
            if ok { return true; }
        }
        false
    }

    // Optimized Part1
    // Math explanation:
    // If an ID is exactly two copies of a k-digit block x, then the ID n is:
    //   n = x * 10^k + x = x * (10^k + 1)
    // Let m = 10^k + 1. For a given range [a,b] we want all integers x such that
    //   a <= m * x <= b  =>  ceil(a/m) <= x <= floor(b/m)
    // Also x must have exactly k digits (no leading zeroes): 10^{k-1} <= x <= 10^k - 1.
    // So x_low = max(ceil(a/m), 10^{k-1}), x_high = min(floor(b/m), 10^k-1).
    // Sum of all n = m * sum_{x=x_low..x_high} x = m * (x_low + x_high) * count / 2.
    // Examples:
    // - k=1, x=5 -> m=10^1+1=11 => n=5*11=55
    // - k=2, x=12 -> m=10^2+1=101 => n=12*101=1212
    let t0 = Instant::now();
    let mut opt1: u128 = 0;
    for (a,b) in &ranges {
        let max_digits = b.to_string().len();
        let max_k = max_digits / 2;
        for k in 1..=max_k {
            let ten_k = 10u128.pow(k as u32);
            let mul = ten_k + 1u128;
            let x_min = 10u128.pow((k-1) as u32);
            let x_max = ten_k - 1u128;
            let x_low = ((a + mul - 1) / mul).max(x_min);
            let x_high = (b / mul).min(x_max);
            if x_low > x_high { continue; }
            let cnt = x_high - x_low + 1;
            let sum_x = (x_low + x_high) * cnt / 2;
            opt1 += mul * sum_x;
        }
    }
    let t_opt1 = t0.elapsed();

    // Brute-force Part1
    let t0 = Instant::now();
    let mut bf1: u128 = 0;
    for (a,b) in &ranges {
        for n in *a..=*b {
            if is_double(&n.to_string()) { bf1 += n; }
        }
    }
    let t_bf1 = t0.elapsed();

    // Optimized Part2 (generation + dedupe)
    // Math explanation:
    // If an ID is r >= 2 repetitions of a k-digit block x (written x repeated r times),
    // the resulting number n is:
    //   n = x * (10^{k*(r-1)} + 10^{k*(r-2)} + ... + 10^{0})
    // Let multiplier M(k,r) = sum_{i=0..r-1} 10^{k*i} = (10^{k*r} - 1) / (10^k - 1) (geometric sum),
    // so n = x * M(k,r). For fixed k and r we generate x in [10^{k-1}, 10^k-1] and compute n,
    // then check whether n falls into any input range. We deduplicate n values across ranges
    // (one n can be inside multiple ranges) using a set.
    // Examples:
    // - k=2, r=2, x=12 -> M = 10^{2}+1 = 101 -> n = 12*101 = 1212
    // - k=2, r=3, x=12 -> M = 10^{4}+10^{2}+1 = 10101 -> n = 12*10101 = 121212
    // - k=1, r=7, x=1 -> M = 1+10+10^2+...+10^6 = 1111111 -> n = 1*1111111 = 1111111
    let t0 = Instant::now();
    let mut opt2_sum: u128 = 0;
    let mut seen_opt2 = BTreeSet::new();
    let max_b = ranges.iter().map(|(_,b)| *b).max().unwrap_or(0);
    let max_digits = max_b.to_string().len();
    for k in 1..=max_digits {
        let ten_k = 10u128.pow(k as u32);
        let x_min = 10u128.pow((k-1) as u32);
        let x_max = ten_k - 1u128;
        for r in 2..=(max_digits / k + 2) {
            let mut mul = 0u128;
            let mut shift = 0u32;
            for _ in 0..r { mul += 10u128.pow(shift); shift += k as u32; }
            let min_n = x_min.saturating_mul(mul);
            if min_n > max_b { break; }
            for x in x_min..=x_max {
                let n = x * mul;
                if n > max_b { break; }
                for (ra, rb) in &ranges {
                    if *ra <= n && n <= *rb {
                        if seen_opt2.insert(n) { opt2_sum += n; }
                        break;
                    }
                }
            }
        }
    }
    let t_opt2 = t0.elapsed();

    // Brute-force Part2
    let t0 = Instant::now();
    let mut bf2: u128 = 0;
    for (a,b) in &ranges {
        for n in *a..=*b {
            if is_repeated(&n.to_string()) { bf2 += n; }
        }
    }
    let t_bf2 = t0.elapsed();

    println!("Optimized Part1: {} (t: {:.3}s)", opt1, t_opt1.as_secs_f64());
    println!("Brute-force Part1: {} (t: {:.3}s)", bf1, t_bf1.as_secs_f64());
    println!("Optimized Part2: {} (t: {:.3}s)", opt2_sum, t_opt2.as_secs_f64());
    println!("Brute-force Part2: {} (t: {:.3}s)", bf2, t_bf2.as_secs_f64());
}
