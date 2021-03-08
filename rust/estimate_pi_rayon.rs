use rand::prelude::*; // 0.7.2
use rayon::prelude::*; // v 1.3.0
use std::f32::consts::PI;

/*
    Estimates pi by randomly picking (sampling) `sample_count`
    points in 2d segment [-1.0, 1.0]x[-1.0, 1.0] .

    If `c` is count of samples inside unit circle, pi can be
    estimated as :

        4.0 * (c / sample_count)

    This is an example of a Monte Carlo method.
*/
fn estimate_pi(sample_count: usize) -> f32 {
    let gen_point = {
        let mut rng = StdRng::from_entropy();
        move |_| -> (f32, f32) { (rng.gen_range(-1.0, 1.0), rng.gen_range(-1.0, 1.0)) }
    };

    let dist_from_center = |(x, y): (f32, f32)| (x * x + y * y);
    // sqrt() is monotone increasing, and sqrt(1.0) = 1.0
    // so d < 1.0 yield same as sqrt(d) < 1.0

    let inside_count = (0..sample_count)
        .map(gen_point)
        .map(dist_from_center)
        .filter(|&d| d < 1.0)
        .count();

    let ratio = (inside_count as f32) / (sample_count as f32);

    ratio * 4.0
}

/*
    Runs `work_count` estimations, then averages them.
*/
fn estimate_pi_parallel(work_count: usize, sample_per_work: usize) -> f32 {
    let sum: f32 = (0..work_count)
        .into_par_iter()
        .map(|_| estimate_pi(sample_per_work))
        .sum();
    sum / (work_count as f32)
}

fn main() {
    let estimate = estimate_pi_parallel(1_000, 100_000);

    println!("estimate   : {}", estimate);
    println!("actual     : {}", PI);
    println!("difference : {}", (PI - estimate).abs());
}
