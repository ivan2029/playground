use std::{
    sync::{Arc, Mutex,},
    thread::{spawn, sleep,},
    time::{Instant, Duration},
};

use rand::Rng;

fn main() {        
    let forks: Vec<_> = (0..5)
        .map(|id| Arc::new(Mutex::new(id)))
        .collect();

    let philosophers: Vec<_> = (0..5)
        .map(|id| {
            let left_fork  = forks[id].clone();
            let right_fork = forks[(id + 1) % 5].clone();
            
            spawn(move || run_philosopher(id, left_fork, right_fork))
        })
        .collect();
    
    for jh in philosophers.into_iter() {
        jh.join().unwrap();
    }
}


//
//
//
const PHILOSOPHERS: [&str; 5] = [
    "Plato",
    "Aristotel",
    "Socrates",
    "Zenon",
    "Epictetus"
];

const CYCLE_COUNT: u32 = 100;

fn run_philosopher(id: usize, left_fork: Arc<Mutex<usize>>, right_fork: Arc<Mutex<usize>>) {
    let name = &PHILOSOPHERS[id];
    for _cycle in 0..CYCLE_COUNT {
        let think_time = rand::thread_rng().gen_range(100 .. 500);
        let eat_time   = rand::thread_rng().gen_range(100 .. 500);
        
        // think
        println!("{} is thinking for {}ms", name, think_time);
        sleep(Duration::from_millis(think_time));
        
        // grab forks
        let grab_begin = Instant::now();

        let _left_fork_grabbed  = left_fork.lock().unwrap();
        let _right_fork_grabbed = right_fork.lock().unwrap();

        let grab_end   = Instant::now();
        let grab_dt = grab_end - grab_begin;

        println!("{} waited for {}ms", name, grab_dt.as_millis());
        
        // eat
        println!("{} eats for {}ms", name, eat_time);
        sleep(Duration::from_millis(eat_time));

        // forks released here
    }
}
