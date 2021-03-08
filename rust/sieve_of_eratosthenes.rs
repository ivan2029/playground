
mod bitset {

    use std::fmt;

    pub struct BitSet { // could be Clone
        len: usize,
        bits: Vec<u64>,
    }

    impl BitSet {

        pub fn with_len(len: usize) -> BitSet { // all are unset
            BitSet{
                len,
                bits: vec![0u64; (len + 63)/64],
            }
        }

        pub fn set(&mut self, idx: usize) {
            let (bucket, mask) = self.bucket_and_mask(idx);
            self.bits[bucket] |= mask;
        }
          
        pub fn unset(&mut self, idx: usize) { // I don't use this
            let (bucket, mask) = self.bucket_and_mask(idx);
            self.bits[bucket] &= !mask;
        }
        
        pub fn test(&self, idx: usize) -> bool {
            let (bucket, mask) = self.bucket_and_mask(idx);
            (self.bits[bucket] & mask) != 0
        }

        fn bucket_and_mask(&self, idx: usize) -> (usize, u64) {
            assert!( idx < self.len );

            let bucket = idx >> 6; // idx / 64
            let pos = 63 - (idx & 0b0011_1111);            
            let mask = 0x1u64 << pos as u64;
            
            (bucket, mask as u64)
        }
        
    }

    impl fmt::Debug for BitSet {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            writeln!(f, "bitset: len = {}, bits = [", self.len)?;

            for (idx, bucket) in self.bits.iter().enumerate() {
                let bytes = bucket.to_be_bytes();
                writeln!(f, "bucket {}: 0xb'{:0>8b}'{:0>8b}'{:0>8b}'{:0>8b}'{:0>8b}'{:0>8b}'{:0>8b}'{:0>8b}"
                         , idx
                         , bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                )?;
            }
            
            writeln!(f, "]")?;

            Ok(())
        }
    }
    
}

//
//
//
/*  
    We know even numbers are non-prime (being divisible by two),
    and we test only odd numbers greater than 2.
    
    Integer division is really slow on modern x86_64. Bitshifts are 
    fast though. Since `n / 2 == n >> 1` (with the way integers are
    represented), shifting is used instead of division. Note, that 
    modern compilers will likely replace such division with shifts 
    anyway, so no need to do it this way.
*/

fn is_odd(n: u32) -> bool {
    (n & 0x1) == 0x1 // n % 2 == 1
}

fn num_to_pos(n: u32) -> u32 {
    assert!( is_odd(n) );
    (n - 3) >> 1 // (n - 3) / 2
}

fn pos_to_num(n: u32) -> u32 {
    n * 2 + 3
}    
    
fn main() {
    let test_count = 50_000;
    let max_candidate = pos_to_num(test_count);
    
    let mut bs = bitset::BitSet::with_len(test_count as _);

    // mark non-primes
    for pos in 0..test_count {
        if bs.test(pos as _) { continue; }

        let n = pos_to_num(pos);
        
        for i in (n*2 .. max_candidate).step_by(n as _) {
            if ! is_odd(i) { continue; }

            let i_pos = num_to_pos(i);
            bs.set(i_pos as _);
        }
    }
    
    // print
    for pos in 0..test_count {
        if bs.test(pos as _) { continue; }
        let n = pos_to_num(pos);
        println!("{}", n);
    }
}
