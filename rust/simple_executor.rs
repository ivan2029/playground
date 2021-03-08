use std::future::*;
use std::pin::*;
use std::task::*;

fn main() {
    let raw_waker = RawWaker::new(0 as *const (), &WAKER_VTABLE);
    let waker = unsafe { Waker::from_raw(raw_waker) };
    let mut context = Context::from_waker(&waker);

    //
    let mut task = async {
        println!("Hello, async world!");
        42
    };
    let mut task = unsafe { Pin::new_unchecked(&mut task) };

    'executor: loop {
        match task.as_mut().poll(&mut context) {
            Poll::Pending => println!("pending still"),
            Poll::Ready(val) => {
                println!("got value {}", val);
                break 'executor;
            }
        }
    }
}

const WAKER_VTABLE: &'static RawWakerVTable =
    &RawWakerVTable::new(clone_waker, wake_waker, wake_by_ref_waker, drop_waker);

unsafe fn clone_waker(data: *const ()) -> RawWaker {
    println!("waker: clone");
    RawWaker::new(data, &WAKER_VTABLE)
}

unsafe fn wake_waker(_data: *const ()) {
    println!("waker: wake");
}

unsafe fn wake_by_ref_waker(_data: *const ()) {
    println!("waker: wake_by_ref");
}

unsafe fn drop_waker(_data: *const ()) {
    println!("waker: drop");
}
