
use futures_lite::future::poll_fn;
use std::{mem, task::{Poll, Context}, future::Future, pin::Pin};

pub(crate) fn join_all<F: Future>(futs: Vec<F>) -> impl Future<Output = Vec<F::Output>> {

    let md_futs: Vec<_> = futs.into_iter().map(MaybeDone::Future).collect();
    let mut pinned_futs = Box::into_pin(md_futs.into_boxed_slice());

    // wait until all the futures are done
    poll_fn(move |ctx| {
        let mut all_done = true;
        for fut in pin_iter_mut(pinned_futs.as_mut()) {
            if fut.poll(ctx).is_pending() { all_done = false };
        }
        if all_done {
            let mut items = mem::replace(&mut pinned_futs, Box::pin([]));
            Poll::Ready(pin_iter_mut(items.as_mut()).map(|md|
                md.take().expect("must be done")
            ).collect())
        } else {
            Poll::Pending
        }
    })

}

// project pin's through a slice and iterate them
fn pin_iter_mut<T>(slice: Pin<&mut [T]>) -> impl Iterator<Item = Pin<&mut T>> {
    // Safety: `std` _could_ make this unsound if it were to decide Pin's
    // invariants aren't required to transmit through slices. Otherwise this has
    // the same safety as a normal field pin projection.
    unsafe { slice.get_unchecked_mut() }.iter_mut().map(|t| unsafe { Pin::new_unchecked(t) })
}

pub(crate) enum MaybeDone<F: Future> {
    Future(F), // #[pin]
    Done(F::Output), // #[pin]
    Gone // #[pin]
}

impl<F: Future> MaybeDone<F> {
     pub fn take(self: Pin<&mut Self>) -> Option<F::Output> {
        if let Self::Done(..) = &*self {
            unsafe {
                match mem::replace(self.get_unchecked_mut(), Self::Gone) {
                    MaybeDone::Done(output) => Some(output),
                    _ => unreachable!(),
                }
            }
        } else {
            None
        }
    }
}

impl<F: Future> Future for MaybeDone<F> {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        unsafe {
            match self.as_mut().get_unchecked_mut() {
                MaybeDone::Future(f) => {
                    let res = futures_lite::ready!(Pin::new_unchecked(f).poll(cx));
                    self.set(Self::Done(res));
                }
                MaybeDone::Done(_) => {}
                MaybeDone::Gone => panic!("value was taken"),
            }
        }
        Poll::Ready(())
    }
}

