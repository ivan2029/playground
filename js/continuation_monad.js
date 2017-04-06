//
// inspired by, but not strictly following, Haskell's Cont monad
//

const Cont = (comp) => ({
    // comp :: (a -> r) -> r
    // :: Cont a r -> (a -> r) -> r
    run: comp,
    // :: Cont a r -> (r -> u) -> Cont a u
    fmap: (mapping) => Cont((cb) => comp((x) => cb(mapping(x)))),
    // :: Cont a r -> (r -> Cont a u) -> Cont a u
    bind: (mapping) => Cont((cb) => comp((x) => mapping(x).run((y) => cb(y))))
})

Cont.unit = (x) => Cont((cb) => cb(x))

//
// examples
//
const addCb = (x, y, cb) => cb(x + y)
const twiceCb = (x, cb) => cb(2 * x)
const timeoutCb = (x, cb) => setTimeout(() => cb(x), 2000)

const addCont = (x, y) => Cont((cb) => addCb(x, y, cb))
const twiceCont = (x) => Cont((cb) => twiceCb(x, cb))
const timeoutCont = (x) => Cont((cb) => timeoutCb(x, cb))

Cont.unit(42).
  bind(timeoutCont).
  run(console.log)

addCont(1, 2).
  fmap((x) => x + 1).
  bind(twiceCont).
  bind(timeoutCont).
  run(console.log)
  
  
