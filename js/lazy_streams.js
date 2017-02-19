
let iter = (function (){
  'use strict';
  let exports = {};
  
  //
  let makeIterable = function (iterator) {
    let iterable = {};
    iterable[Symbol.iterator] = iterator;
    return iterable;
  };
  
  //
  let wrap = function (iterable) {
    let wrapped = {};

    wrapped[Symbol.iterator] = function* () {
      for(let x of iterable) {
        yield x;
      }  
    };
  
    //
    // transforms
    //
    
    // this    :: Iterable a
    // mapping :: a -> b
    // result  :: Iterable b
    wrapped.map = function (mapping) {
      let it = function* () {
        for(let x of iterable) {
          yield mapping(x);
        }
      };
      return wrap(makeIterable(it));
    };
    
    // this    :: Iterable a 
    // mapping :: a -> Iterable b
    // result  :: Iterable b
    wrapped.flatMap = function (mapping) {
      let it = function* () {
        for(let x of iterable) {
          for(let y of mapping(x)) {
            yield y;
          }
        }
      };
      return wrap(makeIterable(it));
    };
    
    // this      :: Iterable a
    // predicate :: a -> Boolean
    // result    :: Iterable a
    wrapped.filter = function (predicate) {
      let it = function* () {
        for(let x in iterable) {
          if(predicate(x)){
            yield x;
          }
        }
      };
      return wrap(makeIterable(it));
    };
    
    // this       :: Iterable a 
    // accumulate :: (b, a) -> b
    // initValue  :: b
    // result     :: Iterable b
    wrapped.exclusiveScan = function (accumulate, initValue) {
      let it = function* () {
        let accumulator = initValue;
        
        let iterator = iterable[Symbol.iterator]();
        let prev;
        let next = iterator.next();
    
        if(!next.done){
          yield accumulator;
          
          prev = next;
          next = iterator.next();
          
          for( ; !next.done; prev = next, next = iterator.next()) {
            accumulator = accumulate(accumulator, prev.value);
            yield accumulator;
          }
        }
      };
      return wrap(makeIterable(it));
    };
    
    
    // this       :: Iterable a 
    // accumulate :: (b, a) -> b
    // initValue  :: b
    // result     :: Iterable b
    wrapped.inclusiveScan = function (accumulate, initValue) {
      let it = function* () {
        let accumulator = initValue;
        for(let x of iterable) {
          accumulator = accumulate(accumulator, x);
          yield accumulator;
        }
      };
      return wrap(makeIterable(it));
    };
    
    // this   :: Iterable a
    // n      :: Integer
    // result :: Iterable a
    wrapped.take = function (n) {
      let it = function* () {
        let i = 0;
        let iterator = iterable[Symbol.iterator]();
        let next = iterator.next();
        while(i < n && !next.done) {
          yield next.value;
          ++ i;
          next = iterator.next();
        }
      };
      return wrap(makeIterable(it));
    };
    
    // this   :: Iterable a
    // n      :: Integer
    // result :: Iterable a
    wrapped.skip = function (n) {
      let it = function* () {
        let i = 0;
        let iterator = iterable[Symbol.iterator]();
        let next = iterator.next();
        while(i < n && !next.done) {
          ++ i;
          next = iterator.next();
        }
        while(!next.done) {
          yield next.value;
          next = iterator.next();
        }
      };
      return wrap(makeIterable(it));
    };
    
    //
    // terminators
    //
    
    // this   :: Iterable a
    // result :: Array a
    wrapped.collect = function () {
      let arr = [];
      for(let x of iterable) {
        arr.push(x);
      }
      return arr;
    };
    
    // this   :: Iterable a
    // action :: a -> void
    // result :: void 
    wrapped.each = function (action) {
      for(let x of iterable){
        action(x);
      }
    };
    
    // this       :: Iterable a 
    // accumulate :: (b, a) -> b
    // initValue  :: b
    // result     :: b
    wrapped.fold = function (accumulate, initValue) {
      let accumulator = initValue;
      for(let x of iterable) {
        accumulator = accumulate(accumulator, x);
      }
      return accumulator;
    };
    
    
    //
    return wrapped;
  };
  
  //
  // sources
  //
  
  exports.fromIterable = wrap;
  
  exports.empty = wrap([]);
  
  exports.single = x => wrap([x]);
  
  exports.repeat = function (x) {
    let it = function* (){
      while(true){
        yield x;
      }
    };
    return wrap(makeIterable(it));
  };
  
  exports.applyRepeatedly = function (fn, initValue) {
    let it = function* () {
      let current = initValue;
      yield current;
      while(true){
        current = fn(current);
        yield current;
      }
    };
    return wrap(makeIterable(it));
  };
  
  exports.steppingRange = function(start, end, step) {
    let it = function* () {
      for(let current = start; current < end; current += step) {
        yield current;
      }
    };
    return wrap(makeIterable(it));
  };
  
  exports.concat = function () {
    let args = Array.prototype.slice.apply(arguments);
    let it = function* () {
      for(let arg of args) {
        for(let x of arg) {
          yield x;
        }
      }
    };
    return wrap(makeIterable(it));
  };
  
  exports.cycle = function(iterable) {
    let it = function* () {
      while(true) {
        for(let x of iterable) {
          yield x;
        }
      }
    };
    return wrap(makeIterable(it));
  };
  //
  return exports;
})();


//
// tests
//

var LOG = console.log.bind(console);

let EXPECT = function (name, expected, actual) {
  LOG(name + ': expected: |' + expected + '|, got |' + actual + '|');
};

//

EXPECT( 'filter then map'
      , [2, 6]
      , iter.fromIterable([1,2,3,4])
          .filter( x => 0 !== x%2 )
          .map( x => x*2 )
          .collect()
);

EXPECT( 'exclusiveScan'
      , [0, 1, 2, 3, 4]
      , iter.fromIterable([1,1,1,1,1])
          .exclusiveScan((acc, x) => acc + x, 0)
          .collect()
);

EXPECT( 'exclusiveScan'
      , [1, 2, 3, 4, 5]
      , iter.fromIterable([1,1,1,1,1])
          .inclusiveScan((acc, x) => acc + x, 0)
          .collect()
);

EXPECT( 'fold' 
      , 55
      , iter.fromIterable([1,2,3,4,5,6,7,8,9,10])
          .fold( (x, y) => x + y, 0)
);

EXPECT( 'empty'
      , []
      , iter.empty.collect()
);

EXPECT( 'single'
      , [1]
      , iter.single(1).collect()
);

EXPECT( 'repeat then take'
      , [1,1,1]
      , iter.repeat(1).take(3).collect()
);

EXPECT( 'apply repeatedly'
      , [1,2,3]
      , iter.applyRepeatedly( x => x + 1, 1).take(3).collect()
);
     
EXPECT( 'stepping range'
      , [1,3,5,7,9]
      , iter.steppingRange(1, 10, 2).collect()
);

let test_it = iter.fromIterable([1,2,3]);
EXPECT( 'concat' 
      , [1,2,3,1,2,3,1,2,3]
      , iter.concat( test_it ).collect()
);

EXPECT( 'flatMap'
      , ['a','a','a','b','b','b','c','c','c']
      , iter.fromIterable("abc").flatMap( x => iter.repeat(x).take(3) ).collect()
);

EXPECT( 'skip'
      , [3,4,5]
      , iter.fromIterable([1,2,3,4,5]).skip(2).collect()
);

EXPECT( 'cycle'
      , [1,2,1,2,1,2]
      , iter.cycle(iter.fromIterable([1,2])).take(6).collect()
);