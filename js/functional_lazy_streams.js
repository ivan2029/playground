/*
    Requires JavaScript interpreter which supports iterators and generators
    
    Even though curry function exists, lots of methods have curried version.
    Naming convention is this: if method's name is 'func', curried version's 
    name is 'funcC'.
    
    Example:
  
    fp.curry(fp.map, (x) => x*2 );
    
    // is equivalent to
  
    fp.mapC( (x) => x*2 );
  
 */


var fp = (function () {
  'use strict';
  var exports = {};
  
  var makeIterable = function (iterator) {
    var iterable = {};
    iterable[Symbol.iterator] = iterator;
    return iterable;
  };
  

  // given: fnC = curry(fn, a1, a2, a3)
  // then: 
  //   fnC(b1, b2) 
  // is equivalent to:
  //   fn(a1, a2, a3, b1, b2)
  // works with arbitrary number of arguments
  exports.curry = function () {
    var slice = Array.prototype.slice;
    var args = slice.apply(arguments, [1]);
    var fn = arguments[0];
    return function () {
      return fn.apply(null, args.concat(slice.apply(arguments)));
    };
  };
  
  // like compose, but functions are applied in reverse order
  // given function f1, f2, ... fn
  // returns function that is equivalent to:
  // x => fn( ... ( f2 ( f1 (x) ) ) )
  exports.chain = function () {
    var args = Array.prototype.slice.apply(arguments);
    return function (x) {
      var y = x;
      for(let fn of args) {
        y = fn(y);
      }
      return y;
    };
  };
  
  // like chain, but functions are applied in reverse order
  // given function f1, f2, ... fn
  // returns function that is equivalent to:
  // x => f1( f2 ( ... ( fn(x) ) ) )
  exports.compose = function () {
    var args = Array.prototype.slice.apply(arguments);
    args.reverse();
    return exports.chain.apply(null, args);
  };
  
  //
  // transformers
  //
  
  // mapping  :: a -> b 
  // iterable :: Iterable a 
  // result   :: Iterable b
  exports.map = function (mapping, iterable) {
    return makeIterable ( function* () {
      for(let x of iterable) {
        yield mapping(x);
      }
    });
  };
  
  exports.mapC = (mapping) => (iterable) => exports.map(mapping, iterable);
  
  // predicate :: a -> Boolean
  // iterable  :: Iterable a
  // result    :: Iterable a
  exports.filter = function (predicate, iterable) {
    return makeIterable ( function* () {
      for(let x of iterable) {
        if( predicate(x) ){
          yield x;
        }
      }
    });
  };
  
  exports.filterC = (predicate) => (iterable) => exports.filter(predicate, iterable);
  
  // accumulate :: (b, a) -> b 
  // initValue  :: b 
  // iterable   :: Iterable a
  // result     :: Iterable b
  exports.exclusiveScan = function (accumulate, initValue, iterable) {
    return makeIterable ( function* () {
      var accumulator = initValue;
      
      var iterator = iterable[Symbol.iterator]();
      var prev;
      var next = iterator.next();
  
      if(!next.done){
        yield accumulator;
        
        prev = next;
        next = iterator.next();
        
        for( ; !next.done; prev = next, next = iterator.next()) {
          accumulator = accumulate(accumulator, prev.value);
          yield accumulator;
        }
      }
      
    });
  };
  
  exports.exclusiveScanC = (accumulate, initValue) => (iterable) => exports.exclusiveScan(accumulate, initValue, iterable);
  
  // accumulate :: (b, a) -> b 
  // initValue  :: b 
  // iterable   :: Iterable a
  // result     :: Iterable b
  exports.inclusiveScan = function (accumulate, initValue, iterable) {
    return makeIterable ( function* () {
      var accumulator = initValue;
      for(let x of iterable) {
        accumulator = accumulate(accumulator, x);
        yield accumulator;
      }
    });
  };
  
    exports.inclusiveScanC = (accumulate, initValue) => (iterable) => exports.inclusiveScan(accumulate, initValue, iterable);
  
  //
  // terminators
  //
  
  // iterable :: Iterable a 
  // result   :: Array a
  exports.collect = function (iterable) {
    var arr = [];
    for(let x of iterable) {
      arr.push(x);
    }
    return arr;
  };
  
  // accumulate :: (b, a) -> b 
  // initValue  :: b 
  // iterable   :: Iterable a
  // result     :: b
  exports.fold = function (accumulate, initValue, iterable) {
    var accumulator = initValue;
    for(let x of iterable) {
      accumulator = accumulate (accumulator, x);
    }
    return accumulator;
  };
  
  exports.foldC = (accumulate, initValue) => (iterable) => exports.fold(accumulate, initValue, iterable);
  
  // action   :: a -> void
  // iterable :: Iterable a
  // result   :: void
  exports.each = function (action, iterable) {
    for(let x of iterable) {
      action(x);
    }
  };
  
  exports.eachC = (action) => (iterable) => exports.each(action, iterable);
  
  //
  return exports;
})();

//
// tests
//

var LOG = console.log.bind(console);

var EXPECT = function (name, expected, actual) {
  LOG(name + ': expected: ' + expected + ', got ' + actual);
};

// curry
EXPECT( 'curry'
      , 3
      , fp.curry( (x, y) => x + y , 1 )(2)
);

// compose
EXPECT( 'compose' 
      , '~<0>~'
      , fp.compose ( x => '~' + x + '~'
                   , x => '<' + x + '>')(0)
);

// chain
EXPECT( 'chain' 
      , '<~0~>'
      , fp.chain ( x => '~' + x + '~'
                 , x => '<' + x + '>')(0)
);

// map
EXPECT( 'map'
      , [2, 4, 6]
      , fp.collect( fp.map( x => x*2, [1,2,3]) )
);

EXPECT( 'mapC'
      , [2, 4, 6]
      , fp.chain( fp.mapC( x => x*2 ), fp.collect) ( [1,2,3] ) 
);

// filter
EXPECT( 'filter'
      , [1,3,5]
      , fp.collect( fp.filter( x => 0 !== x%2, [1,2,3,4,5,6]) )
);

EXPECT( 'filterC'
      , [1,3,5]
      , fp.chain( fp.filterC( x => 0 !== x%2 ), fp.collect)( [1,2,3,4,5,6] ) 
);

// scans
EXPECT( 'exclusiveScan'
      , [0,1,2,3,4]
      , fp.collect( fp.exclusiveScan( (x, y) => x + y, 0, [1,1,1,1,1]) )
);

EXPECT( 'exclusiveScanC'
      , [0,1,2,3,4]
      , fp.chain( fp.exclusiveScanC( (x, y) => x + y, 0 ), fp.collect)( [1,1,1,1,1] ) 
);

EXPECT( 'inclusiveScan'
      , [1,2,3,4,5]
      , fp.collect( fp.inclusiveScan( (x, y) => x + y, 0, [1,1,1,1,1]) )
);

EXPECT( 'inclusiveScanC'
      , [1,2,3,4,5]
      , fp.chain( fp.inclusiveScanC( (x, y) => x + y, 0 ), fp.collect)( [1,1,1,1,1] ) 
);

// fold
EXPECT( 'fold'
      , 55
      , fp.fold( (x,y) => x + y, 0, [1,2,3,4,5,6,7,8,9,10])
);

EXPECT( 'foldC'
      , 55
      , fp.foldC( (x,y) => x + y, 0)( [1,2,3,4,5,6,7,8,9,10] )
);

LOG('each');
fp.each(LOG, [1,2,3] );

LOG('eachC');
fp.eachC(LOG)( [1,2,3] );

// combo
LOG('combination of things')
fp.chain( fp.filterC( x => 0 !== x%2 )
        , fp.mapC( x => '<' + x + '>')
        , fp.inclusiveScanC( (acc, x) => acc + x, '' )
        , fp.eachC(LOG)
  )([1,2,3,4,5,6]);

