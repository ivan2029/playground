/*
    Concept C is a set of requirements. If object satisfies these requirements
    we say that object models concept C.
    
    Requirements are expressed as expressions that should be valid for given 
    object.
    
    Examples:
    
    Object 'it' is InputIterator<A> if:
    
    var el = it.next(); // where el models A or is null
                        // if el is null, another call to next is undefined

    Object 'out' is Inserter<A> if:
    
    out.push(a); // where 'a' models A

    Functions model Function<(Arg1, ... ArgN), Res>, or short: (Arg1, ... ArgN) -> Res
    Predicate<Arg1, ... ArgN> = (Arg1, ... ArgN) -> Bool, where N > 0
    UnaryPredicate<A> = Predicate<A>
    BinaryPredicate<A, B> = Predicate<A, B>
*/

//
// algorithms
//

// inputIterator models InputIterator<A>
// action models A -> void
var for_each = function(inputIterator, action){
    for(var el = inputIterator.next(); el !== null; el = inputIterator.next()){
        action(el);
    }
};

// inputIterator models InputIterator<A>
// inserter models Inserter<B>
// transformF models A -> B
var transform = function(inputIterator, inserter, transformF) {
    var action = function (el) {
        var transformed_element = transformF(el);
        inserter.push(transformed_element);
    };
    for_each(inputIterator, action);
};

// inputIterator models InputIterator<A>
// inserter models Inserter<A>
// predicated models UnaryPredicate<A>
var filter = function(inputIterator, inserter, predicate){
    var action = function(el){
        if( predicate(el) ){
            inserter.push(el);
        }  
    };
    for_each(inputIterator, action);
};

// inputIterator models InputIterator<A>
// initValue models S -- think 'State'
// folding models (A, S) -> S
var fold = function(inputIterator, initValue, folding) {
    var result = initValue;
    var action = function(el){
        result = folding(el, result);
    };
    for_each(inputIterator, action);
    return result;
};

//
//
//

// ArrayInputIterator<A> models InputIterator<A>
var ArrayInputIterator = function(array){
    this.array = array;
    this.index = 0;
    
    // InputIterator
    this.next = function() {
        if(this.index >= this.array.length){
            return null;
        }  
        var old_index = this.index;
        this.index = this.index + 1;
        return this.array[old_index];
    };
};

var input_iterator = function(obj){
    if(Array.isArray(obj)){
        return new ArrayInputIterator(obj);
    }
    return null;
};

var array = [1,2,3,4,5];

// for_each
console.log("# for each");
for_each( input_iterator(array) , console.log );

// transform
console.log("# transform");

var doubled = [];
var times2 = function(x) { return 2*x; };
transform( input_iterator(array), doubled, times2 );
console.log(doubled);

// filter
console.log("# filter");

var evens = [];
var is_even = function(integral) { return integral % 2 === 0; };
filter(input_iterator(array), evens, is_even);
console.log(evens);

// fold
console.log("# fold.sum");

var plus = function(x, y){ return x + y; };
var sum = fold(input_iterator(array), 0, plus);
console.log(sum);

// fold 2
console.log("# fold.statistics");

var initial_state = { evens: 0, odds: 0 };
var update_state = function(value, state) {
    return { evens: state.evens + (value + 1) % 2,
             odds: state.evens + value % 2 
    };
}
var folded_state = fold(input_iterator(array), initial_state, update_state);
console.log(folded_state);