
/*
  Based on: http://stlab.cc/libraries/concurrency/channel/index.html
  
  TODO: channel closing

  Sender : {
    send :: Any -> Void
  }

  Receiver : {
    append :: Process -> Receiver
  }

  Proces is either a function or : {
    co_await :: Any -> Void, called every time new value is received
    co_state :: Void -> State, where State is either 'await' or 'yield', indicating if process is ready to send
    co_yield :: Void -> Any, value to be sent further (co_state must return 'yield')
  }
*/

const csp = (() => {
  const exports = {}
  
  //
  let make_channel;
  let make_receiver;
  let make_sender;
  let is_awaitable_process;
  let is_function; 
  let post_task;
  
  //
  const AWAIT = 'await'
  const YIELD = 'yield'
  
  //
  is_awaitable_process = process => 
    is_function(process.co_await) && 
    is_function(process.co_state) && 
    is_function(process.co_yield)
  
  is_function = fn => {
    let getType = {};
    return fn && getType.toString.call(fn) === '[object Function]';
  }
  
  post_task = task => setTimeout(task, 0)
  
  //
  //
  //
  function Channel() {
    this.processes = []
  }
  
  Channel.prototype.append = function(process) {
    const [sender, receiver] = make_channel()
    this.processes.push(args => {
      let result = null;
      if(is_awaitable_process(process)) {
        process.co_await.apply(process, args)
        if(process.co_state() === YIELD){
          result = process.co_yield()
        }
      }
      else {
        result = process.apply(process, args)
      }
      if(result) {
        sender.send(result)
      }
    })
    return receiver
  }
  
  Channel.prototype.send = function() {
    const args = Array.prototype.slice.call(arguments, 0, arguments.length)
    post_task( () => {
      this.processes.forEach(proc => proc(args))
    })
  }
  
  make_receiver = channel => ({
      append : action => channel.append(action)
    })
  
  make_sender = channel => ({
      send : function(){ 
        channel.send.apply(channel, arguments)
      }
    })
  
  make_channel = () => {
    const ch = new Channel()
    return [make_sender(ch), make_receiver(ch)]
  } 
  
  exports.make_channel = make_channel
  
  //
  //
  //
  const join = function () {
    const [sender, receiver] = make_channel()
    const state = {
      values : Array.prototype.map.call(arguments, () => [])
    }
    const send_if_all = () => {
      if(state.values.every(arr => arr.length >= 1)){
        const args = state.values.map(arr => arr[0])
        state.values.forEach(arr => arr.shift())
        sender.send.apply(sender, args)
      }
    }
    const child_process = idx => value => {
        state.values[idx].push(value)
        send_if_all()
      } 
    for(let idx = 0; idx < arguments.length; ++idx) {
      arguments[idx].append(child_process(idx))
    }
    
    return receiver
  }
  
  exports.join = join
  
  //
  //
  //
  const merge = function () {
    const [sender, receiver] = make_channel()

    const process = function () {
      sender.send.apply(sender, arguments)
    } 
    
    for(let recv of arguments) {
      recv.append(process)
    }
    
    return receiver
  }
  
  exports.merge = merge
  
  //
  //
  //
  const filtering_process = predicate => ({
    //
    value: null,
    
    //
    co_await: function(value) {
      if(predicate(value)) {
        this.value = value
      }
    },
    
    //
    co_yield: function() {
      const value = this.value
      this.value = null
      return value
    },
    
    //
    co_state: function(){
      const state = (this.value === null) ? AWAIT : YIELD
      return state
    }
  })
  
  exports.filtering_process = filtering_process
  
  //
  //
  //
  const folding_process = (accumulate, init_value) => ({
    value: init_value,
    
    //
    co_await: function(value) {
      this.value = accumulate(this.value, value)
    },
    
    //
    co_yield: function() {
      return this.value
    },
    
    //
    co_state: function(){
      return YIELD
    }
  })
  
  exports.folding_process = folding_process
  
  
  //
  return exports
})()


//
// test
//
const logging_process = value => console.log('# ',value)

//
const test_1 = () => {
  const [sender, receiver] = csp.make_channel()
  
  receiver.
    append(logging_process)
    
  /* expect:
      #  1
      #  2
      #  3
  */
  
  sender.send(1)
  sender.send(2)
  sender.send(3)
}

const test_2 = () => {
  const [sender, receiver] = csp.make_channel()
  
  const r1 = receiver.append(x => `1: ${x}`)
  const r2 = receiver.append(x => `2: ${x}`)
  
  const r = csp.join(r1, r2)
  
  r.
    append((x, y) => [x, y]).
    append(logging_process)
  
  /* expect:
      #  [ '1: 1', '2: 1' ]
      #  [ '1: 2', '2: 2' ]
      #  [ '1: 3', '2: 3' ]
  */
  
  sender.send(1)
  sender.send(2)
  sender.send(3)
}

const test_3 = () => {
  const [sender, receiver] = csp.make_channel()
  
  const summing_process = csp.folding_process( (acc, x) => acc + x, 0 )
  
  const doubled = receiver.append( x => x*2 )
  const tripled = receiver.append( x => x*3 )
  const combined = csp.join( doubled, tripled ).append((x,y) => x + y )
  const summed = combined.append(summing_process)
  
  summed.append(logging_process)
  
  /* expect:
      #  5
      #  15
  */
  
  sender.send(1)
  sender.send(2)
}

const test_4 = () => {
  const [s1, r1] = csp.make_channel()
  const [s2, r2] = csp.make_channel()
  
  const r = csp.merge(r1, r2)
  
  r.append(logging_process)
  
  /* expect:
      #  1
      #  2
      #  3
      #  4
  */
  
  s1.send(1)
  s2.send(2)
  s1.send(3)
  s2.send(4)
}

const test_5 = () => {
  const [s, r] = csp.make_channel()

  r
    .append( csp.filtering_process( x => x%2 === 0 ) )
    .append( logging_process )
  
  /* expect:
      #  2
      #  4
  */
  
  s.send(1)
  s.send(2)
  s.send(3)
  s.send(4)
}

const test_6 = () => {
  const [s, r] = csp.make_channel()

  r
    .append( csp.folding_process( (acc, x) => acc + x, 0 ) )
    .append( logging_process )

  /* expect:
      #  1
      #  3
      #  6
      #  10
  */

  s.send(1)
  s.send(2)
  s.send(3)
  s.send(4)
}

const test_7 = () => {
  const [s1, r1] = csp.make_channel()
  const [s2, r2] = csp.make_channel()
  const [s3, r3] = csp.make_channel()
  
  const r = 
    csp.join(r1, r2, r3)
      .append((x,y,z) => `${x}, ${y}, ${z}`)
      .append(logging_process)
  
  /* expect:
    
    # 1, a, A
    # 2, b, B
  
  */
  
  s1.send(1)
  s1.send(2)
  s2.send('a')
  s3.send('A')
  s3.send('B')
  s2.send('b')
  s1.send(3)
}

test_1(); 
test_2(); 
test_3(); 
test_4(); 
test_5();
test_6(); 
test_7(); 






