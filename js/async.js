//
// testing helpers
//
let LOG = console.log.bind(console.log)
let LOGn = (n) => (x) => console.log('#' + n + ' ' + x) 

let simulate_async = (func) => function () {
  let args = Array.prototype.slice.apply(arguments)
  return new Promise( (resolve, reject) => {
    setTimeout(() => resolve(func.apply(func, args)), 1000)
  })
}

let service = (() => {
  let exports = {}
  
  let data = ['first', 'second', 'third']

  exports.get_count = simulate_async( () => data.length )
  
  exports.get_item = simulate_async( index =>  data[index]  )
  
  return exports
})()


//
// async await (node js 7+)
//

let get_all_async = async (service) => {
  let items = []
  let len = await service.get_count()
  for(let index = 0; index < len; ++ index) {
    items.push(await service.get_item(index))
  }
  return items
}

get_all_async(service).then(LOGn(1))

//
// automata
//

let get_all_automata = (service) => {
  let automata = {
    // data
    data: {
      items: [],
      len: null,
    },
  
    // states
    start : () => {
      service.get_count().then((len) => {
        automata.data.len = len
        automata.loop(0)
      })
    },
    loop : (index) => {
      if(index < automata.data.len) {
        service.get_item(index).then((item) => {
          automata.data.items.push(item)
          automata.loop(index + 1)
        })
      }
      else {
        automata.end()
      }
    },
    end : () => {
      automata.resolve(automata.data.items)
    }
  }
  return new Promise((resolve, reject) => {
    automata.resolve = resolve
    automata.start()
  })
}

get_all_automata(service).then(LOGn(2))

//
// recursive
//

let get_all_recursive = (service) => {
  let items = []
  
  let loop = (resolve, len, index) => {
    if( index < len ) {
      service.get_item(index).then((item) => {
        items.push(item)
        loop(resolve, len, index + 1)
      })
    }
    else {
      resolve(items)
    }
  }
  
  return new Promise ((resolve, reject) =>  
    service.
      get_count().
      then((len) => loop(resolve, len, 0)))
}

get_all_recursive(service).then(LOGn(3))
