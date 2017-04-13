//
// inspired by, but not strictly following, Haskell's Cont monad
//
'use strict'

//
//
//

function Cont (comp) {
  this.run = comp
}

Cont.prototype.fmap = function (mapping) {
  const comp = this.run
  return new Cont( cb => comp( x => cb(mapping(x)) ) )
}

Cont.prototype.bind = function (mapping) {
  const comp = this.run
  return new Cont( cb => comp( x => mapping(x).run( cb ) ) )
}

// :: a -> Cont a r
Cont.unit = x => new Cont(cb => cb(x))

// :: Array (Cont a) -> Cont (Array a)
Cont.sequence = vec => {
  const conts = new Array(vec.length)
  let so_far = 0
  const on_run = (i, cb) => x => {
      so_far = so_far + 1
      conts[i] = x
      if(so_far === conts.length) {
        cb(conts)
      }
    }
  return new Cont(cb => {
    for(let i in vec) {
      vec[i].run( on_run(i, cb) )
    }
  })
}

// :: (a -> Cont b) -> Array a -> Cont (Array b)
Cont.mapM = mapping => vec => Cont.sequence(vec.map(mapping))

// helper constructor
const cont = comp => new Cont(comp)


//
// Test, arithmetics
//
const addCb     = (x, y, cb) => cb(x + y)
const twiceCb   = (x,    cb) => cb(2 * x)
const timeoutCb = (x,    cb) => setTimeout(() => cb(x), 2000)

const addCont     = (x, y) => cont( cb => addCb(x, y, cb))
const twiceCont   = (x)    => cont( cb => twiceCb(x, cb))
const timeoutCont = (x)    => cont( cb => timeoutCb(x, cb))


addCont(1, 2).
   fmap((x) => x + 1).
   bind(twiceCont).
   bind(timeoutCont).
   run(console.log)

//
// Test (a movie catalog)
//
const service = (() => {
    const exports = {}

    // helpers
    let next_id = 100;
    const get_id = () => ++next_id

    const uniform_int = (bottom, top) => () => Math.floor(Math.random() * (1 + top - bottom)) + bottom
    const rand = uniform_int(500, 2000)
    const delay = action => setTimeout(action, rand())
    //const delay = action => action()
    
    //
    const data = []

    // implementations
    const insert = (name, year, releases) => data.push({
        id: get_id(),
        name: name,
        year: year,
        releases: releases
    })

    const get_movies = () => data.map(el => ({id:el.id, name: el.name}))

    const get_movie_by_id = (id) => data.find( el => el.id === id ) || { error : "Item not found" }


    // with callbacks
    exports.insert_cb = (name, year, releases, cb) => delay(() => insert(name, year, releases))
    exports.get_movies_cb = cb => delay( () => cb(get_movies()) )
    exports.get_movie_by_id_cb = (id, cb) => delay ( () => cb(get_movie_by_id(id)) )

    // with continuations
    exports.insert_cont = (name, year, releases) => cont( cb => exports.insert_cb(name, year, releases, cb) )
    exports.get_movies_cont = () => cont( cb => exports.get_movies_cb(cb) )
    exports.get_movie_by_id_cont = id => cont( cb => exports.get_movie_by_id_cb(id, cb) )

    // insert some data
    insert("The Matrix", 1999, [{
            type: "dvd",
            year: "2000"
        },
        {
            type: "blu-ray",
            year: "2010"
        },
    ])
    insert("Lord of the Rings", 2003, [{
            type: "dvd",
            year: "2003"
        },
        {
            type: "dvd, extended edition",
            year: "2004"
        },
        {
            type: "blu-ray",
            year: "2010"
        },
        {
            type: "blu-ray, extended",
            year: "2010"
        },
    ])
    insert("District 9", 2009, [{
        type: "blu-ray",
        year: "2010"
    }, ])
    insert("Blade Runner", 1982, [{
            type: "dvd, directors cut",
            year: "2000"
        },
        {
            type: "blu-ray, final cut",
            year: "2011"
        },
    ])

    //
    return exports
})()

service.get_movies_cb(console.log)
service.get_movie_by_id_cb(103, console.log)
service.get_movie_by_id_cb(200, console.log)

service.get_movies_cont().run(console.log)
service.get_movie_by_id_cont(103).run(console.log)
service.get_movie_by_id_cont(200).run(console.log)

//
const get_all_movies = service => service.get_movies_cont().bind(
  Cont.mapM(el => service.get_movie_by_id_cont(el.id))
)

get_all_movies(service).run(console.log)

