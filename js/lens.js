/*
Here, lens is any object that has methods

get :: s -> a
set :: (s, a) -> s

where s is object type, and a is 'field' type.
Both methods must be pure (have no side effects, do not modify state)!

*/

const lens = {
  //
  make: (getter, setter) => ({
    get: getter,
    set: setter
  }),

  //
  from_key: key => ({
    get: obj => obj[key],
    set: (val, obj) => ({...obj, [key]:val})
  }),

  //
  from_index: index => ({
    get: obj => obj[index],
    set: (val, obj) => {
      const copied = [...obj]
      copied[index] = val
      return copied
    }
  }),

  //
  /*
    projection is map: key -> lens
  */
  from_projection: projection => ({
    get: obj => {
      const result = {}
      for(let key in projection) {
        result[key] = projection[key].get(obj)
      }
      return result
    },
    set: (val, obj) => {
      let result = obj
      for(let key in projection) {
        result = projection[key].set(val[key], result)
      }
      return result
    }
  }),

  //
  update: (lens, fn, obj) => lens.set( fn(lens.get(obj)), obj ),

  //
  and_then: (a, b) => ({ // a and_then b
    get: obj => b.get(a.get(obj)),
    set: (val, obj) => a.set(b.set(val, a.get(obj)), obj)
  }),

  //
  and_then_many: lenses => lenses.reduce(lens.and_then),

  //
  zip2: (a, b) => ({
    get: obj => [ a.get(obj), b.get(obj) ],
    set: (vals, obj) => b.set(vals[1], a.set(vals[0], obj)) 
  })

};

//
//
//
const user = {
  name: "someone",
  id: 8309,
  company: {
    id: 23,
    name: "foobar",
    address : {
      street: "blasted street",
      zip: "123U5"
    }
  },
  some: [1,2,3,4]
}

const name    = lens.from_key("name")
const company = lens.from_key("company")
const some    = lens.from_key("some")

const company_name = lens.and_then(company, name)
const company_zip = lens.and_then_many( 
  ["company", "address", "zip"].map(lens.from_key) 
)
const zipped = lens.zip2(company_name, company_zip)

const second = lens.from_index(1)

const projection = {
  user_name:    name,
  company_name: company_name,
  company_zip:  company_zip
}

const proj_lens = lens.from_projection(projection)


// console.log( name.get(user) )
// console.log( name.set("wanda", user) )

// console.log( company_name.get(user) )
// console.log( company_name.set("endava", user) )

// console.log( company_zip.get(user) )
// console.log( company_zip.set("WAAAGH!", user))

// console.log( zipped.get(user) )
// console.log( zipped.set(["m$", "seattle"], user))

// console.log( lens.update(
//     some,
//     xs => xs.map( x => `~${x}~`),
//     user
// ))

// console.log(
//   lens.and_then(some, second).get(user)
// )
// console.log(
//   lens.and_then(some, second).set("WAAAGH!", user)
// )


console.log( proj_lens.get(user) )

const new_proj_val = {
  user_name: 'noone',
  company_name: 'zumba',
  company_zip: 'balloney'
}
console.log( proj_lens.set(new_proj_val, user) )