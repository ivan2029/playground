//
// task : compute character frequency map for all files in given dir, print top 5 most frequent
//
// using: NodeJS, async.js
//

//
// Callback a = (Error, a) -> Void
//

const fs = require('fs')
const as = require('async')
const _  = require('lodash')

//
//
//
const frequencyMap = (string, callback) => {
    let fmap = {}
    for(let c of string) {
        if(fmap[c]) {
            fmap[c] = fmap[c] + 1
        } else {
            fmap[c] = 1
        }
    }
    callback(null, fmap)
}

// :: (Path, Callback (Array (Path ,Map Char Int))) -> Void
const frequencyMapInPathCB = (path, callback) => {
    fs.readdir(path, (err, files) => {
        if(err) {
            return callback(err)
        }
        let reports = []
        let fileCount = 0
        for(let file of files) {
            fs.stat(file, (err, stat) => {
                if(err) {
                    return callback(err)
                }
                if(stat.isFile()) {
                    fileCount = fileCount + 1
                    fs.readFile(file, 'ascii', (err, content) => {
                        if(err) {
                            return callback(err)
                        }
                        content = _.filter( content,
                                            c => /[a-zA-Z0-9]/.test(c)  )
                        frequencyMap(content, (err, fmap) => {
                            if(err) {
                                return callback(err)
                            }
                            reports.push({path:file, fmap:fmap})
                            if(reports.length === fileCount) {
                                callback(null, reports)
                            }
                        })
                    })
                }
            })
        }
    })
}

/*
const isFile = (path, callback) => {
    fs.stat(path, (err, stat) => {
        if(err) {
            return callback(err)
        }
        callback(null, stat.isFile())
    })
}
*/

// :: (Path, Callback (Array (Path ,Map Char Int))) -> Void
const frequencyMapInPathAS = (path, callback) => {
    const isFile = as.compose( as.asyncify( stat => stat.isFile()), fs.stat )
    const alnum  = as.asyncify( string => _.filter( string, c => /[a-zA-Z0-9]/.test(c) ) )
    
    const computeFmap = (path, callback) => {
        as.waterfall([
            as.apply(fs.readFile, path, 'ascii'),
            alnum,
            frequencyMap
        ], (err, fmap) => {
            if(err) {
                return callback(err)
            }
            callback(null, {path:path, fmap:fmap})
        })           
    }

    //
    as.waterfall([
        as.apply(fs.readdir, path),
        (paths, wf_callback) => as.filter(paths, isFile, wf_callback),
        (paths, wf_callback) => as.map(paths, computeFmap, wf_callback)
    ], callback)
}

//
// test helpers
//

// :: Object -> Array
const objectToArray = obj => {
    let arr = []
    for(let key in obj) {
        arr.push([key, obj[key]])
    }
    return arr
}

// :: (Int, Map Char Int) -> Array (Char, Int)
const topFreqs = (n, fmap) => {
    fmap = objectToArray(fmap)
    fmap.sort((a, b) => {
        if(a[1] > b[1]) {
            return -1
        }
        if(a[1] < b[1]) {
            return 1
        }
        return 0
    })
    return fmap.slice(0, n)
}

const testCallback = msg => (err, reports) => {
    if(err) {
        console.log(err)
    }
    console.log(`msg: ${msg}`)
    for(let report of reports) {
        console.log(`path: ${report.path}`)
        for(let pair of topFreqs(5,report.fmap)) {
            console.log(`'${pair[0]}' -> ${pair[1]}`)
        }
    }
    console.log()
}

//
// tests
//


frequencyMapInPathCB('.', testCallback('with callback'))
frequencyMapInPathAS('.', testCallback('with async.js'))

