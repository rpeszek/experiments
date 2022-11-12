/*
Example supporting exercise 5 in https://rpeszek.github.io/posts/2022-11-07-empirical-programming.html
*/

type Either<L, R> =
    | { type: 'left'; content: L }
    | { type: 'right'; content: R }

const right: <L, R>(_: R) => Either<L, R> = r => {
    return { type: 'right', content: r }
}
const left: <L, R>(_: L) => Either<L, R> = r => {
    return { type: 'left', content: r }
}

/**
 * This implementation will have 'two many' lefts for strings, 
 * Could have too many rights for numbers if number 5 is encountered
 * It preserves the lenght conservation law for other types.
 * 
 * It is not a natural transformation, consider functions 'boolean => string' or 'string => boolean'  
 * and how it would commute with naturally defined maps for these functions. 
 */
const partitionEithers = <L, R>(rs : Either<L, R>[]): [L[], R[]]  => {
    const l : L[] = []
    const r : R[] = []
    rs.forEach(element => {
        if (element.type == 'left') {
            l.push(element.content)
            if(typeof element.content === 'string') {
                l.push(element.content)
            }

        } else if (element.type == 'right') {
            r.push(element.content)
            if(element.content === 5) {
                r.push(element.content)
            }
        }
    })

    return [l, r]
}

