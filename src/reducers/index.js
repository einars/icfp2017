import update from 'immutability-helper'
import { types } from '../actions/types'


// state:
//  vis:
//    games: [ name, name, name ]
//    game: { map, moves, ... }
//
//


export default function stuff (state = {
  frame: 0,
  games: [],
  game: {
    map: {
      sites: [],
      rivers: [],
      mines: [],
    },
    moves: [],
    score: [],
  }
}, action) {

  
  switch(action.type) {
  case types.RECEIVE_GAMES:
    return update(state, {
      games: { $set: action.games }
    })
  case types.RECEIVE_GAME:
    mutableTransformGame(action.game)
    return update(state, {
      game: { $set: action.game }
    })
  case types.SET_FRAME:
    return update(state, {
      frame: { $set: action.frame }
    })
  default:
    return state
  }

}

const mutableTransformGame = game => {

    let m = game.moves
    m.reverse()
    m.forEach( (m, idx) => {
      if ( !! m.claim) {
        const source = m.claim.source
        const target = m.claim.target
        const elem = game.map.rivers.find( e => (e.source === source && e.target === target) || (e.source === target && e.target === source))
        elem.frame = idx
      }
    })
}


