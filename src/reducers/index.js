import update from 'immutability-helper'
import { types } from '../actions/types'


// state:
//  vis:
//    games: [ name, name, name ]
//    game: { map, moves, ... }
//
//


export default function stuff (state = {
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
    return update(state, {
      game: { $set: action.game }
    })
  default:
    return state
  }

}


