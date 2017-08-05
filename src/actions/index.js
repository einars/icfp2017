import axios from 'axios'
import { types } from './types'

export function requestGames () {
  return function requestGamesImpl (dispatch) {
    return axios
      .get('http://192.168.8.105/punt/?action=games')
      .then(res => {
        dispatch(receiveGames(res.data))
      })
  }
}

export function receiveGames (games) {
  return {
    games,
    type: types.RECEIVE_GAMES,
  }
}

export function requestGame (id) {
  return function requestGameImpl (dispatch) {
    return axios
      .get('http://192.168.8.105/punt/?action=game&id=' + id)
      .then(res => {
        
        const game = res.data

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

        dispatch(receiveGame(game))
      })
  }
}

export function receiveGame (game) {
  return {
    game,
    type: types.RECEIVE_GAME,
  }
}

