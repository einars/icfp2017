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
  return function receiveGamesImpl (dispatch) {
    if (games.length > 0) {
      dispatch(requestGame(games[0].name))
    }
    dispatch({
      games,
      type: types.RECEIVE_GAMES,
    })
  }
}

export function setFrame (frame) {
  return {
    frame,
    type: types.SET_FRAME,
  }
}

export function setZoom (zoom) {
  return {
    zoom,
    type: types.SET_ZOOM,
  }
}


export function requestGame (id) {
  return function requestGameImpl (dispatch) {
    return axios
      .get('http://192.168.8.105/punt/?action=game&id=' + id)
      .then(res => {
        
        const game = res.data

        game.id = id

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

        dispatch(setFrame(0))
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

