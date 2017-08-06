import React, { Component } from 'react'
import { connect } from 'react-redux'

import playercolors from './playercolors'

class Visualizer extends Component {

  line (source, dest) {
    const src = this.props.game.map.sites.find( e => e.id === source)
    const dst = this.props.game.map.sites.find( e => e.id === dest)
    return {
      x1: src.x,
      y1: src.y,
      x2: dst.x,
      y2: dst.y,
    }
  }

  circle (id) {
    const src = this.props.game.map.sites.find( e => e.id === id)
    return {
      x: src.x,
      y: src.y,
    }
  }

  render () {

    if (this.props.game.moves.length === 0) return null

    const strokeColorFor = (m) => {
      if (m.owner === null || m.frame === undefined || m.frame >= this.props.frame) {
        return playercolors.none
      } else if (m.owner === 0) {
        return playercolors.p0
      } else if (m.owner === 1) {
        return playercolors.p1
      } else if (m.owner === 2) {
        return playercolors.p2
      } else {
        return playercolors.other
      }
    }

    let pixelw

    const strokeWidthFor = m => {
      if (m.owner === null || m.frame === undefined || m.frame >= this.props.frame) {
        return pixelw
      } else {
        return pixelw * 3
      }
    }


    const viewbox = () => {
      let have_some = false
      let min_x, max_x, min_y, max_y
      this.props.game.map.sites.forEach( s => {
        if ( ! have_some) {
          min_x = s.x
          max_x = s.x
          min_y = s.y
          max_y = s.y
          have_some = true;
        } else {
          if (s.x < min_x) min_x = s.x
          if (s.y < min_y) min_y = s.y
          if (s.x > max_x) max_x = s.x
          if (s.y > max_y) max_y = s.y
        }
      })

      let wx = max_x - min_x
      let wy = max_y - min_y
      let ww = wx > wy ? wx : wy
      let cx = min_x + wx / 2
      let cy = min_y + wy / 2
      wx = ww * 1.1
      pixelw = wx / 1200

      let viewbox = `${cx - wx / 2} ${cy - wx / 2} ${wx} ${wx}`

      return viewbox

    }


    return (
      <svg width={1200} height={700} viewBox={viewbox()} className="visualization">
      {
        this.props.game.map.rivers.map( m => {
          const coords = this.line(m.source, m.target)
          return (
            <line className="svg-line" key={`${m.source}:${m.target}`} stroke={strokeColorFor(m)} strokeWidth={strokeWidthFor(m)} {...coords}><title>{m.source} ... {m.target}</title></line>
          )
        })
      }
      {
        this.props.game.map.sites.map( m => {
          const coords = this.circle(m.id)
          return (
            <circle className="svg-circle" r={pixelw * 3} cx={coords.x} cy={coords.y} key={`${m.id}`} stroke="#111" fill="rgba(255, 255, 255, 0.9)" strokeWidth={pixelw}><title>{m.id}</title></circle>
          )
        })
      }
      {
        this.props.game.map.mines.map( m => {
          const coords = this.circle(m)
          return (
            <circle className="svg-circle" r={pixelw * 7} cx={coords.x} cy={coords.y} key={`${m}`} stroke="#993300" fill="#ff0000" strokeWidth={pixelw * 3}><title>{m}</title></circle>
          )
        })
      }
      </svg>
    )
  }
}

const mapStateToProps = (state, ownProps) => {
  return {
    game:  state.game,
    frame: state.frame,
  }
}


export default connect(mapStateToProps)(Visualizer)
