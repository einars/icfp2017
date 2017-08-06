import React, { PureComponent } from 'react';
import { connect } from 'react-redux'
import { setFrame } from './actions'
import { bindActionCreators } from 'redux'

import playercolors from './playercolors'

import { LineChart, Line, Tooltip, CartesianGrid, ReferenceArea } from 'recharts'


class Scorebox extends PureComponent {

  constructor (props) {
    super(props)
    this.chartClicked = this.chartClicked.bind(this)
    this.state = {
      chart: []
    }
  }

  chartClicked (ev) {
    this.props.actions.setFrame(ev.activeLabel)
  }

  componentWillReceiveProps (nextProps) {
    if (this.props.game !== nextProps.game) {
      this.setState({chart: this.makeChart(nextProps.game)})
    }
  }

  makeChart (g) {
    
    const players = g.scores.map(p => p.punter)

    let out = [{step: 0}]
    g.moves.forEach((m, idx) => { out.push({step: idx + 1}) })

    players.forEach( player_id => {
      let score = 0
      out[0][`p${player_id}`] = score
      g.moves.forEach((m, idx) => {
        if ( !! m.claim && m.claim.punter === player_id ) {
          score = m.claim.score
        }
        out[idx + 1][`p${player_id}`] = score
      })
    })
    return out


  }

  render () {

    const g = this.props.game

    if ( ! g.scores || ! g.scores.length) return null


    if (g.moves.length > 1200) return (
        <div className="scorebox">
        <p className="text-muted">Scorebox disabled, too many nodes</p>
        </div>
    )


    const players = g.scores.map(p => p.punter)

    const getStroke = (p) => {
      if (p === 0) return playercolors.p0
      if (p === 1) return playercolors.p1
      if (p === 2) return playercolors.p2
      return playercolors.other
    }

    return (
      <div className="scorebox">
      <LineChart onClick={this.chartClicked} width={1200} height={150} data={this.state.chart}>
        <ReferenceArea fillOpacity={0.2} x1={0} x2={this.props.frame} />
        <CartesianGrid stroke="#eee" />
      {
        players.map( (p,idx) => {
          let key = `p${p}`
          let stroke = getStroke(p)
          return (
            <Line isAnimationActive={false} key={idx} type="linear" dataKey={key} stroke={stroke} dot={false} activeDot={{d: 3}} />
          )
        })
      }
        <Tooltip />
      </LineChart>
      </div>
    )
  }



}

const mapStateToProps = (state, ownProps) => {
  return {
    game:  state.game,
    frame: state.frame,
  }
}

const mapDispatchToProps = function (dispatch) {
  return {
    actions: bindActionCreators({setFrame}, dispatch)
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(Scorebox)
