import React, { PureComponent } from 'react';
import { connect } from 'react-redux'
import { setFrame } from './actions'
import { bindActionCreators } from 'redux'

import playercolors from './playercolors'

import { LineChart, Line, Tooltip, CartesianGrid, ReferenceArea, ReferenceLine, XAxis } from 'recharts'


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
    
    const thinnedList = (ls, target) => {
      let increase = target
      if (ls.length > target)
        increase = target * target / g.moves.length
      let out = []
      let r = 0
      ls.forEach( (e, idx) => {
        r += increase
        if (r >= target || idx === 0 || idx === (ls.length - 1)) {
         r = 0
         out.push(e)
        }
      })

      console.log(`Thinned list from ${ls.length} to ${out.length}`)
      console.log(`Increase: ${increase}`)

      return out

    }

    const players = g.scores.map(p => p.punter)


    let out = [{step: 0}]

    let r = 0
    let idx = 1
    players.forEach( player_id => {
      let score = 0
      let moves = g.moves.map( (m, idx) => {
        if ( !! m.claim && m.claim.punter === player_id ) {
          score = m.claim.score
        }
        return [idx, score]
      })

      let thinned_moves = thinnedList (moves, 100)

      thinned_moves.forEach((m, idx) => {
        if ( ! out[idx + 1]) {
          out[idx + 1] = {step: m[0] + 1}
        }
        if (out[idx + 1]['step'] !== m[0]) console.log('thinned data warning, there are bugs')
        out[idx + 1][`p${player_id}`] = m[1]
      })
    })
    return out


  }

  render () {

    const g = this.props.game

    if ( ! g.scores || ! g.scores.length) return null


    if (false && g.moves.length > 1200) return (
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
        <XAxis dataKey="step" type="number" />
        {
           // <ReferenceLine fillOpacity={0.2} x1={0} x2={this.props.frame} />
           <ReferenceArea fillOpacity={0.2} x1={0} x2={this.props.frame} /> 
        }
        <CartesianGrid stroke="#eee" />
      {
        players.map( (p,idx) => {
          let key = `p${p}`
          let stroke = getStroke(p)
          return (
            <Line isAnimationActive={false} key={idx} type="monotone" dataKey={key} stroke={stroke} dot={false} activeDot={{d: 3}} />
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
