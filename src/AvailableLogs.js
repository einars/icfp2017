import React, { Component } from 'react'
import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import { requestGame } from './actions'

class AvailableLogs extends Component {

  render () {
    return (
      <div>
        <h2>Available logs</h2>
        <div className="scrollpane">
        <table className="table table-condensed"><tbody>
        {
          this.props.games.map( g => {
            let classname = "btn btn-xs"
            if (g.name === this.props.game.id) classname += " btn-success"; else classname += " btn-default"
            return (
              <tr>
                <td style={{verticalAlign: 'middle'}}>
                  <button key={g.name} className={classname} onClick={() => this.props.actions.requestGame(g.name)}>{ g.map }</button>
                </td>
                <td style={{verticalAlign: 'middle'}}>
                  {
                    g.scores.map( s => {
                      return (
                        <div>
                        <span key={s.punter} className="punter-fixed">{ s.name }</span> { s.score }<br />
                        </div>
                      )
                    })
                  }
                </td>
              </tr>
            )
          })
        }
        </tbody></table>
        </div>
      </div>
    )
  }
}

const mapStateToProps = (state, ownProps) => {
  return {
    games:  state.games,
    game:  state.game,
  }
}

const mapDispatchToProps = function (dispatch) {
  return {
    actions: bindActionCreators({requestGame}, dispatch)
  }
}



export default connect(mapStateToProps, mapDispatchToProps)(AvailableLogs)

