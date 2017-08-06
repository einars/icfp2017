import React, { Component } from 'react'
import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import { requestGame } from './actions'

class AvailableLogs extends Component {

  render () {
    return (
      <div>
        <h2>Available logs</h2>
        <div className="scrollpane panel panel-default">
        <div className="panel-body">
        {
          this.props.games.map( g => {
            let classname = "w100 btn-xs btn"
            if (g.name === this.props.game.id) classname += " btn-success"; else classname += " btn-default"
            return (
              <button key={g.name} className={classname} onClick={() => this.props.actions.requestGame(g.name)}>{ g.name }</button>
            )
          })
        }
      </div>
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

