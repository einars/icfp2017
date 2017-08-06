import React, { Component } from 'react';
import { connect } from 'react-redux'

class Mapinfo extends Component {

  render () {


    const g = this.props.game

    if ( ! g.map.sites || g.map.sites === 0) return null

    let move = this.props.game.moves[ this.props.frame - 1]
    if ( ! move) move = ''; else move = JSON.stringify(move)

    return (
      <div>
        Frame: {this.props.frame} of {this.props.game.moves.length + 1}<br />
        Move: {move}<br />
        Game: {g.id}<br />
        File: {g.file}
        Sites: {g.map.sites.length}<br/ >
        Rivers: {g.map.rivers.length} <br />
        Mines: {g.map.mines.length}<br />
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


export default connect(mapStateToProps)(Mapinfo)

