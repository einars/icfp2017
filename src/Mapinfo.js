import React, { Component } from 'react';
import { connect } from 'react-redux'

class Mapinfo extends Component {

  render () {


    const g = this.props.game

    if ( ! g.map.sites || g.map.sites == 0) return null

    let move = this.props.game.moves[ this.props.frame - 1]
    if ( ! move) move = ''; else move = JSON.stringify(move)

    return (
      <div>
        <p className="pull-right">{this.props.frame} of {this.props.game.moves.length + 1}</p>
        <p>{g.id} {g.file}, sites: {g.map.sites.length}, rivers: {g.map.rivers.length}, mines: {g.map.mines.length} {move}</p>
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

