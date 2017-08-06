import React, { PureComponent } from 'react';
import { connect } from 'react-redux'

class Mapinfo extends PureComponent {

  render () {


    const g = this.props.game

    if ( ! g.map.sites || g.map.sites === 0) return null

    let move = this.props.game.moves[ this.props.frame - 1]
    if ( ! move) move = ''; else move = JSON.stringify(move)

    return (
      <div>
        Game: {g.id}<br />
        File: {g.file}<br />
        Sites: {g.map.sites.length}<br/ >
        Rivers: {g.map.rivers.length} <br />
        Mines: {g.map.mines.length}<br />
        <br />
        Frame: {this.props.frame} of {this.props.game.moves.length}<br />
        Move: {move}<br />
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

