import React, { Component } from 'react'
import { connect } from 'react-redux'

class Players extends Component {

  render () {

    if ( ! this.props.game.scores || ! this.props.game.scores.length) return null;

    return (
      <div>
      <h2>Players</h2>
      <table className="table w0">
        <thead><tr>
          <th>ID</th>
          <th>Name</th>
          <th>Score</th>
        </tr></thead><tbody>
      {
        this.props.game.scores.map( g => {
          return (
            <tr><td>{ g.punter }</td><td className={`text-${g.punter}`}>{ g.name }</td><td>{ g.score }</td></tr>
          );
        })
      }
      </tbody></table>
      </div>
    )



  }


}


const mapStateToProps = (state, ownProps) => {
  return {
    game:  state.game,
  }
}


export default connect(mapStateToProps)(Players)
