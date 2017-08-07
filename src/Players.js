import React, { PureComponent } from 'react'
import { connect } from 'react-redux'

class Players extends PureComponent {

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
          const c = `text-${g.punter}`
          return (
            <tr key={g.punter}><td className={c}>{ g.punter }</td><td className={c}>{ g.name }</td><td className={c}>{ g.score }</td></tr>
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
