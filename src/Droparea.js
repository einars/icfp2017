import React, { PureComponent } from 'react';
import { connect } from 'react-redux'

import { receiveGame } from './actions'
import { bindActionCreators } from 'redux'

class Droparea extends PureComponent {

  constructor (props) {
    super(props)
    this.onDragOver = this.onDragOver.bind(this)
    this.onDrop = this.onDrop.bind(this)
  }

  onDragOver (ev) {
    ev.dataTransfer.dropEffect = 'copy'
    ev.preventDefault()
  }

  
  fixupCers (game) {
    let moves = []
    game.moves.forEach( m => {
      moves = moves.concat(m[1].slice(1))
    })
    moves.reverse()
    game.moves = moves
    game.map = {
      sites: game.map[0].slice(1),
      rivers: game.map[1].slice(1),
      mines: game.map[2].slice(1),
    }
  }

  onDrop (ev) {
    ev.preventDefault()
    const files = ev.dataTransfer.files; // FileList
    const r = new FileReader()
    const req = files[0]
    r.onloadend = (file) => {
      const game = JSON.parse(file.target.result)
      game.id = req.name
      console.log(game)
      if ( !! game.map[0]) {
        this.fixupCers(game)
      }
      this.props.actions.receiveGame(game)
    }
    r.readAsText(req)
  }

  render () {
    return (
      <div onDragOver={this.onDragOver} onDrop={this.onDrop} className="droparea js-droparea">drop your own json logs here</div>
    )
  }
}



const mapStateToProps = (state, ownProps) => {
  return {
  }
}

const mapDispatchToProps = function (dispatch) {
  return {
    actions: bindActionCreators({receiveGame}, dispatch)
  }
}



export default connect(mapStateToProps, mapDispatchToProps)(Droparea)
