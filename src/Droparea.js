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

  onDrop (ev) {
    ev.preventDefault()
    const files = ev.dataTransfer.files; // FileList
    const r = new FileReader()
    const req = files[0]
    r.onloadend = (file) => {
      const game = JSON.parse(file.target.result)
      game.id = req.name
      console.log(req)
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
