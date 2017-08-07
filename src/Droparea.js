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
    r.onloadend = (file) => {
      const game = JSON.parse(file.target.result)
      console.log(game)
      this.props.actions.receiveGame(game)
    }
    r.readAsText(files[0])
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
