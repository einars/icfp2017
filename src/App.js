import React, { Component } from 'react';
import './App.css';
import { connect } from 'react-redux'

import Visualizer from './Visualizer'
import Players from './Players'
import AvailableLogs from './AvailableLogs'

class App extends Component {

  constructor (props) {
    super(props);

    this.state = {
      frame: 0,
    }
    this.rangeChanged = this.rangeChanged.bind(this)
    
  }

  rangeChanged (ev) {
    this.setState({ frame: ev.target.value })
  }



  render () {
    return (
      <div className="container-fluid">
      <div className="col-md-9">
        <h2>puntlog visualizer</h2>
        <Visualizer frame={this.state.frame} />
        <div>
          <p>Frame {this.state.frame} of {this.props.game.moves.length + 1}</p>
          <input type="range" value={this.state.frame} min={0} max={this.props.game.moves.length + 1} onChange={this.rangeChanged} />
        </div>
      </div>
      <div className="col-md-3">
        <AvailableLogs />
        <Players />
      </div>

    </div>
    );
  }
}

const mapStateToProps = (state, ownProps) => {
  return {
    game:  state.game,
  }
}


export default connect(mapStateToProps)(App)
