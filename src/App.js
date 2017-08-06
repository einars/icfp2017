import React, { Component } from 'react';
import './App.css';
import { connect } from 'react-redux'

import Visualizer from './Visualizer'
import Players from './Players'
import AvailableLogs from './AvailableLogs'
import Mapinfo from './Mapinfo'
import Scorebox from './Scorebox'

import { setFrame } from './actions'
import { bindActionCreators } from 'redux'

class App extends Component {

  constructor (props) {
    super(props);

    this.state = {
      is_autoplay: false,
    }
    this.rangeChanged = this.rangeChanged.bind(this)
    this.togglePlay = this.togglePlay.bind(this)
    this.tick = this.tick.bind(this)
    
  }

  rangeChanged (ev) {
    this.props.actions.setFrame( + ev.target.value )
    this.setState({ is_autoplay: false })
  }

  componentDidMount () {
    this.timer = setInterval(this.tick, 200);
  }

  componentWillUnmount () {
    clearInterval(this.timer)
  }

  tick () {
    if (this.state.is_autoplay) {
      if (this.props.frame === this.props.game.moves.length) {
        this.setState({ is_autoplay: false })
      } else {
        this.props.actions.setFrame(this.props.frame + 1)
      }
    }
  }

  togglePlay ()  {
    this.setState({ is_autoplay: ! this.state.is_autoplay })
  }

  render () {
    return (
      <div className="container-fluid">
      <div className="col-md-9" style={{width: 1220}}>
        <Visualizer />
        <div>
          <Scorebox />
          <div className="row">
            <div className="col-sm-1">
            <button className="w100 btn btn-default" onClick={this.togglePlay}>
              {this.state.is_autoplay ? 'Pause' : 'Play'}
              </button>
            </div>
            <div className="col-sm-11">
              <input className="form-control" type="range" value={this.props.frame} min={0} max={this.props.game.moves.length} onChange={this.rangeChanged} />
            </div>
          </div>
        </div>
      </div>
      <div className="col-md-3">
        <AvailableLogs />
        <Players />
        <Mapinfo />
      </div>

    </div>
    );
  }
}

const mapStateToProps = (state, ownProps) => {
  return {
    game:  state.game,
    frame: state.frame,
  }
}

const mapDispatchToProps = function (dispatch) {
  return {
    actions: bindActionCreators({setFrame}, dispatch)
  }
}



export default connect(mapStateToProps, mapDispatchToProps)(App)
