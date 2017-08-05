import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import { Provider } from 'react-redux'

import App from './App';

import { createStore, applyMiddleware, compose, combineReducers } from 'redux'
import thunkMiddleware from 'redux-thunk'

import { requestGames } from './actions'
import reducers from './reducers'

const store = createStore(reducers, applyMiddleware(thunkMiddleware))

store.dispatch( requestGames() )


ReactDOM.render(<Provider store={store}><App /></Provider>, document.getElementById('root'));
