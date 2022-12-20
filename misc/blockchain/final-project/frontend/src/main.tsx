import React from 'react'
import ReactDOM from 'react-dom'
import './App.css'
import App from './App'
import { MetamaskStateProvider } from "use-metamask";

import { createRoot } from 'react-dom/client';

const container = document.getElementById('root')
const root = createRoot(container!)
root.render(
  <MetamaskStateProvider>
    <React.StrictMode>
      <App />
    </React.StrictMode>
  </MetamaskStateProvider>
)
