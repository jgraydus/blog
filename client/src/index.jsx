import React from 'react'
import { createRoot } from 'react-dom/client'
import { BrowserRouter } from 'react-router-dom'
import Modal from 'react-modal'
import App from './app'

const rootElement = document.getElementById('root');

const root = createRoot(rootElement);

root.render(
  <BrowserRouter>
    <App />
  </BrowserRouter>
);

Modal.setAppElement(rootElement)

// TODO disable this in production build
if (true) {
  const socket = new WebSocket('ws://localhost:8082');
  
  socket.addEventListener('message', (event) => {
      if (event.data === 'RELOAD') {
          location.reload();
      }
      return false;
  });
}

