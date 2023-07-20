import React from 'react'
import { createRoot } from 'react-dom/client'
import App from './app'

const rootElement = document.getElementById('root');

const root = createRoot(rootElement);

root.render(
  <App />
);

const socket = new WebSocket('ws://localhost:8082');

socket.addEventListener('message', (event) => {
    if (event.data === 'RELOAD') {
        location.reload();
    }
    return false;
});

